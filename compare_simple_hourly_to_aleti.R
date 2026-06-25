suppressPackageStartupMessages({
  library(data.table)
})

env_chr = function(name, default) {
  value = Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

env_int = function(name, default) {
  value = Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(default)
  parsed = suppressWarnings(as.integer(value))
  if (is.na(parsed)) default else parsed
}

PATH_SIMPLE_FACTORS = env_chr(
  "PATH_SIMPLE_FACTORS",
  file.path("factor_returns_simple", "factor_returns_wide.csv")
)
PATH_ALETI_MINUTE_DIR = env_chr(
  "PATH_ALETI_MINUTE_DIR",
  file.path("..", "intradayzoo", "data", "factor_returns")
)
PATH_ALETI_HOURLY_FACTORS = env_chr(
  "PATH_ALETI_HOURLY_FACTORS",
  file.path(dirname(PATH_SIMPLE_FACTORS), "aleti_hourly_from_minute.csv")
)
PATH_ALETI_FACTORS = env_chr(
  "PATH_ALETI_FACTORS",
  PATH_ALETI_HOURLY_FACTORS
)
PATH_COMPARE_OUT = env_chr(
  "PATH_COMPARE_OUT",
  file.path(dirname(PATH_SIMPLE_FACTORS), "aleti_comparison.csv")
)
PATH_COMPARE_PLOTS = env_chr(
  "PATH_COMPARE_PLOTS",
  file.path(dirname(PATH_COMPARE_OUT), "aleti_comparison_plots")
)
ALETI_FORCE_AGGREGATE = env_chr("ALETI_FORCE_AGGREGATE", "0") %in% c("1", "true", "TRUE", "yes", "YES")
ALETI_MAX_FILES = env_int("ALETI_MAX_FILES", 0L)

comparison_map = data.table(
  simple_feature = c(
    "MKT_EW",
    "MKT_DOLLAR_VOL_W",
    "zero_trades_21d",
    "zero_trades_126d",
    "zero_trades_252d",
    "turnover_proxy_126d",
    "turnover_var_proxy_126d",
    "ivol_capm_252d",
    "beta_tailrisk_proxy_252d",
    "rvol_21d"
  ),
  aleti_minute_feature = c(
    "ff__mkt",
    "ff__mkt",
    "jkp__zero_trades_21d",
    "jkp__zero_trades_126d",
    "jkp__zero_trades_252d",
    "jkp__turnover_126d",
    "jkp__turnover_var_126d",
    "jkp__ivol_capm_252d",
    "cz__betatailrisk",
    "jkp__rvol_21d"
  )
)
comparison_map[, aleti_feature := paste0(aleti_minute_feature, "_hour")]

compound_return = function(x) {
  x = as.numeric(x)
  x = x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  prod(1 + x) - 1
}

ceiling_hour = function(x) {
  as.POSIXct(
    ceiling(as.numeric(x) / 3600) * 3600,
    origin = "1970-01-01",
    tz = "America/New_York"
  )
}

aleti_cache_meta_file = function(out_file) {
  paste0(out_file, ".meta.csv")
}

aleti_cache_metadata_matches = function(out_file, minute_dir, minute_features, expected_file_count) {
  meta_file = aleti_cache_meta_file(out_file)
  if (!file.exists(meta_file)) return(FALSE)

  meta = tryCatch(
    fread(meta_file, showProgress = FALSE),
    error = function(e) NULL
  )
  if (is.null(meta) || !nrow(meta)) return(FALSE)

  expected_features = paste(sort(unique(minute_features)), collapse = "|")
  expected_dir = normalizePath(minute_dir, winslash = "/", mustWork = FALSE)
  all(c(
    "minute_dir",
    "minute_file_count",
    "minute_features"
  ) %in% names(meta)) &&
    identical(meta$minute_dir[[1]], expected_dir) &&
    identical(as.integer(meta$minute_file_count[[1]]), as.integer(expected_file_count)) &&
    identical(meta$minute_features[[1]], expected_features)
}

write_aleti_cache_metadata = function(out_file, minute_dir, minute_files, minute_features) {
  meta = data.table(
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    minute_dir = normalizePath(minute_dir, winslash = "/", mustWork = FALSE),
    minute_file_count = length(minute_files),
    first_minute_file = basename(minute_files[[1L]]),
    last_minute_file = basename(minute_files[[length(minute_files)]]),
    minute_features = paste(sort(unique(minute_features)), collapse = "|")
  )
  fwrite(meta, aleti_cache_meta_file(out_file))
}

aggregate_aleti_minutes_to_hourly = function(minute_dir, out_file, minute_features, max_files = 0L) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to aggregate raw Aleti minute parquet files.")
  }
  if (!requireNamespace("tidyselect", quietly = TRUE)) {
    stop("Package 'tidyselect' is required to select Aleti parquet columns.")
  }

  minute_files = sort(list.files(minute_dir, pattern = "\\.parquet$", full.names = TRUE))
  if (!length(minute_files)) {
    stop(sprintf("No Aleti minute parquet files found in %s", minute_dir))
  }
  if (max_files > 0L) {
    minute_files = head(minute_files, max_files)
  }

  needed_cols = unique(c("datetime", minute_features))
  hourly_parts = vector("list", length(minute_files))
  cat(sprintf("Aggregating %d Aleti minute parquet files to hourly returns\n", length(minute_files)))
  for (i in seq_along(minute_files)) {
    dt = as.data.table(arrow::read_parquet(
      minute_files[[i]],
      col_select = tidyselect::all_of(needed_cols)
    ))
    missing_cols = setdiff(needed_cols, names(dt))
    if (length(missing_cols)) {
      stop(sprintf(
        "Missing columns in %s: %s",
        minute_files[[i]],
        paste(missing_cols, collapse = ", ")
      ))
    }

    dt[, datetime := as.POSIXct(datetime, tz = "America/New_York")]
    dt[, minute_time := format(datetime, "%H:%M:%S", tz = "America/New_York")]
    dt = dt[minute_time != "09:30:00"]
    dt[, datetime_hour := ceiling_hour(datetime)]

    hourly_parts[[i]] = dt[, lapply(.SD, compound_return),
      by = .(datetime = datetime_hour),
      .SDcols = minute_features
    ]
    if (i %% 25L == 0L) {
      cat(sprintf("Aggregated %d/%d Aleti minute files\n", i, length(minute_files)))
    }
  }

  hourly = rbindlist(hourly_parts, use.names = TRUE, fill = TRUE)
  hourly = hourly[, lapply(.SD, compound_return), by = datetime, .SDcols = minute_features]
  setnames(hourly, minute_features, paste0(minute_features, "_hour"))
  setorder(hourly, datetime)
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  hourly_out = copy(hourly)
  hourly_out[, datetime := format(datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC")]
  fwrite(hourly_out, out_file)
  write_aleti_cache_metadata(out_file, minute_dir, minute_files, minute_features)
  cat(sprintf("Saved hourly Aleti factors from minute returns: %s rows=%d cols=%d\n", out_file, nrow(hourly), ncol(hourly)))
  out_file
}

if (dir.exists(PATH_ALETI_MINUTE_DIR)) {
  all_minute_files = sort(list.files(PATH_ALETI_MINUTE_DIR, pattern = "\\.parquet$", full.names = TRUE))
  expected_file_count = length(all_minute_files)
  if (ALETI_MAX_FILES > 0L) {
    expected_file_count = min(expected_file_count, ALETI_MAX_FILES)
  }
  needs_aggregation = TRUE
  if (file.exists(PATH_ALETI_HOURLY_FACTORS) && !ALETI_FORCE_AGGREGATE) {
    cached_header = names(fread(PATH_ALETI_HOURLY_FACTORS, nrows = 0L, showProgress = FALSE))
    header_matches = all(c("datetime", comparison_map$aleti_feature) %in% cached_header)
    metadata_matches = aleti_cache_metadata_matches(
      PATH_ALETI_HOURLY_FACTORS,
      PATH_ALETI_MINUTE_DIR,
      unique(comparison_map$aleti_minute_feature),
      expected_file_count
    )
    needs_aggregation = !(header_matches && metadata_matches)
  }
  if (needs_aggregation || ALETI_FORCE_AGGREGATE) {
    aggregate_aleti_minutes_to_hourly(
      PATH_ALETI_MINUTE_DIR,
      PATH_ALETI_HOURLY_FACTORS,
      unique(comparison_map$aleti_minute_feature),
      ALETI_MAX_FILES
    )
  }
  PATH_ALETI_FACTORS = PATH_ALETI_HOURLY_FACTORS
}

if (!file.exists(PATH_ALETI_FACTORS)) {
  stop(sprintf(
    "Aleti hourly factor file not found: %s. Set PATH_ALETI_MINUTE_DIR to raw parquet directory or PATH_ALETI_FACTORS to an hourly CSV.",
    PATH_ALETI_FACTORS
  ))
}

if (!file.exists(PATH_SIMPLE_FACTORS)) {
  stop(sprintf(
    "Simple factor file not found: %s. Aleti hourly cache is ready if aggregation completed.",
    PATH_SIMPLE_FACTORS
  ))
}

simple_header = names(fread(PATH_SIMPLE_FACTORS, nrows = 0L, showProgress = FALSE))
aleti_header = names(fread(PATH_ALETI_FACTORS, nrows = 0L, showProgress = FALSE))

comparison_map[, simple_available := simple_feature %in% simple_header]
comparison_map[, aleti_available := aleti_feature %in% aleti_header]
active_map = comparison_map[simple_available == TRUE & aleti_available == TRUE]
if (!nrow(active_map)) {
  fwrite(comparison_map, sub("\\.csv$", "_availability.csv", PATH_COMPARE_OUT))
  stop("No overlapping comparison features found. Wrote availability table.")
}

required_simple_cols = c("trading_day", "bar_time")
missing_required_simple_cols = setdiff(required_simple_cols, simple_header)
if (length(missing_required_simple_cols)) {
  stop(sprintf(
    "Simple factor file is missing required columns: %s",
    paste(missing_required_simple_cols, collapse = ", ")
  ))
}

simple_cols = unique(c(intersect("date", simple_header), required_simple_cols, active_map$simple_feature))
simple = fread(PATH_SIMPLE_FACTORS, select = simple_cols, showProgress = FALSE)
simple[, trading_day := as.IDate(trading_day)]
simple[, bar_time := as.character(bar_time)]
simple = simple[!is.na(trading_day) & !is.na(bar_time)]
duplicate_simple_keys = simple[, .N, by = .(trading_day, bar_time)][N > 1L]
if (nrow(duplicate_simple_keys)) {
  stop(sprintf(
    "Simple factor file has duplicate trading_day/bar_time keys. First duplicate: %s %s n=%d. Regenerate simple factors with the fixed timestamp grouping.",
    duplicate_simple_keys$trading_day[[1L]],
    duplicate_simple_keys$bar_time[[1L]],
    duplicate_simple_keys$N[[1L]]
  ))
}

aleti_cols = unique(c("datetime", active_map$aleti_feature))
aleti = fread(
  PATH_ALETI_FACTORS,
  select = aleti_cols,
  colClasses = c(datetime = "character"),
  showProgress = FALSE
)
aleti[, datetime := as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")]
aleti[, trading_day := as.IDate(datetime, tz = "America/New_York")]
aleti[, bar_time := format(datetime, "%H:%M:%S", tz = "America/New_York")]

simple_feature_cols = intersect(active_map$simple_feature, names(simple))
aleti_feature_cols = intersect(active_map$aleti_feature, names(aleti))
setnames(simple, simple_feature_cols, paste0("simple__", simple_feature_cols))
setnames(aleti, aleti_feature_cols, paste0("aleti__", aleti_feature_cols))

merged = merge(
  simple,
  aleti,
  by = c("trading_day", "bar_time"),
  all = FALSE,
  allow.cartesian = TRUE
)
if ("date" %in% names(merged)) {
  merged[, plot_datetime := as.POSIXct(date, tz = "America/New_York")]
} else {
  merged[, plot_datetime := as.POSIXct(
    paste(trading_day, bar_time),
    tz = "America/New_York"
  )]
}
if ((!nrow(merged) || all(is.na(merged$plot_datetime))) && "datetime" %in% names(merged)) {
  merged[, plot_datetime := as.POSIXct(datetime, tz = "America/New_York")]
}
setorder(merged, plot_datetime)

results = rbindlist(lapply(seq_len(nrow(active_map)), function(i) {
  simple_feature = active_map$simple_feature[[i]]
  aleti_feature = active_map$aleti_feature[[i]]
  simple_col = paste0("simple__", simple_feature)
  aleti_col = paste0("aleti__", aleti_feature)

  if (!all(c(simple_col, aleti_col) %in% names(merged))) {
    return(data.table(
      simple_feature = simple_feature,
      aleti_feature = aleti_feature,
      n = 0L,
      corr = NA_real_,
      beta_simple_on_aleti = NA_real_,
      mean_simple = NA_real_,
      mean_aleti = NA_real_,
      sd_simple = NA_real_,
      sd_aleti = NA_real_,
      first_day = as.IDate(NA),
      last_day = as.IDate(NA)
    ))
  }

  x = as.numeric(merged[[simple_col]])
  y = as.numeric(merged[[aleti_col]])
  ok = is.finite(x) & is.finite(y)
  if (sum(ok) < 10L) {
    beta = NA_real_
    corr = NA_real_
  } else {
    corr = suppressWarnings(cor(x[ok], y[ok]))
    beta = suppressWarnings(coef(lm(x[ok] ~ y[ok]))[[2L]])
  }

  data.table(
    simple_feature = simple_feature,
    aleti_feature = aleti_feature,
    n = sum(ok),
    corr = corr,
    beta_simple_on_aleti = beta,
    mean_simple = mean(x[ok], na.rm = TRUE),
    mean_aleti = mean(y[ok], na.rm = TRUE),
    sd_simple = sd(x[ok], na.rm = TRUE),
    sd_aleti = sd(y[ok], na.rm = TRUE),
    first_day = if (any(ok)) min(merged$trading_day[ok]) else as.IDate(NA),
    last_day = if (any(ok)) max(merged$trading_day[ok]) else as.IDate(NA)
  )
}), use.names = TRUE, fill = TRUE)

results[, abs_corr := abs(corr)]
setorder(results, -abs_corr, simple_feature)
results[, abs_corr := NULL]
dir.create(dirname(PATH_COMPARE_OUT), recursive = TRUE, showWarnings = FALSE)
fwrite(results, PATH_COMPARE_OUT)
fwrite(comparison_map, sub("\\.csv$", "_availability.csv", PATH_COMPARE_OUT))

dir.create(PATH_COMPARE_PLOTS, recursive = TRUE, showWarnings = FALSE)
cumulative_curves = rbindlist(lapply(seq_len(nrow(active_map)), function(i) {
  simple_feature = active_map$simple_feature[[i]]
  aleti_feature = active_map$aleti_feature[[i]]
  simple_col = paste0("simple__", simple_feature)
  aleti_col = paste0("aleti__", aleti_feature)

  if (!all(c(simple_col, aleti_col, "plot_datetime") %in% names(merged))) {
    return(NULL)
  }

  dt = merged[, .(
    datetime = plot_datetime,
    simple_ret = as.numeric(get(simple_col)),
    aleti_ret = as.numeric(get(aleti_col))
  )]
  dt = dt[is.finite(simple_ret) & is.finite(aleti_ret) & !is.na(datetime)]
  if (nrow(dt) < 10L) return(NULL)
  setorder(dt, datetime)

  dt[, simple_cumret := cumprod(1 + simple_ret) - 1]
  dt[, aleti_cumret := cumprod(1 + aleti_ret) - 1]
  dt[, simple_feature := simple_feature]
  dt[, aleti_feature := aleti_feature]

  plot_file = file.path(
    PATH_COMPARE_PLOTS,
    gsub(
      "[^A-Za-z0-9_.-]",
      "_",
      sprintf("%s_vs_%s.png", simple_feature, aleti_feature)
    )
  )

  if (capabilities("cairo")) {
    png(plot_file, width = 1400, height = 850, res = 140, type = "cairo")
  } else {
    png(plot_file, width = 1400, height = 850, res = 140)
  }
  y_range = range(c(dt$simple_cumret, dt$aleti_cumret), na.rm = TRUE)
  plot(
    dt$datetime,
    dt$simple_cumret,
    type = "l",
    col = "#1f77b4",
    lwd = 1.5,
    ylim = y_range,
    xlab = "Time",
    ylab = "Cumulative return",
    main = sprintf("%s vs %s", simple_feature, aleti_feature)
  )
  lines(dt$datetime, dt$aleti_cumret, col = "#d62728", lwd = 1.5)
  grid()
  legend(
    "topleft",
    legend = c("simple hourly", "Aleti"),
    col = c("#1f77b4", "#d62728"),
    lwd = 1.5,
    bty = "n"
  )
  dev.off()

  dt[, .(
    datetime,
    simple_feature,
    aleti_feature,
    simple_ret,
    aleti_ret,
    simple_cumret,
    aleti_cumret
  )]
}), use.names = TRUE, fill = TRUE)

curve_file = sub("\\.csv$", "_cumulative_returns.csv", PATH_COMPARE_OUT)
if (nrow(cumulative_curves)) {
  fwrite(cumulative_curves, curve_file)
}

cat(sprintf(
  "Saved Aleti comparison: %s rows=%d matched_timestamps=%d plots=%s\n",
  PATH_COMPARE_OUT,
  nrow(results),
  nrow(merged),
  PATH_COMPARE_PLOTS
))

quit(save = "no", status = 0L, runLast = FALSE)
