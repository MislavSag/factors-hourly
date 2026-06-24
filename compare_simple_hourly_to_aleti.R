suppressPackageStartupMessages({
  library(data.table)
})

env_chr = function(name, default) {
  value = Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

PATH_SIMPLE_FACTORS = env_chr(
  "PATH_SIMPLE_FACTORS",
  file.path("factor_returns_simple", "factor_returns_wide.csv")
)
PATH_ALETI_FACTORS = env_chr(
  "PATH_ALETI_FACTORS",
  file.path("..", "intradayzoo", "data", "factor_returns.csv")
)
PATH_COMPARE_OUT = env_chr(
  "PATH_COMPARE_OUT",
  file.path(dirname(PATH_SIMPLE_FACTORS), "aleti_comparison.csv")
)

if (!file.exists(PATH_SIMPLE_FACTORS)) {
  stop(sprintf("Simple factor file not found: %s", PATH_SIMPLE_FACTORS))
}
if (!file.exists(PATH_ALETI_FACTORS)) {
  stop(sprintf("Aleti factor file not found: %s", PATH_ALETI_FACTORS))
}

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
  aleti_feature = c(
    "ff__mkt_hour",
    "ff__mkt_hour",
    "jkp__zero_trades_21d_hour",
    "jkp__zero_trades_126d_hour",
    "jkp__zero_trades_252d_hour",
    "jkp__turnover_126d_hour",
    "jkp__turnover_var_126d_hour",
    "jkp__ivol_capm_252d_hour",
    "cz__betatailrisk_hour",
    "jkp__rvol_21d_hour"
  )
)

simple_header = names(fread(PATH_SIMPLE_FACTORS, nrows = 0L, showProgress = FALSE))
aleti_header = names(fread(PATH_ALETI_FACTORS, nrows = 0L, showProgress = FALSE))

comparison_map[, simple_available := simple_feature %in% simple_header]
comparison_map[, aleti_available := aleti_feature %in% aleti_header]
active_map = comparison_map[simple_available == TRUE & aleti_available == TRUE]
if (!nrow(active_map)) {
  fwrite(comparison_map, sub("\\.csv$", "_availability.csv", PATH_COMPARE_OUT))
  stop("No overlapping comparison features found. Wrote availability table.")
}

simple_cols = unique(c("date", "trading_day", "bar_time", active_map$simple_feature))
simple = fread(PATH_SIMPLE_FACTORS, select = simple_cols, showProgress = FALSE)
simple[, trading_day := as.IDate(trading_day)]
simple[, bar_time := as.character(bar_time)]
simple = simple[!is.na(trading_day) & !is.na(bar_time)]

aleti_cols = unique(c("datetime", active_map$aleti_feature))
aleti = fread(PATH_ALETI_FACTORS, select = aleti_cols, showProgress = FALSE)
aleti[, datetime := as.POSIXct(datetime, tz = "America/New_York")]
aleti[, trading_day := as.IDate(datetime, tz = "America/New_York")]
aleti[, bar_time := format(datetime, "%H:%M:%S", tz = "America/New_York")]
aleti = aleti[grepl(":00:00$", bar_time)]

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

cat(sprintf(
  "Saved Aleti comparison: %s rows=%d matched_timestamps=%d\n",
  PATH_COMPARE_OUT,
  nrow(results),
  nrow(merged)
))

quit(save = "no", status = 0L, runLast = FALSE)
