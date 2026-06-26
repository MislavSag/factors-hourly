suppressPackageStartupMessages({
  library(data.table)
})

env_chr = function(name, default) {
  value = Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

env_int = function(name, default) {
  value = env_chr(name, "")
  if (!nzchar(value)) return(default)
  parsed = suppressWarnings(as.integer(value))
  if (is.na(parsed) || parsed < 0L) {
    stop(sprintf("%s must be a non-negative integer.", name))
  }
  parsed
}

env_num = function(name, default) {
  value = env_chr(name, "")
  if (!nzchar(value)) return(default)
  parsed = suppressWarnings(as.numeric(value))
  if (!is.finite(parsed)) stop(sprintf("%s must be numeric.", name))
  parsed
}

env_bool = function(name, default) {
  value = env_chr(name, if (default) "1" else "0")
  value %in% c("1", "true", "TRUE", "yes", "YES")
}

parse_feature_list = function(value) {
  trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
}

safe_feature_name = function(x) {
  gsub("[^A-Za-z0-9_.-]+", "_", x)
}

normalize_market_symbol = function(x) {
  x = toupper(as.character(x))
  x = sub("\\.[0-9]+$", "", x)
  gsub(".", "-", x, fixed = TRUE)
}

read_optional_table = function(file) {
  if (grepl("\\.rds$", file, ignore.case = TRUE)) {
    as.data.table(readRDS(file))
  } else if (grepl("\\.parquet$", file, ignore.case = TRUE)) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Reading parquet PATH_MARKET_CAP requires the arrow package.")
    }
    as.data.table(arrow::read_parquet(file))
  } else {
    fread(file, showProgress = FALSE)
  }
}

weighted_mean = function(x, w) {
  ok = is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

empty_factor_row = function() {
  list(
    n = 0L,
    n_long = 0L,
    n_short = 0L,
    long_ret = NA_real_,
    short_ret = NA_real_,
    factor_ret = NA_real_,
    signal_q_low = NA_real_,
    signal_q_high = NA_real_
  )
}

collapse_market_parts = function(parts) {
  x = rbindlist(parts, use.names = TRUE, fill = TRUE)
  x[, .(
    n = sum(n, na.rm = TRUE),
    sum_w = sum(sum_w, na.rm = TRUE),
    sum_wr = sum(sum_wr, na.rm = TRUE)
  ), by = .(date, trading_day, bar_time)]
}

collapse_market_daily_parts = function(parts) {
  x = rbindlist(parts, use.names = TRUE, fill = TRUE)
  x[, .(
    sum_ret0 = sum(sum_ret0, na.rm = TRUE),
    n = sum(n, na.rm = TRUE)
  ), by = trading_day]
}

feature_needs_calendar = function(feature) {
  feature != "MKT_MARKET_CAP_W"
}

feature_needs_market_daily = function(feature) {
  grepl("^ivol_capm_", feature) || grepl("^beta_tailrisk_proxy_", feature)
}

feature_window = function(feature, prefix, suffix = "d") {
  pattern = sprintf("^%s([0-9]+)%s$", prefix, suffix)
  if (!grepl(pattern, feature)) return(NA_integer_)
  as.integer(sub(pattern, "\\1", feature))
}

PATH_PRICES = env_chr("PATH_PRICES", "prices_factors_hour")
PATH_FACTORS = env_chr("PATH_FACTORS", "factor_returns_daily_findata_sane_retcap20")
PATH_MARKET_CAP = env_chr(
  "PATH_MARKET_CAP",
  file.path("data", "findata_market_cap", "daily_market_cap_clean_sane_hourly.rds")
)
FEATURES = parse_feature_list(env_chr(
  "DAILY_FEATURES",
  paste(
    c(
      "MKT_MARKET_CAP_W",
      "zero_trades_21d",
      "zero_trades_126d",
      "zero_trades_252d",
      "turnover_proxy_126d",
      "turnover_var_proxy_126d",
      "ivol_capm_252d",
      "beta_tailrisk_proxy_252d",
      "rvol_21d"
    ),
    collapse = ","
  )
))

array_index = env_int("PBS_ARRAY_INDEX", 0L)
DAILY_FEATURE = env_chr("DAILY_FEATURE", "")
if (!nzchar(DAILY_FEATURE)) {
  if (array_index < 1L || array_index > length(FEATURES)) {
    stop(sprintf(
      "Set DAILY_FEATURE or PBS_ARRAY_INDEX in 1..%d. PBS_ARRAY_INDEX=%d",
      length(FEATURES),
      array_index
    ))
  }
  DAILY_FEATURE = FEATURES[[array_index]]
}
if (!DAILY_FEATURE %in% FEATURES) {
  FEATURES = c(FEATURES, DAILY_FEATURE)
}

RETURN_COL = env_chr("SIMPLE_RETURN_COL", "returns_oc")
DROP_FIRST_BAR = env_bool("SIMPLE_DROP_FIRST_BAR", TRUE)
MARKET_CAP_LAG_DAYS = env_int("SIMPLE_MARKET_CAP_LAG_DAYS", 1L)
TAIL_PROB = env_num("SIMPLE_TAIL_PROB", 0.10)
MIN_LEG_N = env_int("SIMPLE_MIN_LEG_N", 10L)
MAX_NA_FRAC = env_num("SIMPLE_MAX_NA_FRAC", 0.80)
MAX_ABS_RETURN = env_num("SIMPLE_MAX_ABS_RETURN", 0.20)
MIN_SYMBOL_ROWS = env_int("SIMPLE_MIN_SYMBOL_ROWS", 50L)
MAX_FILES = env_int("SIMPLE_MAX_FILES", 0L)
MARKET_TAIL_Z = env_num("SIMPLE_MARKET_TAIL_Z", -1.0)
COLLAPSE_EVERY = env_int("STREAM_COLLAPSE_EVERY", 25L)
FORCE = env_bool("FORCE", FALSE)

if (TAIL_PROB <= 0 || TAIL_PROB >= 0.5) {
  stop("SIMPLE_TAIL_PROB must be in (0, 0.5).")
}
if (MAX_NA_FRAC < 0 || MAX_NA_FRAC >= 1) {
  stop("SIMPLE_MAX_NA_FRAC must be in [0, 1).")
}
if (!file.exists(PATH_MARKET_CAP)) {
  stop(sprintf("PATH_MARKET_CAP does not exist: %s", PATH_MARKET_CAP))
}

parts_dir = file.path(PATH_FACTORS, "parts")
dir.create(parts_dir, recursive = TRUE, showWarnings = FALSE)
part_id = if (array_index > 0L) sprintf("%04d", array_index) else safe_feature_name(DAILY_FEATURE)
out_file = file.path(parts_dir, sprintf("factor_returns_part_%s_%s.csv", part_id, safe_feature_name(DAILY_FEATURE)))
if (file.exists(out_file) && !FORCE) {
  cat(sprintf("Skipping existing part: %s\n", out_file))
  quit(save = "no", status = 0L, runLast = FALSE)
}

price_files = sort(list.files(PATH_PRICES, pattern = "\\.csv$", full.names = TRUE))
if (!length(price_files)) {
  stop(sprintf("No CSV files found in PATH_PRICES: %s", PATH_PRICES))
}
if (MAX_FILES > 0L) {
  price_files = head(price_files, MAX_FILES)
}

read_header = function(file) names(fread(file, nrows = 0L, showProgress = FALSE))

read_symbol_file = function(file) {
  header = read_header(file)
  required = c("symbol", "date", "trading_day", "bar_time", "volume", RETURN_COL)
  missing = setdiff(required, header)
  if (length(missing)) {
    warning(sprintf("Skipping %s; missing columns: %s", file, paste(missing, collapse = ", ")))
    return(NULL)
  }

  cols = intersect(
    c("symbol", "date", "trading_day", "bar_time", "is_first_bar", "close", "volume", RETURN_COL),
    header
  )
  dt = fread(file, select = cols, showProgress = FALSE)
  if (nrow(dt) < MIN_SYMBOL_ROWS) return(NULL)

  dt[, symbol := as.character(symbol)]
  dt[, date := as.POSIXct(date, tz = "America/New_York")]
  dt[, trading_day := as.IDate(trading_day)]
  dt[, bar_time := as.character(bar_time)]
  if (!"is_first_bar" %in% names(dt)) dt[, is_first_bar := FALSE]
  if (!"close" %in% names(dt)) dt[, close := NA_real_]
  if (DROP_FIRST_BAR) dt = dt[is.na(is_first_bar) | is_first_bar == FALSE]
  if (!nrow(dt)) return(NULL)

  dt[, ret := as.numeric(get(RETURN_COL))]
  dt[!is.finite(ret), ret := NA_real_]
  if (is.finite(MAX_ABS_RETURN)) {
    dt[abs(ret) > MAX_ABS_RETURN, ret := NA_real_]
  }
  dt[, close := as.numeric(close)]
  dt[, volume := as.numeric(volume)]
  dt[!is.finite(volume) | volume < 0, volume := NA_real_]
  dt[, dollar_vol := close * volume]
  dt[!is.finite(dollar_vol) | dollar_vol <= 0, dollar_vol := NA_real_]
  dt[, .(symbol, date, trading_day, bar_time, ret, volume, dollar_vol)]
}

build_daily_base = function(hourly_dt, all_days) {
  daily = hourly_dt[, .(
    daily_ret = if (all(!is.finite(ret))) NA_real_ else prod(1 + ret[is.finite(ret)]) - 1,
    daily_volume = sum(volume, na.rm = TRUE),
    daily_dollar_vol = sum(dollar_vol, na.rm = TRUE),
    n_bars = sum(is.finite(ret) | is.finite(volume))
  ), by = .(symbol, trading_day)]

  symbol_ranges = daily[, .(start_day = min(trading_day), end_day = max(trading_day)), by = symbol]
  calendar = symbol_ranges[
    ,
    .(trading_day = all_days[all_days >= start_day & all_days <= end_day]),
    by = symbol
  ]
  daily = daily[calendar, on = .(symbol, trading_day)]
  setorder(daily, symbol, trading_day)

  daily[is.na(n_bars), n_bars := 0L]
  daily[is.na(daily_volume), daily_volume := 0]
  daily[is.na(daily_dollar_vol), daily_dollar_vol := 0]
  daily[, zero_trade_day := as.integer(n_bars == 0L | daily_volume <= 0)]
  daily[, daily_ret0 := fifelse(is.finite(daily_ret), daily_ret, 0)]
  daily
}

add_feature = function(daily, feature, market_daily = NULL, market_tail = NULL) {
  if (grepl("^zero_trades_", feature)) {
    window = feature_window(feature, "zero_trades_")
    daily[, signal := shift(frollsum(zero_trade_day, n = window, align = "right", fill = NA_real_), 1L), by = symbol]
  } else if (feature == "turnover_proxy_126d") {
    daily[, log_dollar_vol := log1p(pmax(daily_dollar_vol, 0))]
    daily[, signal := shift(frollmean(log_dollar_vol, n = 126L, align = "right", fill = NA_real_), 1L), by = symbol]
  } else if (feature == "turnover_var_proxy_126d") {
    daily[, log_dollar_vol := log1p(pmax(daily_dollar_vol, 0))]
    daily[, signal := shift(frollsd(log_dollar_vol, n = 126L, align = "right", fill = NA_real_)^2, 1L), by = symbol]
  } else if (grepl("^rvol_", feature)) {
    window = feature_window(feature, "rvol_")
    daily[, signal := shift(frollsd(daily_ret0, n = window, align = "right", fill = NA_real_), 1L), by = symbol]
  } else if (grepl("^ivol_capm_", feature)) {
    window = feature_window(feature, "ivol_capm_")
    daily = market_daily[daily, on = "trading_day"]
    setorder(daily, symbol, trading_day)
    daily[, mean_r := frollmean(daily_ret0, n = window, align = "right", fill = NA_real_), by = symbol]
    daily[, mean_m := frollmean(mkt_daily_ret, n = window, align = "right", fill = NA_real_), by = symbol]
    daily[, mean_rm := frollmean(daily_ret0 * mkt_daily_ret, n = window, align = "right", fill = NA_real_), by = symbol]
    daily[, mean_m2 := frollmean(mkt_daily_ret^2, n = window, align = "right", fill = NA_real_), by = symbol]
    daily[, var_m := mean_m2 - mean_m^2]
    daily[, beta_capm := (mean_rm - mean_r * mean_m) / fifelse(abs(var_m) > 1e-12, var_m, NA_real_)]
    daily[, resid_capm := daily_ret0 - beta_capm * mkt_daily_ret]
    daily[, signal := shift(frollsd(resid_capm, n = window, align = "right", fill = NA_real_), 1L), by = symbol]
  } else if (grepl("^beta_tailrisk_proxy_", feature)) {
    window = feature_window(feature, "beta_tailrisk_proxy_")
    daily = market_daily[daily, on = "trading_day"]
    daily = market_tail[, .(trading_day, market_tail_day)][daily, on = "trading_day"]
    setorder(daily, symbol, trading_day)
    daily[, tail := as.numeric(market_tail_day)]
    daily[, tail_n := frollsum(tail, n = window, align = "right", fill = NA_real_), by = symbol]
    daily[, tail_sum_r := frollsum(tail * daily_ret0, n = window, align = "right", fill = NA_real_), by = symbol]
    daily[, tail_sum_m := frollsum(tail * mkt_daily_ret, n = window, align = "right", fill = NA_real_), by = symbol]
    daily[, tail_sum_rm := frollsum(tail * daily_ret0 * mkt_daily_ret, n = window, align = "right", fill = NA_real_), by = symbol]
    daily[, tail_sum_m2 := frollsum(tail * mkt_daily_ret^2, n = window, align = "right", fill = NA_real_), by = symbol]
    daily[, tail_mean_r := tail_sum_r / tail_n]
    daily[, tail_mean_m := tail_sum_m / tail_n]
    daily[, tail_cov := tail_sum_rm / tail_n - tail_mean_r * tail_mean_m]
    daily[, tail_var_m := tail_sum_m2 / tail_n - tail_mean_m^2]
    daily[, signal := shift(tail_cov / fifelse(abs(tail_var_m) > 1e-12, tail_var_m, NA_real_), 1L), by = symbol]
  } else {
    stop(sprintf("Unsupported DAILY_FEATURE: %s", feature))
  }

  daily[, .(symbol, trading_day, signal)]
}

load_market_cap = function() {
  market_cap = read_optional_table(PATH_MARKET_CAP)
  date_col = intersect(c("date", "trading_day", "market_cap_date"), names(market_cap))[1L]
  if (!"symbol" %in% names(market_cap) || is.na(date_col) || !"market_cap" %in% names(market_cap)) {
    stop("PATH_MARKET_CAP must contain symbol, date/trading_day, and market_cap columns.")
  }
  market_cap[, market_symbol := normalize_market_symbol(symbol)]
  market_cap[, trading_day := as.IDate(get(date_col)) + MARKET_CAP_LAG_DAYS]
  market_cap[, market_cap := as.numeric(market_cap)]
  market_cap = market_cap[
    nzchar(market_symbol) & !is.na(trading_day) & is.finite(market_cap) & market_cap > 0,
    .(market_symbol, trading_day, market_cap)
  ]
  setorder(market_cap, market_symbol, trading_day)
  market_cap = market_cap[, .SD[.N], by = .(market_symbol, trading_day)]
  setkey(market_cap, market_symbol, trading_day)
  market_cap
}

join_market_cap = function(dt, market_cap) {
  dt[, market_symbol := normalize_market_symbol(symbol)]
  setkey(dt, market_symbol, trading_day)
  out = market_cap[dt, roll = Inf]
  out[is.finite(ret) & is.finite(market_cap) & market_cap > 0]
}

collect_all_days = function() {
  cat(sprintf("Scanning trading days from %d price files\n", length(price_files)))
  all_days = as.IDate(character())
  for (i in seq_along(price_files)) {
    header = read_header(price_files[[i]])
    if ("trading_day" %in% header) {
      days = fread(price_files[[i]], select = "trading_day", showProgress = FALSE)[
        ,
        unique(as.IDate(trading_day))
      ]
      all_days = unique(c(all_days, days[!is.na(days)]))
    }
    if (i %% 250L == 0L) cat(sprintf("Scanned calendar %d/%d files\n", i, length(price_files)))
  }
  sort(all_days)
}

compute_market_daily = function(all_days) {
  cat("Precomputing equal-weighted daily market returns for CAPM/tail features\n")
  accum = NULL
  for (i in seq_along(price_files)) {
    dt = read_symbol_file(price_files[[i]])
    if (is.null(dt) || !nrow(dt)) next
    daily = build_daily_base(dt, all_days)
    part = daily[, .(sum_ret0 = sum(daily_ret0, na.rm = TRUE), n = .N), by = trading_day]
    accum = if (is.null(accum)) part else rbindlist(list(accum, part), use.names = TRUE, fill = TRUE)
    if (i %% COLLAPSE_EVERY == 0L) accum = collapse_market_daily_parts(list(accum))
    if (i %% 250L == 0L) cat(sprintf("Precomputed market daily %d/%d files\n", i, length(price_files)))
  }
  if (is.null(accum) || !nrow(accum)) stop("No rows while precomputing market_daily.")
  accum = collapse_market_daily_parts(list(accum))
  accum[, mkt_daily_ret := sum_ret0 / n]
  setorder(accum, trading_day)
  accum[, .(trading_day, mkt_daily_ret)]
}

compute_market_tail = function(market_daily) {
  market_tail = copy(market_daily)
  setorder(market_tail, trading_day)
  market_tail[, mkt_mean := shift(frollmean(mkt_daily_ret, n = 252L, align = "right", fill = NA_real_), 1L)]
  market_tail[, mkt_sd := shift(frollsd(mkt_daily_ret, n = 252L, align = "right", fill = NA_real_), 1L)]
  market_tail[, market_tail_day := as.integer((mkt_daily_ret - mkt_mean) / mkt_sd <= MARKET_TAIL_Z)]
  market_tail[is.na(market_tail_day), market_tail_day := 0L]
  market_tail[, .(trading_day, market_tail_day)]
}

compute_market_feature = function(market_cap) {
  cat("Computing MKT_MARKET_CAP_W\n")
  accum = NULL
  for (i in seq_along(price_files)) {
    dt = read_symbol_file(price_files[[i]])
    if (is.null(dt) || !nrow(dt)) next
    dt = join_market_cap(dt, market_cap)
    if (!nrow(dt)) next
    part = dt[, .(
      n = .N,
      sum_w = sum(market_cap, na.rm = TRUE),
      sum_wr = sum(ret * market_cap, na.rm = TRUE)
    ), by = .(date, trading_day, bar_time)]
    accum = if (is.null(accum)) part else rbindlist(list(accum, part), use.names = TRUE, fill = TRUE)
    if (i %% COLLAPSE_EVERY == 0L) accum = collapse_market_parts(list(accum))
    if (i %% 100L == 0L) cat(sprintf("Processed market %d/%d price files\n", i, length(price_files)))
  }
  if (is.null(accum) || !nrow(accum)) stop("No market rows were produced.")
  out = collapse_market_parts(list(accum))
  out[, `:=`(
    feature = "MKT_MARKET_CAP_W",
    factor_ret = sum_wr / sum_w,
    n_long = NA_integer_,
    n_short = NA_integer_,
    long_ret = NA_real_,
    short_ret = NA_real_,
    signal_q_low = NA_real_,
    signal_q_high = NA_real_,
    is_first_bar = FALSE
  )]
  out
}

compute_signal_feature = function(feature, market_cap, all_days, market_daily = NULL, market_tail = NULL) {
  cat(sprintf("Computing %s\n", feature))
  signal_parts = vector("list", length(price_files))
  for (i in seq_along(price_files)) {
    dt = read_symbol_file(price_files[[i]])
    if (is.null(dt) || !nrow(dt)) next

    daily = build_daily_base(dt, all_days)
    daily_feature = add_feature(daily, feature, market_daily = market_daily, market_tail = market_tail)
    dt = daily_feature[dt, on = .(symbol, trading_day)]
    dt = join_market_cap(dt, market_cap)
    dt = dt[
      is.finite(signal) & is.finite(ret) & is.finite(market_cap) & market_cap > 0,
      .(date, trading_day, bar_time, ret, market_cap, signal)
    ]
    signal_parts[[i]] = dt

    if (i %% 100L == 0L) {
      cat(sprintf(
        "Processed %s %d/%d price files; signal rows so far approx=%d\n",
        feature,
        i,
        length(price_files),
        sum(vapply(signal_parts, function(x) if (is.null(x)) 0L else nrow(x), integer(1L)))
      ))
    }
  }

  rows = rbindlist(signal_parts, use.names = TRUE, fill = TRUE)
  rm(signal_parts)
  invisible(gc())
  if (!nrow(rows)) stop(sprintf("No signal rows were produced for %s.", feature))

  out = rows[, {
    ok = is.finite(signal) & is.finite(ret) & is.finite(market_cap) & market_cap > 0
    n_ok = sum(ok)
    if (n_ok < 2L * MIN_LEG_N || mean(!ok) > MAX_NA_FRAC) {
      empty_factor_row()
    } else {
      q_low = as.numeric(quantile(signal[ok], probs = TAIL_PROB, na.rm = TRUE, type = 7L))
      q_high = as.numeric(quantile(signal[ok], probs = 1 - TAIL_PROB, na.rm = TRUE, type = 7L))
      long = ok & signal >= q_high
      short = ok & signal <= q_low
      n_long = sum(long)
      n_short = sum(short)
      if (n_long < MIN_LEG_N || n_short < MIN_LEG_N || !is.finite(q_low) || !is.finite(q_high)) {
        empty_factor_row()
      } else {
        long_ret = weighted_mean(ret[long], market_cap[long])
        short_ret = weighted_mean(ret[short], market_cap[short])
        list(
          n = n_ok,
          n_long = n_long,
          n_short = n_short,
          long_ret = long_ret,
          short_ret = short_ret,
          factor_ret = long_ret - short_ret,
          signal_q_low = q_low,
          signal_q_high = q_high
        )
      }
    }
  }, by = .(date, trading_day, bar_time)]
  rm(rows)
  invisible(gc())
  out[, `:=`(feature = feature, is_first_bar = FALSE)]
  out
}

cat(sprintf(
  "Daily feature task: feature=%s output=%s return_col=%s drop_first=%s max_abs_return=%s\n",
  DAILY_FEATURE,
  out_file,
  RETURN_COL,
  DROP_FIRST_BAR,
  as.character(MAX_ABS_RETURN)
))

market_cap = load_market_cap()
cat(sprintf(
  "Using market-cap weights from %s rows=%d symbols=%d lag_days=%d\n",
  PATH_MARKET_CAP,
  nrow(market_cap),
  uniqueN(market_cap$market_symbol),
  MARKET_CAP_LAG_DAYS
))

all_days = NULL
market_daily = NULL
market_tail = NULL
if (feature_needs_calendar(DAILY_FEATURE)) {
  all_days = collect_all_days()
  if (!length(all_days)) stop("No trading days found.")
  if (feature_needs_market_daily(DAILY_FEATURE)) {
    market_daily = compute_market_daily(all_days)
    if (grepl("^beta_tailrisk_proxy_", DAILY_FEATURE)) {
      market_tail = compute_market_tail(market_daily)
    }
  }
}

factor_returns = if (DAILY_FEATURE == "MKT_MARKET_CAP_W") {
  compute_market_feature(market_cap)
} else {
  compute_signal_feature(
    DAILY_FEATURE,
    market_cap = market_cap,
    all_days = all_days,
    market_daily = market_daily,
    market_tail = market_tail
  )
}

setcolorder(factor_returns, c(
  "date",
  "trading_day",
  "bar_time",
  "is_first_bar",
  "feature",
  "factor_ret",
  setdiff(names(factor_returns), c("date", "trading_day", "bar_time", "is_first_bar", "feature", "factor_ret"))
))
setorder(factor_returns, feature, date)
fwrite(factor_returns, out_file)

summary_dt = factor_returns[, .(
  n_obs = sum(is.finite(factor_ret)),
  first_date = min(date[is.finite(factor_ret)], na.rm = TRUE),
  last_date = max(date[is.finite(factor_ret)], na.rm = TRUE),
  mean_ret = mean(factor_ret, na.rm = TRUE),
  sd_ret = sd(factor_ret, na.rm = TRUE),
  min_ret = min(factor_ret, na.rm = TRUE),
  max_ret = max(factor_ret, na.rm = TRUE),
  mean_n = mean(n, na.rm = TRUE),
  mean_n_long = mean(n_long, na.rm = TRUE),
  mean_n_short = mean(n_short, na.rm = TRUE)
), by = feature]
summary_file = sub("\\.csv$", "_summary.csv", out_file)
summary_file = file.path(parts_dir, sprintf("summary_%s.csv", safe_feature_name(DAILY_FEATURE)))
fwrite(summary_dt, summary_file)

manifest = data.table(
  feature = DAILY_FEATURE,
  return_col = RETURN_COL,
  weight_source = "market_cap",
  market_cap_file = PATH_MARKET_CAP,
  drop_first_bar = DROP_FIRST_BAR,
  max_abs_return = MAX_ABS_RETURN,
  tail_prob = TAIL_PROB,
  market_tail_z = MARKET_TAIL_Z,
  output_file = out_file
)
fwrite(manifest, file.path(parts_dir, sprintf("manifest_%s.csv", safe_feature_name(DAILY_FEATURE))))

cat(sprintf("Saved %s rows=%d\n", out_file, nrow(factor_returns)))
print(summary_dt)
quit(save = "no", status = 0L, runLast = FALSE)
