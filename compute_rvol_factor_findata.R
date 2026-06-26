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

PATH_PRICES = env_chr("PATH_PRICES", "prices_factors_hour")
PATH_MARKET_CAP = env_chr(
  "PATH_MARKET_CAP",
  file.path("data", "findata_market_cap", "daily_market_cap_clean_sane_hourly.rds")
)
PATH_FACTORS = env_chr("PATH_FACTORS", "factor_returns_rvol_findata_sane")
RETURN_COL = env_chr("SIMPLE_RETURN_COL", "returns_oc")
DROP_FIRST_BAR = env_bool("SIMPLE_DROP_FIRST_BAR", TRUE)
MARKET_CAP_LAG_DAYS = env_int("SIMPLE_MARKET_CAP_LAG_DAYS", 1L)
RVOL_WINDOW_DAYS = env_int("SIMPLE_RVOL_WINDOW_DAYS", 21L)
TAIL_PROB = env_num("SIMPLE_TAIL_PROB", 0.10)
MIN_LEG_N = env_int("SIMPLE_MIN_LEG_N", 10L)
MAX_NA_FRAC = env_num("SIMPLE_MAX_NA_FRAC", 0.80)
MIN_SYMBOL_ROWS = env_int("SIMPLE_MIN_SYMBOL_ROWS", 50L)
MAX_FILES = env_int("SIMPLE_MAX_FILES", 0L)
WRITE_SIGNAL_ROWS = env_bool("WRITE_SIGNAL_ROWS", FALSE)

if (TAIL_PROB <= 0 || TAIL_PROB >= 0.5) {
  stop("SIMPLE_TAIL_PROB must be in (0, 0.5).")
}
if (MAX_NA_FRAC < 0 || MAX_NA_FRAC >= 1) {
  stop("SIMPLE_MAX_NA_FRAC must be in [0, 1).")
}
if (!file.exists(PATH_MARKET_CAP)) {
  stop(sprintf("PATH_MARKET_CAP does not exist: %s", PATH_MARKET_CAP))
}

dir.create(PATH_FACTORS, recursive = TRUE, showWarnings = FALSE)

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
    c("symbol", "date", "trading_day", "bar_time", "is_first_bar", "volume", RETURN_COL),
    header
  )
  dt = fread(file, select = cols, showProgress = FALSE)
  if (nrow(dt) < MIN_SYMBOL_ROWS) return(NULL)

  dt[, symbol := as.character(symbol)]
  dt[, date := as.POSIXct(date, tz = "America/New_York")]
  dt[, trading_day := as.IDate(trading_day)]
  dt[, bar_time := as.character(bar_time)]
  if (!"is_first_bar" %in% names(dt)) dt[, is_first_bar := FALSE]
  if (DROP_FIRST_BAR) dt = dt[is.na(is_first_bar) | is_first_bar == FALSE]
  if (!nrow(dt)) return(NULL)

  dt[, ret := as.numeric(get(RETURN_COL))]
  dt[!is.finite(ret), ret := NA_real_]
  dt[, volume := as.numeric(volume)]
  dt[!is.finite(volume) | volume < 0, volume := NA_real_]
  dt[, .(symbol, date, trading_day, bar_time, ret, volume)]
}

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
all_days = sort(all_days)
if (!length(all_days)) stop("No trading days found.")

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

cat(sprintf(
  "Using market-cap weights from %s rows=%d symbols=%d lag_days=%d\n",
  PATH_MARKET_CAP,
  nrow(market_cap),
  uniqueN(market_cap$market_symbol),
  MARKET_CAP_LAG_DAYS
))

signal_parts = vector("list", length(price_files))
for (i in seq_along(price_files)) {
  dt = read_symbol_file(price_files[[i]])
  if (is.null(dt) || !nrow(dt)) next

  daily = dt[, .(
    daily_ret = if (all(!is.finite(ret))) NA_real_ else prod(1 + ret[is.finite(ret)]) - 1,
    daily_volume = sum(volume, na.rm = TRUE),
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
  daily[, daily_ret0 := fifelse(is.finite(daily_ret), daily_ret, 0)]
  daily[, rvol_21d := shift(
    frollsd(daily_ret0, n = RVOL_WINDOW_DAYS, align = "right", fill = NA_real_),
    1L
  ), by = symbol]

  dt = daily[, .(symbol, trading_day, rvol_21d)][dt, on = .(symbol, trading_day)]
  dt[, market_symbol := normalize_market_symbol(symbol)]
  setkey(dt, market_symbol, trading_day)
  dt = market_cap[dt, roll = Inf]
  dt = dt[
    is.finite(ret) & is.finite(rvol_21d) & is.finite(market_cap) & market_cap > 0,
    .(symbol, date, trading_day, bar_time, ret, market_cap, rvol_21d)
  ]
  signal_parts[[i]] = dt

  if (i %% 100L == 0L) {
    cat(sprintf(
      "Processed %d/%d price files; signal rows so far approx=%d\n",
      i,
      length(price_files),
      sum(vapply(signal_parts, nrow, integer(1L)))
    ))
  }
}

rows = rbindlist(signal_parts, use.names = TRUE, fill = TRUE)
rm(signal_parts)
if (!nrow(rows)) stop("No rvol signal rows were produced.")
setorder(rows, date, symbol)

factor_dt = rows[, {
  signal = rvol_21d
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
factor_dt[, feature := "rvol_21d"]

market_dt = rows[, .(
  n = sum(is.finite(ret) & is.finite(market_cap) & market_cap > 0),
  n_long = NA_integer_,
  n_short = NA_integer_,
  long_ret = NA_real_,
  short_ret = NA_real_,
  factor_ret = weighted_mean(ret, market_cap),
  signal_q_low = NA_real_,
  signal_q_high = NA_real_,
  feature = "MKT_MARKET_CAP_W"
), by = .(date, trading_day, bar_time)]

factor_returns = rbindlist(list(market_dt, factor_dt), use.names = TRUE, fill = TRUE)
setcolorder(factor_returns, c(
  "date",
  "trading_day",
  "bar_time",
  "feature",
  "factor_ret",
  setdiff(names(factor_returns), c("date", "trading_day", "bar_time", "feature", "factor_ret"))
))
setorder(factor_returns, feature, date)

summary_dt = factor_returns[, .(
  n_obs = sum(is.finite(factor_ret)),
  first_date = min(date[is.finite(factor_ret)], na.rm = TRUE),
  last_date = max(date[is.finite(factor_ret)], na.rm = TRUE),
  mean_ret = mean(factor_ret, na.rm = TRUE),
  sd_ret = sd(factor_ret, na.rm = TRUE),
  min_ret = min(factor_ret, na.rm = TRUE),
  max_ret = max(factor_ret, na.rm = TRUE),
  mean_n = mean(n, na.rm = TRUE)
), by = feature]

wide = dcast(
  factor_returns[, .(date, trading_day, bar_time, feature, factor_ret)],
  date + trading_day + bar_time ~ feature,
  value.var = "factor_ret"
)
setorder(wide, date)

manifest = data.table(
  feature = c("MKT_MARKET_CAP_W", "rvol_21d"),
  source = c("market_cap_weighted_market", "daily_rvol_21d_market_cap_weighted_decile_spread"),
  return_col = RETURN_COL,
  weight_source = "market_cap",
  market_cap_file = PATH_MARKET_CAP,
  drop_first_bar = DROP_FIRST_BAR,
  rvol_window_days = RVOL_WINDOW_DAYS,
  tail_prob = TAIL_PROB
)

fwrite(manifest, file.path(PATH_FACTORS, "factor_feature_manifest.csv"))
fwrite(summary_dt, file.path(PATH_FACTORS, "factor_returns_summary.csv"))
fwrite(wide, file.path(PATH_FACTORS, "factor_returns_wide.csv"))
fwrite(factor_returns, file.path(PATH_FACTORS, "factor_returns_long.csv"))
if (WRITE_SIGNAL_ROWS) {
  fwrite(rows, file.path(PATH_FACTORS, "rvol_signal_rows.csv"))
}

cat(sprintf(
  "Saved rvol factor test to %s rows=%d wide_rows=%d signal_rows=%d\n",
  PATH_FACTORS,
  nrow(factor_returns),
  nrow(wide),
  nrow(rows)
))
print(summary_dt)
