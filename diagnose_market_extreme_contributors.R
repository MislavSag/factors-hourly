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
  if (is.na(parsed) || parsed < 0L) default else parsed
}

normalize_market_symbol = function(x) {
  x = toupper(as.character(x))
  x = sub("\\.[0-9]+$", "", x)
  gsub(".", "-", x, fixed = TRUE)
}

time_minus_one_hour = function(x) {
  hour = as.integer(substr(x, 1, 2))
  minute_second = substr(x, 3, nchar(x))
  if (any(is.na(hour) | hour <= 0L)) {
    stop("Target bar_time must be HH:MM:SS and greater than 00:00:00.")
  }
  sprintf("%02d%s", hour - 1L, minute_second)
}

read_targets = function(file) {
  if (file.exists(file)) {
    x = fread(file, showProgress = FALSE)
    if (!all(c("trading_day", "bar_time") %in% names(x))) {
      stop(sprintf("%s must contain trading_day and bar_time.", file))
    }
    if ("residual_intraday" %in% names(x)) {
      x[, abs_residual_intraday := abs(residual_intraday)]
      setorder(x, -abs_residual_intraday)
      x[, abs_residual_intraday := NULL]
    }
    x = head(x, env_int("TARGET_TOP_N", 25L))
    keep_cols = intersect(
      c("trading_day", "bar_time", "aleti_mkt", "our_mcap_oc", "our_mcap_intraday", "residual_intraday"),
      names(x)
    )
    x = x[, ..keep_cols]
  } else {
    x = data.table(
      trading_day = c("2017-04-17", "1999-05-04", "2017-05-02", "2009-04-14", "2017-04-18"),
      bar_time = c("12:00:00", "13:00:00", "16:00:00", "11:00:00", "11:00:00")
    )
  }
  x[, trading_day := as.IDate(trading_day)]
  x[, target_bar_time := bar_time]
  x[, raw_bar_time := time_minus_one_hour(target_bar_time)]
  x[, target_key := paste(trading_day, target_bar_time)]
  unique(x, by = c("trading_day", "target_bar_time"))
}

PATH_PRICES = env_chr("PATH_PRICES", "prices_factors_hour")
PATH_MARKET_CAP = env_chr("PATH_MARKET_CAP", file.path("data", "eodhd_market_cap", "daily_market_cap.rds"))
PATH_TARGETS = env_chr("PATH_TARGETS", file.path("diagnostics", "market_gap", "top_residual_hours.csv"))
PATH_OUT = env_chr("PATH_OUT", file.path("factor_returns_market_diagnostics", "extreme_contributors.csv"))
PATH_SUMMARY_OUT = env_chr("PATH_SUMMARY_OUT", file.path(dirname(PATH_OUT), "extreme_contributors_summary.csv"))
MARKET_CAP_LAG_DAYS = env_int("SIMPLE_MARKET_CAP_LAG_DAYS", 1L)
TOP_CONTRIBUTORS = env_int("TOP_CONTRIBUTORS", 20L)

price_files = sort(list.files(PATH_PRICES, pattern = "\\.csv$", full.names = TRUE))
if (!length(price_files)) stop(sprintf("No CSV files found in PATH_PRICES: %s", PATH_PRICES))

targets = read_targets(PATH_TARGETS)
target_lookup = targets[, .(
  trading_day,
  bar_time = raw_bar_time,
  target_bar_time,
  target_key,
  aleti_mkt = if ("aleti_mkt" %in% names(targets)) aleti_mkt else NA_real_,
  our_mcap_oc_file = if ("our_mcap_oc" %in% names(targets)) our_mcap_oc else NA_real_,
  our_mcap_intraday_file = if ("our_mcap_intraday" %in% names(targets)) our_mcap_intraday else NA_real_,
  residual_intraday_file = if ("residual_intraday" %in% names(targets)) residual_intraday else NA_real_
)]
setkey(target_lookup, trading_day, bar_time)

market_cap = as.data.table(readRDS(PATH_MARKET_CAP))
if (!all(c("symbol", "date", "market_cap") %in% names(market_cap))) {
  stop("PATH_MARKET_CAP must contain symbol, date, and market_cap.")
}
market_cap[, market_symbol := normalize_market_symbol(symbol)]
market_cap[, trading_day := as.IDate(date) + MARKET_CAP_LAG_DAYS]
market_cap = market_cap[
  is.finite(market_cap) & market_cap > 0,
  .(market_symbol, trading_day, market_cap = as.numeric(market_cap))
]
market_cap = market_cap[, .SD[.N], by = .(market_symbol, trading_day)]
setkey(market_cap, market_symbol, trading_day)

selected = vector("list", length(price_files))
for (idx in seq_along(price_files)) {
  file = price_files[[idx]]
  dt = fread(
    file,
    select = c(
      "symbol", "date", "open", "close", "close_raw", "volume",
      "trading_day", "bar_time", "returns_oc", "returns_intraday", "returns_cc"
    ),
    showProgress = FALSE
  )
  dt[, trading_day := as.IDate(trading_day)]
  hit = target_lookup[dt, on = .(trading_day, bar_time), nomatch = 0L]
  if (nrow(hit)) {
    selected[[idx]] = hit[, source_file := basename(file)]
  }
  if (idx %% 250L == 0L) {
    message(sprintf("Scanned %d/%d files; matched rows so far: %d", idx, length(price_files), sum(lengths(selected))))
  }
}

rows = rbindlist(selected, use.names = TRUE, fill = TRUE)
if (!nrow(rows)) {
  stop("No raw hourly rows matched target hours.")
}

rows[, market_symbol := normalize_market_symbol(symbol)]
rows = market_cap[rows, on = .(market_symbol, trading_day)]
rows = rows[is.finite(market_cap) & market_cap > 0]

rows[, `:=`(
  returns_oc = as.numeric(returns_oc),
  returns_intraday = as.numeric(returns_intraday),
  returns_cc = as.numeric(returns_cc),
  open = as.numeric(open),
  close = as.numeric(close),
  close_raw = as.numeric(close_raw),
  volume = as.numeric(volume)
)]

rows[, `:=`(
  sum_market_cap = sum(market_cap, na.rm = TRUE),
  n_symbols = uniqueN(market_symbol)
), by = target_key]
rows[, weight := market_cap / sum_market_cap]
rows[, `:=`(
  contrib_oc = weight * returns_oc,
  contrib_intraday = weight * returns_intraday,
  contrib_cc = weight * returns_cc
)]

summary = rows[, .(
  n_symbols = uniqueN(market_symbol),
  sum_market_cap = sum(market_cap, na.rm = TRUE),
  market_oc_rebuilt = sum(contrib_oc, na.rm = TRUE),
  market_intraday_rebuilt = sum(contrib_intraday, na.rm = TRUE),
  market_cc_rebuilt = sum(contrib_cc, na.rm = TRUE),
  aleti_mkt = first(aleti_mkt),
  our_mcap_oc_file = first(our_mcap_oc_file),
  our_mcap_intraday_file = first(our_mcap_intraday_file),
  residual_intraday_file = first(residual_intraday_file),
  max_abs_return_intraday = max(abs(returns_intraday), na.rm = TRUE),
  max_abs_contrib_intraday = max(abs(contrib_intraday), na.rm = TRUE),
  top1_weight = max(weight, na.rm = TRUE),
  top5_weight = sum(head(sort(weight, decreasing = TRUE), 5), na.rm = TRUE)
), by = .(trading_day, target_bar_time, target_key)]
summary[, abs_residual_intraday_file := abs(residual_intraday_file)]
setorder(summary, -abs_residual_intraday_file)
summary[, abs_residual_intraday_file := NULL]

contributors = rows[
  ,
  .SD[order(-abs(contrib_intraday))][seq_len(min(.N, TOP_CONTRIBUTORS))],
  by = .(trading_day, target_bar_time, target_key)
][
  ,
  .(
    trading_day,
    target_bar_time,
    symbol,
    market_symbol,
    source_file,
    raw_bar_time = bar_time,
    open,
    close,
    close_raw,
    volume,
    market_cap,
    weight,
    returns_oc,
    returns_intraday,
    returns_cc,
    contrib_oc,
    contrib_intraday,
    contrib_cc,
    aleti_mkt,
    our_mcap_intraday_file,
    residual_intraday_file
  )
]
contributors[, abs_contrib_intraday := abs(contrib_intraday)]
setorder(contributors, trading_day, target_bar_time, -abs_contrib_intraday)
contributors[, abs_contrib_intraday := NULL]

dir.create(dirname(PATH_OUT), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(PATH_SUMMARY_OUT), recursive = TRUE, showWarnings = FALSE)
fwrite(summary, PATH_SUMMARY_OUT)
fwrite(contributors, PATH_OUT)

cat("Wrote summary:", normalizePath(PATH_SUMMARY_OUT, winslash = "/", mustWork = FALSE), "\n")
cat("Wrote contributors:", normalizePath(PATH_OUT, winslash = "/", mustWork = FALSE), "\n")
print(summary)
print(contributors[, .SD[1:min(.N, 5L)], by = .(trading_day, target_bar_time)])
