suppressPackageStartupMessages({
  library(data.table)
})

env_chr = function(name, default) {
  value = Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

normalize_market_symbol = function(x) {
  x = toupper(as.character(x))
  x = sub("\\.[0-9]+$", "", x)
  gsub(".", "-", x, fixed = TRUE)
}

parse_utc = function(x) {
  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    return(as.POSIXct(x, tz = "UTC"))
  }
  x_chr = as.character(x)
  out = suppressWarnings(as.POSIXct(x_chr, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  missing = is.na(out)
  if (any(missing)) {
    out[missing] = suppressWarnings(as.POSIXct(x_chr[missing], format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  }
  out
}

shifted_key = function(raw_utc) {
  shifted = raw_utc + 3600
  local_text = format(shifted, tz = "America/New_York", usetz = FALSE)
  local_time = as.POSIXct(local_text, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
  data.table(
    target_trading_day = as.IDate(format(local_time, "%Y-%m-%d")),
    target_bar_time = format(local_time, "%H:%M:%S")
  )
}

PATH_RAW = env_chr("PATH_RAW", file.path("factor_returns_market_diagnostics", "extreme_raw_rows.csv"))
PATH_MARKET_CAP = env_chr("PATH_MARKET_CAP", file.path("data", "eodhd_market_cap", "daily_market_cap.rds"))
PATH_TARGETS = env_chr("PATH_TARGETS", file.path("diagnostics", "market_gap", "top_residual_hours.csv"))
PATH_OUT = env_chr("PATH_OUT", file.path("factor_returns_market_diagnostics", "extreme_contributors_from_raw.csv"))
PATH_SUMMARY_OUT = env_chr("PATH_SUMMARY_OUT", file.path("factor_returns_market_diagnostics", "extreme_contributors_from_raw_summary.csv"))

raw = fread(PATH_RAW, showProgress = FALSE)
raw[, raw_utc := parse_utc(date)]
raw = cbind(raw, shifted_key(raw$raw_utc))
raw[, trading_day := as.IDate(trading_day)]
raw[, market_symbol := normalize_market_symbol(symbol)]

numeric_cols = intersect(
  c("open", "high", "low", "close", "volume", "close_raw", "returns_cc", "returns_intraday", "returns_oc"),
  names(raw)
)
raw[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]

targets = fread(PATH_TARGETS, showProgress = FALSE)
targets[, target_trading_day := as.IDate(trading_day)]
targets[, target_bar_time := bar_time]
targets = targets[, .(
  target_trading_day,
  target_bar_time,
  aleti_mkt,
  our_mcap_oc_file = our_mcap_oc,
  our_mcap_intraday_file = our_mcap_intraday,
  residual_intraday_file = residual_intraday
)]

market_cap = as.data.table(readRDS(PATH_MARKET_CAP))
market_cap[, market_symbol := normalize_market_symbol(symbol)]
market_cap[, trading_day := as.IDate(date) + 1L]
market_cap = market_cap[
  is.finite(market_cap) & market_cap > 0,
  .(market_symbol, trading_day, market_cap = as.numeric(market_cap))
]
market_cap = market_cap[, .SD[.N], by = .(market_symbol, trading_day)]
setkey(market_cap, market_symbol, trading_day)

rows = market_cap[raw, on = .(market_symbol, trading_day)]
rows = targets[rows, on = .(target_trading_day, target_bar_time), nomatch = 0L]
rows = rows[is.finite(market_cap) & market_cap > 0]
rows = rows[is.finite(returns_intraday)]

rows[, `:=`(
  n_symbols = uniqueN(market_symbol),
  sum_market_cap = sum(market_cap, na.rm = TRUE)
), by = .(target_trading_day, target_bar_time)]
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
), by = .(target_trading_day, target_bar_time)]
summary[, abs_residual := abs(residual_intraday_file)]
setorder(summary, -abs_residual)
summary[, abs_residual := NULL]

contributors = rows[
  ,
  .SD[order(-abs(contrib_intraday))][seq_len(min(.N, 20L))],
  by = .(target_trading_day, target_bar_time)
][
  ,
  .(
    target_trading_day,
    target_bar_time,
    symbol,
    market_symbol,
    raw_utc,
    raw_bar_time = bar_time,
    open,
    high,
    low,
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
setorder(contributors, target_trading_day, target_bar_time, -abs_contrib_intraday)
contributors[, abs_contrib_intraday := NULL]

dir.create(dirname(PATH_OUT), recursive = TRUE, showWarnings = FALSE)
fwrite(summary, PATH_SUMMARY_OUT)
fwrite(contributors, PATH_OUT)

cat("Wrote:", normalizePath(PATH_SUMMARY_OUT, winslash = "/", mustWork = FALSE), "\n")
cat("Wrote:", normalizePath(PATH_OUT, winslash = "/", mustWork = FALSE), "\n")
print(summary[1:15])
cat("Top contributors for largest residual hours:\n")
print(contributors[, .SD[1:min(.N, 5L)], by = .(target_trading_day, target_bar_time)][
  target_trading_day %in% summary[1:5, target_trading_day] &
    target_bar_time %in% summary[1:5, target_bar_time]
])
