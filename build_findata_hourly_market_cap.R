suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

env_chr = function(name, default) {
  value = Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

env_num = function(name, default) {
  value = Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(default)
  parsed = suppressWarnings(as.numeric(value))
  if (!is.finite(parsed) || parsed <= 0) {
    stop(sprintf("%s must be a positive number.", name))
  }
  parsed
}

normalize_hourly_symbol = function(x) {
  x = toupper(trimws(as.character(x)))
  x = sub("\\.[0-9]+$", "", x)
  gsub(".", "-", x, fixed = TRUE)
}

PATH_FINDATA_DIR = env_chr("PATH_FINDATA_DIR", "H:/data/findata/eodhd")
PATH_HOURLY_SYMBOLS = env_chr(
  "PATH_HOURLY_SYMBOLS",
  file.path("data", "eodhd_market_cap", "hourly_symbols_padobran.txt")
)
PATH_OUT = env_chr(
  "PATH_OUT",
  file.path("data", "findata_market_cap", "daily_market_cap_clean_hourly.rds")
)
MAX_ADJUSTED_CLOSE = env_num("MAX_ADJUSTED_CLOSE", Inf)
MAX_MARKET_CAP = env_num("MAX_MARKET_CAP", Inf)
MAX_MARKET_CAP_PRE_2010 = env_num("MAX_MARKET_CAP_PRE_2010", Inf)
MAX_MARKET_CAP_PRE_2018 = env_num("MAX_MARKET_CAP_PRE_2018", Inf)
MAX_MARKET_CAP_PRE_2020 = env_num("MAX_MARKET_CAP_PRE_2020", Inf)
MAX_MARKET_CAP_PRE_2022 = env_num("MAX_MARKET_CAP_PRE_2022", Inf)

path_daily_market_cap = file.path(PATH_FINDATA_DIR, "daily_market_cap.parquet")
path_security_master = file.path(PATH_FINDATA_DIR, "security_master.parquet")
path_outliers = file.path(PATH_FINDATA_DIR, "market_cap_outliers.csv")

required = c(path_daily_market_cap, path_security_master, PATH_HOURLY_SYMBOLS)
missing = required[!file.exists(required)]
if (length(missing)) {
  stop(sprintf("Missing required file(s): %s", paste(missing, collapse = ", ")))
}

hourly_symbols = fread(PATH_HOURLY_SYMBOLS, header = FALSE, col.names = "symbol")
hourly_symbols[, base_symbol := normalize_hourly_symbol(symbol)]
hourly_symbols = unique(hourly_symbols[nzchar(base_symbol), .(base_symbol)])

security_master = as.data.table(read_parquet(path_security_master))
if (!all(c("symbol", "base_symbol") %in% names(security_master))) {
  stop("security_master.parquet must contain symbol and base_symbol.")
}
security_master[, base_symbol := normalize_hourly_symbol(base_symbol)]
security_master = security_master[base_symbol %in% hourly_symbols$base_symbol]
security_master = unique(security_master[, .(findata_symbol = symbol, base_symbol)])

if (!nrow(security_master)) {
  stop("No findata security_master symbols match hourly symbols.")
}

message(sprintf(
  "Matched %d findata securities to %d hourly base symbols.",
  nrow(security_master),
  uniqueN(security_master$base_symbol)
))

daily_market_cap = as.data.table(
  open_dataset(path_daily_market_cap) |>
    dplyr::filter(symbol %in% security_master$findata_symbol) |>
    dplyr::select(symbol, date, adjusted_close, shares_outstanding, market_cap, market_cap_source_date) |>
    dplyr::collect()
)

setnames(daily_market_cap, "symbol", "findata_symbol")
daily_market_cap = security_master[daily_market_cap, on = "findata_symbol", nomatch = 0L]
daily_market_cap[, date := as.IDate(date)]
daily_market_cap[, market_cap_source_date := as.IDate(market_cap_source_date)]
daily_market_cap[, market_cap := as.numeric(market_cap)]

if (file.exists(path_outliers)) {
  outliers = fread(path_outliers, showProgress = FALSE)
  if (all(c("symbol", "date", "market_cap_outlier") %in% names(outliers))) {
    outliers = outliers[market_cap_outlier == TRUE, .(
      findata_symbol = symbol,
      date = as.IDate(date),
      market_cap_outlier = TRUE
    )]
    setkey(outliers, findata_symbol, date)
    setkey(daily_market_cap, findata_symbol, date)
    daily_market_cap = outliers[daily_market_cap]
    daily_market_cap[is.na(market_cap_outlier), market_cap_outlier := FALSE]
    daily_market_cap[market_cap_outlier == TRUE, market_cap := NA_real_]
    daily_market_cap[, market_cap_outlier := NULL]
  } else {
    warning("market_cap_outliers.csv does not have symbol/date/market_cap_outlier; skipping outlier cleaning.")
  }
}

out = daily_market_cap[
  ,
  .(
    symbol = base_symbol,
    date,
    adjusted_close = as.numeric(adjusted_close),
    shares_outstanding = as.numeric(shares_outstanding),
    market_cap,
    market_cap_source_date,
    findata_symbol
  )
]
out = out[!is.na(date)]

out[, adjusted_close := as.numeric(adjusted_close)]
out[, shares_outstanding := as.numeric(shares_outstanding)]
out[, market_cap := as.numeric(market_cap)]

removed_adjusted_close = 0L
if (is.finite(MAX_ADJUSTED_CLOSE)) {
  bad_adjusted_close = is.finite(out$adjusted_close) & out$adjusted_close > MAX_ADJUSTED_CLOSE
  removed_adjusted_close = sum(bad_adjusted_close & is.finite(out$market_cap), na.rm = TRUE)
  out[bad_adjusted_close, market_cap := NA_real_]
}

cap_limit = rep(MAX_MARKET_CAP, nrow(out))
if (is.finite(MAX_MARKET_CAP_PRE_2022)) {
  cap_limit[out$date < as.IDate("2022-01-01")] = pmin(
    cap_limit[out$date < as.IDate("2022-01-01")],
    MAX_MARKET_CAP_PRE_2022
  )
}
if (is.finite(MAX_MARKET_CAP_PRE_2020)) {
  cap_limit[out$date < as.IDate("2020-01-01")] = pmin(
    cap_limit[out$date < as.IDate("2020-01-01")],
    MAX_MARKET_CAP_PRE_2020
  )
}
if (is.finite(MAX_MARKET_CAP_PRE_2018)) {
  cap_limit[out$date < as.IDate("2018-01-01")] = pmin(
    cap_limit[out$date < as.IDate("2018-01-01")],
    MAX_MARKET_CAP_PRE_2018
  )
}
if (is.finite(MAX_MARKET_CAP_PRE_2010)) {
  cap_limit[out$date < as.IDate("2010-01-01")] = pmin(
    cap_limit[out$date < as.IDate("2010-01-01")],
    MAX_MARKET_CAP_PRE_2010
  )
}
bad_market_cap = is.finite(out$market_cap) & is.finite(cap_limit) & out$market_cap > cap_limit
removed_market_cap_limit = sum(bad_market_cap, na.rm = TRUE)
out[bad_market_cap, market_cap := NA_real_]

# If multiple findata securities map to the same base symbol/date, keep the largest
# finite market cap. This avoids duplicate weights while preserving primary shares.
out[, rank_market_cap := fifelse(is.finite(market_cap), market_cap, -Inf)]
setorder(out, symbol, date, -rank_market_cap)
out = out[, .SD[1L], by = .(symbol, date)]
out[, rank_market_cap := NULL]
setorder(out, symbol, date)

dir.create(dirname(PATH_OUT), recursive = TRUE, showWarnings = FALSE)
if (grepl("\\.parquet$", PATH_OUT, ignore.case = TRUE)) {
  write_parquet(out, PATH_OUT)
} else if (grepl("\\.rds$", PATH_OUT, ignore.case = TRUE)) {
  saveRDS(out, PATH_OUT)
} else {
  fwrite(out, PATH_OUT)
}

coverage = data.table(
  hourly_symbols = nrow(hourly_symbols),
  matched_hourly_symbols = uniqueN(out$symbol),
  rows = nrow(out),
  first_date = min(out$date, na.rm = TRUE),
  last_date = max(out$date, na.rm = TRUE),
  finite_market_cap_rows = sum(is.finite(out$market_cap)),
  missing_market_cap_rows = sum(!is.finite(out$market_cap)),
  removed_adjusted_close_rows = removed_adjusted_close,
  removed_market_cap_limit_rows = removed_market_cap_limit,
  max_adjusted_close = MAX_ADJUSTED_CLOSE,
  max_market_cap = MAX_MARKET_CAP,
  max_market_cap_pre_2010 = MAX_MARKET_CAP_PRE_2010,
  max_market_cap_pre_2018 = MAX_MARKET_CAP_PRE_2018,
  max_market_cap_pre_2020 = MAX_MARKET_CAP_PRE_2020,
  max_market_cap_pre_2022 = MAX_MARKET_CAP_PRE_2022
)
fwrite(coverage, paste0(PATH_OUT, ".coverage.csv"))

message(sprintf(
  "Saved findata hourly market cap to %s rows=%d symbols=%d",
  normalizePath(PATH_OUT, winslash = "/", mustWork = FALSE),
  nrow(out),
  uniqueN(out$symbol)
))
print(coverage)
