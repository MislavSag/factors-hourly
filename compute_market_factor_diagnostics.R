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
  value = suppressWarnings(as.integer(value))
  if (is.na(value) || value < 0L) stop(sprintf("%s must be a non-negative integer.", name))
  value
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

PATH_PRICES = env_chr("PATH_PRICES", "prices_factors_hour")
PATH_MARKET_CAP = env_chr("PATH_MARKET_CAP", file.path("data", "eodhd_market_cap", "daily_market_cap.rds"))
PATH_OUT = env_chr("PATH_OUT", file.path("factor_returns_market_diagnostics", "factor_returns_wide.csv"))
DROP_FIRST_BAR = env_bool("SIMPLE_DROP_FIRST_BAR", TRUE)
MARKET_CAP_LAG_DAYS = env_int("SIMPLE_MARKET_CAP_LAG_DAYS", 1L)
MAX_FILES = env_int("SIMPLE_MAX_FILES", 0L)
COLLAPSE_EVERY = env_int("STREAM_COLLAPSE_EVERY", 25L)

price_files = sort(list.files(PATH_PRICES, pattern = "\\.csv$", full.names = TRUE))
if (!length(price_files)) stop(sprintf("No CSV files found in PATH_PRICES: %s", PATH_PRICES))
if (MAX_FILES > 0L) price_files = head(price_files, MAX_FILES)

market_cap = read_optional_table(PATH_MARKET_CAP)
date_col = intersect(c("date", "trading_day", "market_cap_date"), names(market_cap))[1L]
if (!"symbol" %in% names(market_cap) || is.na(date_col) || !"market_cap" %in% names(market_cap)) {
  stop("PATH_MARKET_CAP must contain symbol, date/trading_day, and market_cap columns.")
}
market_cap[, .market_symbol := normalize_market_symbol(symbol)]
market_cap[, trading_day := as.IDate(get(date_col)) + MARKET_CAP_LAG_DAYS]
market_cap[, .market_cap := as.numeric(market_cap)]
market_cap = market_cap[
  nzchar(.market_symbol) & !is.na(trading_day) & is.finite(.market_cap) & .market_cap > 0,
  .(.market_symbol, trading_day, .market_cap)
]
setorder(market_cap, .market_symbol, trading_day)
market_cap = market_cap[, .SD[.N], by = .(.market_symbol, trading_day)]
setkey(market_cap, .market_symbol, trading_day)

collapse_parts = function(parts) {
  combined = rbindlist(parts, use.names = TRUE, fill = TRUE)
  combined[, .(
    n_all = sum(n_all, na.rm = TRUE),
    n_mcap = sum(n_mcap, na.rm = TRUE),
    n_dv = sum(n_dv, na.rm = TRUE),
    sum_ret_oc_all = sum(sum_ret_oc_all, na.rm = TRUE),
    sum_ret_intraday_all = sum(sum_ret_intraday_all, na.rm = TRUE),
    sum_ret_oc_mcap = sum(sum_ret_oc_mcap, na.rm = TRUE),
    sum_ret_intraday_mcap = sum(sum_ret_intraday_mcap, na.rm = TRUE),
    sum_mcap = sum(sum_mcap, na.rm = TRUE),
    sum_mcap2 = sum(sum_mcap2, na.rm = TRUE),
    max_mcap = max(max_mcap, na.rm = TRUE),
    sum_mcap_ret_oc = sum(sum_mcap_ret_oc, na.rm = TRUE),
    sum_mcap_ret_intraday = sum(sum_mcap_ret_intraday, na.rm = TRUE),
    sum_dv = sum(sum_dv, na.rm = TRUE),
    sum_dv_ret_oc = sum(sum_dv_ret_oc, na.rm = TRUE),
    sum_dv_ret_intraday = sum(sum_dv_ret_intraday, na.rm = TRUE)
  ), by = .(date, trading_day, bar_time)]
}

read_header = function(file) names(fread(file, nrows = 0L, showProgress = FALSE))

accum = NULL
for (i in seq_along(price_files)) {
  file = price_files[[i]]
  header = read_header(file)
  required = c("symbol", "date", "trading_day", "bar_time", "returns_oc", "returns_intraday")
  missing = setdiff(required, header)
  if (length(missing)) {
    warning(sprintf("Skipping %s; missing columns: %s", file, paste(missing, collapse = ", ")))
    next
  }

  cols = intersect(c("symbol", "date", "trading_day", "bar_time", "is_first_bar", "returns_oc", "returns_intraday", "close", "volume"), header)
  dt = fread(file, select = cols, showProgress = FALSE)
  if (!nrow(dt)) next
  setDT(dt)

  dt[, symbol := as.character(symbol)]
  dt[, date := as.POSIXct(date, tz = "America/New_York")]
  dt[, trading_day := as.IDate(trading_day)]
  dt[, bar_time := as.character(bar_time)]
  if (!"is_first_bar" %in% names(dt)) dt[, is_first_bar := FALSE]
  if (DROP_FIRST_BAR) dt = dt[is.na(is_first_bar) | is_first_bar == FALSE]
  if (!nrow(dt)) next

  dt[, ret_oc := as.numeric(returns_oc)]
  dt[, ret_intraday := as.numeric(returns_intraday)]
  if (all(c("close", "volume") %in% names(dt))) {
    dt[, dollar_vol := as.numeric(close) * as.numeric(volume)]
    dt[!is.finite(dollar_vol) | dollar_vol <= 0, dollar_vol := NA_real_]
  } else {
    dt[, dollar_vol := NA_real_]
  }

  dt[, .market_symbol := normalize_market_symbol(symbol)]
  setkey(dt, .market_symbol, trading_day)
  dt = market_cap[dt, roll = Inf]

  part = dt[, {
    ok_all_oc = is.finite(ret_oc)
    ok_all_intraday = is.finite(ret_intraday)
    ok_mcap_oc = ok_all_oc & is.finite(.market_cap) & .market_cap > 0
    ok_mcap_intraday = ok_all_intraday & is.finite(.market_cap) & .market_cap > 0
    ok_dv_oc = ok_all_oc & is.finite(dollar_vol) & dollar_vol > 0
    ok_dv_intraday = ok_all_intraday & is.finite(dollar_vol) & dollar_vol > 0
    list(
      n_all = sum(ok_all_intraday | ok_all_oc),
      n_mcap = sum(ok_mcap_intraday | ok_mcap_oc),
      n_dv = sum(ok_dv_intraday | ok_dv_oc),
      sum_ret_oc_all = sum(ret_oc[ok_all_oc]),
      sum_ret_intraday_all = sum(ret_intraday[ok_all_intraday]),
      sum_ret_oc_mcap = sum(ret_oc[ok_mcap_oc]),
      sum_ret_intraday_mcap = sum(ret_intraday[ok_mcap_intraday]),
      sum_mcap = sum(.market_cap[ok_mcap_intraday | ok_mcap_oc]),
      sum_mcap2 = sum(.market_cap[ok_mcap_intraday | ok_mcap_oc]^2),
      max_mcap = if (any(ok_mcap_intraday | ok_mcap_oc)) max(.market_cap[ok_mcap_intraday | ok_mcap_oc]) else NA_real_,
      sum_mcap_ret_oc = sum(ret_oc[ok_mcap_oc] * .market_cap[ok_mcap_oc]),
      sum_mcap_ret_intraday = sum(ret_intraday[ok_mcap_intraday] * .market_cap[ok_mcap_intraday]),
      sum_dv = sum(dollar_vol[ok_dv_intraday | ok_dv_oc]),
      sum_dv_ret_oc = sum(ret_oc[ok_dv_oc] * dollar_vol[ok_dv_oc]),
      sum_dv_ret_intraday = sum(ret_intraday[ok_dv_intraday] * dollar_vol[ok_dv_intraday])
    )
  }, by = .(date, trading_day, bar_time)]

  accum = if (is.null(accum)) part else rbindlist(list(accum, part), use.names = TRUE, fill = TRUE)
  if (!is.null(accum) && i %% COLLAPSE_EVERY == 0L) accum = collapse_parts(list(accum))
  if (i %% 100L == 0L) cat(sprintf("Processed %d/%d price files\n", i, length(price_files)))
}

if (is.null(accum) || !nrow(accum)) stop("No market diagnostic rows were produced.")
accum = collapse_parts(list(accum))
accum[, mkt_ew_all_oc := sum_ret_oc_all / n_all]
accum[, mkt_ew_all_intraday := sum_ret_intraday_all / n_all]
accum[, mkt_ew_mcap_oc := sum_ret_oc_mcap / n_mcap]
accum[, mkt_ew_mcap_intraday := sum_ret_intraday_mcap / n_mcap]
accum[, mkt_mcap_w_oc := sum_mcap_ret_oc / sum_mcap]
accum[, mkt_mcap_w_intraday := sum_mcap_ret_intraday / sum_mcap]
accum[, mkt_dollar_vol_w_oc := sum_dv_ret_oc / sum_dv]
accum[, mkt_dollar_vol_w_intraday := sum_dv_ret_intraday / sum_dv]
accum[, mcap_top1_share := max_mcap / sum_mcap]
accum[, mcap_hhi := sum_mcap2 / (sum_mcap^2)]
setorder(accum, date)

keep_cols = c(
  "date", "trading_day", "bar_time",
  "mkt_ew_all_oc", "mkt_ew_all_intraday",
  "mkt_ew_mcap_oc", "mkt_ew_mcap_intraday",
  "mkt_mcap_w_oc", "mkt_mcap_w_intraday",
  "mkt_dollar_vol_w_oc", "mkt_dollar_vol_w_intraday",
  "n_all", "n_mcap", "n_dv", "sum_mcap", "mcap_top1_share", "mcap_hhi"
)
out = accum[, ..keep_cols]
dir.create(dirname(PATH_OUT), recursive = TRUE, showWarnings = FALSE)
fwrite(out, PATH_OUT)
cat(sprintf("Saved market diagnostics to %s rows=%d\n", PATH_OUT, nrow(out)))
