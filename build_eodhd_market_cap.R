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
  if (is.na(value) || value < 0L) {
    stop(sprintf("%s must be a non-negative integer.", name))
  }
  value
}

normalize_market_symbol = function(x) {
  x = toupper(trimws(as.character(x)))
  x = sub("\\.[0-9]+$", "", x)
  gsub(".", "-", x, fixed = TRUE)
}

as_idate = function(x) {
  if (inherits(x, "IDate")) return(x)
  if (inherits(x, "Date")) return(as.IDate(x))
  suppressWarnings(as.IDate(as.character(x)))
}

PATH_EODHD_PRICES = env_chr("PATH_EODHD_PRICES", "F:/data/equity/us/us_daily_eodhd")
PATH_EODHD_BALANCE_SHEET_QUARTERLY = env_chr(
  "PATH_EODHD_BALANCE_SHEET_QUARTERLY",
  "F:/data/equity/us/fundamentals/balance-sheet-statement-eodhd/quarter"
)
PATH_HOURLY_PRICES = env_chr("PATH_HOURLY_PRICES", "prices_factors_hour")
PATH_SYMBOLS = env_chr("PATH_SYMBOLS", "")
PATH_MARKET_CAP_OUT = env_chr(
  "PATH_MARKET_CAP_OUT",
  file.path("data", "eodhd_market_cap", "daily_market_cap.rds")
)
DEFAULT_REPORTING_LAG_DAYS = env_int("EODHD_DEFAULT_REPORTING_LAG_DAYS", 90L)
MAX_SYMBOLS = env_int("EODHD_MARKET_CAP_MAX_SYMBOLS", 0L)

read_symbol_filter = function() {
  if (nzchar(PATH_SYMBOLS) && file.exists(PATH_SYMBOLS)) {
    symbols = fread(PATH_SYMBOLS, header = FALSE, col.names = "symbol")$symbol
    return(sort(unique(normalize_market_symbol(symbols))))
  }

  if (!dir.exists(PATH_HOURLY_PRICES)) {
    return(character())
  }

  price_files = sort(list.files(PATH_HOURLY_PRICES, pattern = "\\.csv$", full.names = TRUE))
  symbols = vapply(price_files, function(file) {
    row = tryCatch(fread(file, nrows = 1L, showProgress = FALSE), error = function(e) NULL)
    if (is.null(row) || !"symbol" %in% names(row) || !nrow(row)) return(NA_character_)
    as.character(row$symbol[[1L]])
  }, character(1L), USE.NAMES = FALSE)
  sort(unique(normalize_market_symbol(symbols[!is.na(symbols) & nzchar(symbols)])))
}

symbols = read_symbol_filter()
if (length(symbols) && MAX_SYMBOLS > 0L) {
  symbols = head(symbols, MAX_SYMBOLS)
}

if (length(symbols)) {
  cat(sprintf("Building EODHD market cap for %d requested symbols.\n", length(symbols)))
} else {
  cat("No symbol filter found; building EODHD market cap for every available US daily price file.\n")
}

quarter_files = sort(list.files(
  PATH_EODHD_BALANCE_SHEET_QUARTERLY,
  pattern = "\\.csv$",
  full.names = TRUE
))
if (!length(quarter_files)) {
  stop(sprintf("No quarterly balance-sheet CSV files found: %s", PATH_EODHD_BALANCE_SHEET_QUARTERLY))
}

read_one_quarter = function(file) {
  header = names(fread(file, nrows = 0L, showProgress = FALSE))
  required = c("symbol", "date", "commonStockSharesOutstanding")
  missing = setdiff(required, header)
  if (length(missing)) {
    warning(sprintf("Skipping %s; missing columns: %s", file, paste(missing, collapse = ", ")))
    return(NULL)
  }
  cols = intersect(c("symbol", "date", "filing_date", "commonStockSharesOutstanding"), header)
  out = fread(file, select = cols, showProgress = FALSE)
  setDT(out)
  if (!"filing_date" %in% names(out)) {
    out[, filing_date := as.IDate(NA)]
  }
  out[, symbol := normalize_market_symbol(symbol)]
  if (length(symbols)) {
    out = out[symbol %in% symbols]
  }
  if (!nrow(out)) return(NULL)

  out[, fiscal_date_ending := as_idate(date)]
  out[, filing_date := as_idate(filing_date)]
  out[, fund_available_date := filing_date]
  out[is.na(fund_available_date), fund_available_date := fiscal_date_ending + DEFAULT_REPORTING_LAG_DAYS]
  out[, shares_outstanding := as.numeric(commonStockSharesOutstanding)]
  out = out[
    !is.na(symbol) &
      nzchar(symbol) &
      !is.na(fund_available_date) &
      is.finite(shares_outstanding) &
      shares_outstanding > 0,
    .(symbol, fiscal_date_ending, fund_available_date, shares_outstanding)
  ]
  out
}

shares = rbindlist(lapply(quarter_files, read_one_quarter), use.names = TRUE, fill = TRUE)
if (!nrow(shares)) {
  stop("No valid shares outstanding observations after filtering.")
}

setorder(shares, symbol, fiscal_date_ending, fund_available_date)
shares = shares[, .SD[.N], by = .(symbol, fiscal_date_ending)]
shares[, market_cap_source_date := fund_available_date]
shares = shares[, .(symbol, fund_available_date, market_cap_source_date, shares_outstanding)]
setorder(shares, symbol, fund_available_date)
shares = shares[, .SD[.N], by = .(symbol, fund_available_date)]

price_files_all = sort(list.files(PATH_EODHD_PRICES, pattern = "\\.csv$", full.names = TRUE))
if (!length(price_files_all)) {
  stop(sprintf("No EODHD daily price CSV files found: %s", PATH_EODHD_PRICES))
}

file_symbols = normalize_market_symbol(sub("_US\\.csv$", "", basename(price_files_all), ignore.case = TRUE))
if (length(symbols)) {
  price_files = price_files_all[file_symbols %in% symbols]
  file_symbols = file_symbols[file_symbols %in% symbols]
} else {
  price_files = price_files_all
}

if (!length(price_files)) {
  stop("No EODHD price files match the requested symbols.")
}

read_one_price = function(file, symbol) {
  header = names(fread(file, nrows = 0L, showProgress = FALSE))
  date_col = intersect(c("Date", "date", "datetime", "timestamp"), header)[1L]
  adjusted_col = intersect(c("Adjusted_close", "adjusted_close", "adjustedclose", "Adj_Close", "adj_close", "Close", "close"), header)[1L]
  if (is.na(date_col) || is.na(adjusted_col)) {
    warning(sprintf("Skipping price file without date/adjusted close: %s", file))
    return(NULL)
  }
  out = fread(file, select = c(date_col, adjusted_col), showProgress = FALSE)
  setDT(out)
  setnames(out, c(date_col, adjusted_col), c("date", "adjusted_close"))
  out[, symbol := symbol]
  out[, date := as_idate(date)]
  out[, adjusted_close := as.numeric(adjusted_close)]
  out = out[!is.na(date) & is.finite(adjusted_close) & adjusted_close > 0]
  out[, .(symbol, date, adjusted_close)]
}

prices = vector("list", length(price_files))
for (i in seq_along(price_files)) {
  prices[[i]] = read_one_price(price_files[[i]], file_symbols[[i]])
  if (i %% 250L == 0L) {
    cat(sprintf("Read %d/%d EODHD daily price files\n", i, length(price_files)))
  }
}
prices = rbindlist(prices, use.names = TRUE, fill = TRUE)
if (!nrow(prices)) {
  stop("No valid EODHD daily price rows after filtering.")
}

setkey(shares, symbol, fund_available_date)
prices_join = copy(prices)
setnames(prices_join, "date", "fund_available_date")
setkey(prices_join, symbol, fund_available_date)
market_cap = shares[prices_join, roll = Inf]
setnames(market_cap, "fund_available_date", "date")
market_cap[, market_cap := adjusted_close * shares_outstanding]
market_cap = market_cap[
  !is.na(date) & is.finite(market_cap) & market_cap > 0,
  .(symbol, date, adjusted_close, shares_outstanding, market_cap, market_cap_source_date)
]
setorder(market_cap, symbol, date)
market_cap = unique(market_cap, by = c("symbol", "date"))

dir.create(dirname(PATH_MARKET_CAP_OUT), recursive = TRUE, showWarnings = FALSE)
if (grepl("\\.parquet$", PATH_MARKET_CAP_OUT, ignore.case = TRUE)) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Writing parquet output requires the arrow package.")
  }
  arrow::write_parquet(market_cap, PATH_MARKET_CAP_OUT)
} else if (grepl("\\.rds$", PATH_MARKET_CAP_OUT, ignore.case = TRUE)) {
  saveRDS(market_cap, PATH_MARKET_CAP_OUT)
} else {
  fwrite(market_cap, PATH_MARKET_CAP_OUT)
}

coverage = data.table(
  requested_symbols = length(symbols),
  symbols_with_market_cap = uniqueN(market_cap$symbol),
  first_date = min(market_cap$date),
  last_date = max(market_cap$date),
  rows = nrow(market_cap)
)
fwrite(coverage, paste0(PATH_MARKET_CAP_OUT, ".coverage.csv"))

cat(sprintf(
  "Saved EODHD daily market cap to %s rows=%d symbols=%d date_range=%s..%s\n",
  PATH_MARKET_CAP_OUT,
  nrow(market_cap),
  uniqueN(market_cap$symbol),
  min(market_cap$date),
  max(market_cap$date)
))
