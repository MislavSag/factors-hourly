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
PATH_OUT = env_chr("PATH_OUT", file.path("factor_returns_market_cap_only", "factor_returns_wide.csv"))
RETURN_COL = env_chr("SIMPLE_RETURN_COL", "returns_oc")
DROP_FIRST_BAR = env_bool("SIMPLE_DROP_FIRST_BAR", TRUE)
MARKET_CAP_LAG_DAYS = env_int("SIMPLE_MARKET_CAP_LAG_DAYS", 1L)
MAX_FILES = env_int("SIMPLE_MAX_FILES", 0L)
COLLAPSE_EVERY = env_int("STREAM_COLLAPSE_EVERY", 25L)

if (!file.exists(PATH_MARKET_CAP)) {
  stop(sprintf("PATH_MARKET_CAP does not exist: %s", PATH_MARKET_CAP))
}

price_files = sort(list.files(PATH_PRICES, pattern = "\\.csv$", full.names = TRUE))
if (!length(price_files)) {
  stop(sprintf("No CSV files found in PATH_PRICES: %s", PATH_PRICES))
}
if (MAX_FILES > 0L) {
  price_files = head(price_files, MAX_FILES)
}

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
    sum_wr = sum(sum_wr, na.rm = TRUE),
    sum_w = sum(sum_w, na.rm = TRUE),
    n = sum(n, na.rm = TRUE),
    n_symbols = sum(n_symbols, na.rm = TRUE)
  ), by = .(date, trading_day, bar_time)]
}

read_header = function(file) {
  names(fread(file, nrows = 0L, showProgress = FALSE))
}

accum = NULL
coverage = data.table(files = integer(), rows = integer(), weighted_rows = integer(), symbols = integer(), weighted_symbols = integer())

for (i in seq_along(price_files)) {
  file = price_files[[i]]
  header = read_header(file)
  required = c("symbol", "date", "trading_day", "bar_time", RETURN_COL)
  missing = setdiff(required, header)
  if (length(missing)) {
    warning(sprintf("Skipping %s; missing columns: %s", file, paste(missing, collapse = ", ")))
    next
  }

  cols = intersect(c("symbol", "date", "trading_day", "bar_time", "is_first_bar", RETURN_COL), header)
  dt = fread(file, select = cols, showProgress = FALSE)
  if (!nrow(dt)) next
  setDT(dt)

  dt[, symbol := as.character(symbol)]
  dt[, date := as.POSIXct(date, tz = "America/New_York")]
  dt[, trading_day := as.IDate(trading_day)]
  dt[, bar_time := as.character(bar_time)]
  if (!"is_first_bar" %in% names(dt)) {
    dt[, is_first_bar := FALSE]
  }
  if (DROP_FIRST_BAR) {
    dt = dt[is.na(is_first_bar) | is_first_bar == FALSE]
  }
  if (!nrow(dt)) next

  dt[, .ret := as.numeric(get(RETURN_COL))]
  dt[, .market_symbol := normalize_market_symbol(symbol)]
  setkey(dt, .market_symbol, trading_day)
  dt = market_cap[dt, roll = Inf]

  rows = nrow(dt)
  weighted_rows = sum(is.finite(dt$.ret) & is.finite(dt$.market_cap) & dt$.market_cap > 0)
  symbols_n = uniqueN(dt$symbol)
  weighted_symbols = uniqueN(dt[is.finite(.ret) & is.finite(.market_cap) & .market_cap > 0, symbol])
  coverage = rbind(
    coverage,
    data.table(files = i, rows = rows, weighted_rows = weighted_rows, symbols = symbols_n, weighted_symbols = weighted_symbols),
    use.names = TRUE
  )

  part = dt[
    is.finite(.ret) & is.finite(.market_cap) & .market_cap > 0,
    .(
      sum_wr = sum(.ret * .market_cap),
      sum_w = sum(.market_cap),
      n = .N,
      n_symbols = uniqueN(symbol)
    ),
    by = .(date, trading_day, bar_time)
  ]

  if (!is.null(part) && nrow(part)) {
    accum = if (is.null(accum)) part else rbindlist(list(accum, part), use.names = TRUE, fill = TRUE)
  }

  if (!is.null(accum) && (i %% COLLAPSE_EVERY == 0L)) {
    accum = collapse_parts(list(accum))
  }
  if (i %% 100L == 0L) {
    cat(sprintf("Processed %d/%d price files\n", i, length(price_files)))
  }
}

if (is.null(accum) || !nrow(accum)) {
  stop("No market-cap weighted rows were produced.")
}

accum = collapse_parts(list(accum))
accum[, MKT_MARKET_CAP_W := fifelse(sum_w > 0, sum_wr / sum_w, NA_real_)]
setorder(accum, date)

out = accum[, .(date, trading_day, bar_time, MKT_MARKET_CAP_W, n, n_symbols, sum_w)]
dir.create(dirname(PATH_OUT), recursive = TRUE, showWarnings = FALSE)
fwrite(out, PATH_OUT)
fwrite(
  coverage[, .(
    rows = sum(rows),
    weighted_rows = sum(weighted_rows),
    symbols = max(symbols),
    weighted_symbols = max(weighted_symbols),
    files = max(files)
  )],
  sub("\\.csv$", "_coverage.csv", PATH_OUT)
)

cat(sprintf(
  "Saved market-cap market factor to %s rows=%d coverage_rows=%d/%d\n",
  PATH_OUT,
  nrow(out),
  sum(coverage$weighted_rows),
  sum(coverage$rows)
))
