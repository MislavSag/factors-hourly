# remotes::install_github("MislavSag/aifinfeatures")
suppressPackageStartupMessages({
  library(data.table)
  library(aifinfeatures)
})

env_chr = function(name, default) {
  value = Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

env_int = function(name, default) {
  value = Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(default)
  value = suppressWarnings(as.integer(value))
  if (is.na(value) || value <= 0L) {
    stop(sprintf("%s must be a positive integer.", name))
  }
  value
}

env_lgl = function(name, default = FALSE) {
  value = Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(default)
  value %in% c("1", "true", "TRUE", "yes", "YES")
}

env_vec = function(name, default = NULL) {
  value = Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(default)
  value = trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  value[nzchar(value)]
}

# paths
if (interactive()) {
  PATH_PRICES = file.path("D:/predictors/prices_factors_hour")
  PATH_PREDICTORS = file.path("D:/predictors/hourly_ai")
} else {
  PATH_PRICES = env_chr("PATH_PRICES", file.path("prices_factors_hour"))
  PATH_PREDICTORS = env_chr("PATH_PREDICTORS_AI", file.path("hourly_ai"))
}

dir.create(PATH_PREDICTORS, recursive = TRUE, showWarnings = FALSE)

# Hourly lookback configuration.
BARS_PER_DAY = env_int("BARS_PER_DAY", 7L)
TRADING_DAYS_PER_MONTH = 21L
TRADING_DAYS_PER_YEAR = 252L

bars = function(days) as.integer(days * BARS_PER_DAY)

W_1D = bars(1L)
W_1M = bars(TRADING_DAYS_PER_MONTH)
W_3M = bars(3L * TRADING_DAYS_PER_MONTH)
W_6M = bars(6L * TRADING_DAYS_PER_MONTH)
W_1Y = bars(TRADING_DAYS_PER_YEAR)

default_windows = c(W_1D, W_1M, W_3M, W_6M, W_1Y)
AI_WINDOWS = env_vec("AI_WINDOWS", default = as.character(default_windows))
AI_WINDOWS = sort(unique(as.integer(AI_WINDOWS)))
AI_WINDOWS = AI_WINDOWS[is.finite(AI_WINDOWS) & AI_WINDOWS > 1L]
if (length(AI_WINDOWS) == 0L) {
  stop("AI_WINDOWS must contain at least one integer window greater than 1.")
}

AI_PROFILE = env_chr("AI_PROFILE", "alpha_v1")
AI_FEATURE_SET = env_vec("AI_FEATURE_SET", default = "alpha_core")
AI_CROSS_SECTIONAL = env_chr("AI_CROSS_SECTIONAL", "rank")
AI_RETURN_COL = env_chr("AI_RETURN_COL", "returns_intraday")
AI_OVERWRITE = env_lgl("AI_OVERWRITE", FALSE)

# Get chunk index.
if (interactive()) {
  i = 1L
} else {
  i = as.integer(Sys.getenv("PBS_ARRAY_INDEX", unset = "1"))
}

files = sort(list.files(PATH_PRICES, pattern = "\\.csv$", full.names = FALSE))
if (length(files) == 0L) {
  stop(sprintf("No CSV files found in PATH_PRICES: %s", PATH_PRICES))
}
if (is.na(i) || i < 1L || i > length(files)) {
  stop(sprintf(
    "Invalid PBS_ARRAY_INDEX=%s for %d price files.",
    Sys.getenv("PBS_ARRAY_INDEX", unset = as.character(i)),
    length(files)
  ))
}

file_i = files[[i]]
input_file = file.path(PATH_PRICES, file_i)
output_file = file.path(PATH_PREDICTORS, file_i)

if (file.exists(output_file) && !AI_OVERWRITE) {
  cat(sprintf("File already exists: %s\n", output_file))
  quit(save = "no", status = 0L)
}

cat(sprintf("Processing: %s\n", file_i))
cat(sprintf(
  "aifinfeatures profile=%s feature_set=%s cross_sectional=%s windows=%s return_col=%s\n",
  AI_PROFILE,
  paste(AI_FEATURE_SET, collapse = ","),
  AI_CROSS_SECTIONAL,
  paste(AI_WINDOWS, collapse = ","),
  AI_RETURN_COL
))

ohlcv_dt = fread(input_file)
setDT(ohlcv_dt)

required_cols = c("symbol", "date", "open", "high", "low", "close", "volume")
missing_cols = setdiff(required_cols, names(ohlcv_dt))
if (length(missing_cols) > 0L) {
  stop(sprintf("Missing required OHLCV columns: %s", paste(missing_cols, collapse = ", ")))
}
if (nzchar(AI_RETURN_COL) && !AI_RETURN_COL %in% names(ohlcv_dt)) {
  stop(sprintf("AI_RETURN_COL='%s' not found in %s", AI_RETURN_COL, input_file))
}

ohlcv_dt[, date := as.POSIXct(date, tz = "America/New_York")]
setorder(ohlcv_dt, symbol, date)

numeric_cols = c("open", "high", "low", "close", "volume")
ohlcv_dt[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]

if (interactive()) {
  ohlcv_dt = ohlcv_dt[, head(.SD, 500L), by = symbol]
}

min_symbol_n = ohlcv_dt[, .N, by = symbol][, min(N)]
AI_WINDOWS = AI_WINDOWS[AI_WINDOWS < min_symbol_n]
if (length(AI_WINDOWS) == 0L) {
  stop(sprintf(
    "Not enough rows per symbol for requested windows. min_symbol_n=%d",
    min_symbol_n
  ))
}

ff_ohlcv = as_ff_ohlcv(ohlcv_dt, date_col = "date", missing = "drop")
if (nzchar(AI_RETURN_COL)) {
  ff_ohlcv[, returns := get(AI_RETURN_COL)]
}

predictors = compute_features(
  ff_ohlcv,
  profile = AI_PROFILE,
  windows = AI_WINDOWS,
  feature_set = AI_FEATURE_SET,
  cross_sectional = AI_CROSS_SECTIONAL,
  missing = "error"
)

setorder(predictors, symbol, date)
fwrite(predictors, output_file)

cat(sprintf(
  "Saved: %s rows=%d cols=%d\n",
  output_file,
  nrow(predictors),
  ncol(predictors)
))
