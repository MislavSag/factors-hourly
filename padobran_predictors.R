# remotes::install_github("MislavSag/finfeatures")
library(data.table)
library(finfeatures)

# TODO: 
# 1) Fix tsfresh after fix in theft package
# 2) add kats sometime in the future if upgrade to python 3.12

# paths
if (interactive()) {
  PATH_PRICES     = file.path("D:/predictors/prices_factors_hour")
  PATH_PREDICTORS = file.path("D:/predictors/hourly")
} else {
  PATH_PRICES = file.path("prices")
  PATH_PREDICTORS = file.path("hourly")
}

# Create directory if it doesnt exists
if (!dir.exists(PATH_PREDICTORS)) dir.create(PATH_PREDICTORS, recursive = TRUE)

# Hourly lookback configuration.
# QuantConnect US-equity hourly bars are treated as intraday bars, not days.
get_env_int = function(name, default) {
  value = Sys.getenv(name, unset = "")
  if (value == "") {
    return(default)
  }
  value = suppressWarnings(as.integer(value))
  if (is.na(value) || value <= 0L) {
    stop(sprintf("%s must be a positive integer.", name))
  }
  value
}

BARS_PER_DAY = get_env_int("BARS_PER_DAY", 7L)
TRADING_DAYS_PER_MONTH = 21L
TRADING_DAYS_PER_YEAR = 252L

bars = function(days) as.integer(days * BARS_PER_DAY)

W_1D = bars(1L)
W_1M = bars(TRADING_DAYS_PER_MONTH)
W_3M = bars(3L * TRADING_DAYS_PER_MONTH)
W_6M = bars(6L * TRADING_DAYS_PER_MONTH)
W_1Y = bars(TRADING_DAYS_PER_YEAR)
W_2Y = bars(2L * TRADING_DAYS_PER_YEAR)

BASE_WINDOWS = c(W_1M, W_1Y)
LONG_WINDOWS = c(BASE_WINDOWS, W_2Y)
OHLCV_WINDOWS = c(W_1M, W_3M, W_6M, W_1Y, W_2Y)
FORECAST_HORIZON = W_1M

available_windows = function(windows, min_n) {
  windows = sort(unique(as.integer(windows)))
  windows[windows < min_n]
}

# Get index
if (interactive()) {
  i = 1L
} else {
  i = as.integer(Sys.getenv('PBS_ARRAY_INDEX'))
}

# Get symbol
symbols = gsub("\\.csv", "", list.files(PATH_PRICES, pattern = "\\.csv$"))
if (length(symbols) == 0L) {
  stop(sprintf("No CSV files found in PATH_PRICES: %s", PATH_PRICES))
}
if (is.na(i) || i < 1L || i > length(symbols)) {
  stop(sprintf(
    "Invalid PBS_ARRAY_INDEX=%s for %d price files.",
    Sys.getenv("PBS_ARRAY_INDEX", unset = as.character(i)),
    length(symbols)
  ))
}
symbol_i = symbols[i]

# If files already exists cont
file_name = file.path(PATH_PREDICTORS, paste0(symbol_i, ".csv"))
if (!file.exists(file_name)) {
  cat(sprintf("Processing: %s\n", symbol_i))
} else {
  cat(sprintf("File already exists: %s\n", file_name))
  quit(save = "no", status = 0)
}

# python environment
python_virtualenv = Sys.getenv("FACTORS_PYTHON_VENV", unset = "")
if (python_virtualenv == "") {
  python_virtualenv = if (interactive()) file.path(getwd(), ".venv") else "/opt/venv"
}
reticulate::use_virtualenv(python_virtualenv, required = TRUE)
# theftms::init_theft(python_virtualenv) # ???
tsfel = reticulate::import("tsfel")
tsfresh = reticulate::import("tsfresh", convert = FALSE)
warnigns = reticulate::import("warnings", convert = FALSE)
warnigns$filterwarnings('ignore')

# Import Ohlcv data
ohlcv_dt = fread(file.path(PATH_PRICES, paste0(symbol_i, ".csv")))
attr(ohlcv_dt$date, "tzone") = "America/New_York"
setorder(ohlcv_dt, symbol, date)
if (interactive()) {
  ohlcv_dt = ohlcv_dt[1:250]
}
nr = nrow(ohlcv_dt)
min_symbol_n = ohlcv_dt[, .N, by = symbol][, min(N)]
ohlcv_extra_cols = c(
  "returns_cc",
  "returns_intraday",
  "returns_oc",
  "trading_day",
  "bar_time",
  "is_first_bar"
)
missing_required_cols = setdiff("returns_intraday", names(ohlcv_dt))
if (length(missing_required_cols) > 0L) {
  stop(sprintf(
    "Missing required intraday return column: %s",
    paste(missing_required_cols, collapse = ", ")
  ))
}
ohlcv_cols = c(
  "symbol",
  "date",
  "open",
  "high",
  "low",
  "close",
  "volume",
  intersect(ohlcv_extra_cols, names(ohlcv_dt))
)
ohlcv = Ohlcv$new(ohlcv_dt[, .SD, .SDcols = ohlcv_cols],
                  date_col = "date")
ohlcv$X[, returns := returns_intraday]

# Parameters
workers = 1L
lag_ = 0L
at = seq_len(nr)
windows = available_windows(BASE_WINDOWS, min_symbol_n)
long_windows = available_windows(LONG_WINDOWS, min_symbol_n)
ohlcv_windows = available_windows(OHLCV_WINDOWS, min_symbol_n)

if (length(windows) == 0L || length(ohlcv_windows) == 0L) {
  stop(sprintf(
    "Not enough rows per symbol for hourly features. min_symbol_n=%d, BARS_PER_DAY=%d, shortest_window=%d",
    min_symbol_n,
    BARS_PER_DAY,
    min(BASE_WINDOWS)
  ))
}

cat(sprintf(
  "Hourly config: BARS_PER_DAY=%d, base_windows=%s, long_windows=%s, ohlcv_windows=%s\n",
  BARS_PER_DAY,
  paste(windows, collapse = ","),
  paste(long_windows, collapse = ","),
  paste(ohlcv_windows, collapse = ",")
))

# Exuber
exuber_init = RollingExuber$new(
  windows = long_windows,
  at = at,
  workers = workers,
  lag = lag_,
  exuber_lag = 1L
)
exuber = exuber_init$get_rolling_features(ohlcv, TRUE)

# Backcusum
backcusum_init = RollingBackcusum$new(
  windows = long_windows,
  workers = workers,
  at = at,
  lag = lag_,
  alternative = c("greater", "two.sided"),
  return_power = c(1, 2))
backcusum = backcusum_init$get_rolling_features(ohlcv)

# Theft r
theft_init = RollingTheft$new(
  windows = windows,
  workers = workers,
  at = at,
  lag = lag_,
  features_set = c("catch22", "feasts"))
theft_r = suppressMessages(theft_init$get_rolling_features(ohlcv))

# Theft py
theft_init = RollingTheft$new(
  windows = windows,
  workers = 1L,
  at = at,
  lag = lag_,
  features_set = c("tsfel"))
theft_py = suppressMessages(theft_init$get_rolling_features(ohlcv))

# Forecasts
forecasts_init = RollingForecats$new(
  windows = windows,
  workers = workers,
  at = at,
  lag = lag_,
  forecast_type = c("autoarima", "nnetar", "ets"),
  h = FORECAST_HORIZON)
forecasts = suppressMessages(forecasts_init$get_rolling_features(ohlcv))

# Tsfeatures
tsfeatures_init = RollingTsfeatures$new(
  windows = windows[windows > 51],
  workers = workers,
  at = at,
  lag = lag_,
  scale = TRUE)
tsfeatures = suppressMessages(tsfeatures_init$get_rolling_features(ohlcv))

# WaveletArima
waveletarima_init = RollingWaveletArima$new(
  windows = windows,
  workers = workers,
  at = at,
  lag = lag_,
  filter = "haar")
waveletarima = suppressMessages(waveletarima_init$get_rolling_features(ohlcv))

# FracDiff
fracdiff_init = RollingFracdiff$new(
  windows = windows,
  workers = workers,
  at = at,
  lag = lag_,
  nar = c(1), 
  nma = c(1),
  bandw_exp = c(0.1, 0.5, 0.9))
fracdiff = suppressMessages(suppressWarnings(fracdiff_init$get_rolling_features(ohlcv)))

# Theft r with returns
theft_init = RollingTheft$new(
  windows = windows,
  workers = workers,
  at = at,
  lag = lag_,
  features_set = c("catch22", "feasts"))
theft_r_r = suppressMessages(theft_init$get_rolling_features(ohlcv, price_col = "returns"))
cols = colnames(theft_r_r)[3:ncol(theft_r_r)]
setnames(theft_r_r, cols, paste0(cols, "_r"))

# Theft py with returns
theft_init = RollingTheft$new(
  windows = windows,
  workers = 1L,
  at = at,
  lag = lag_,
  features_set = c("tsfel"))
theft_py_r = suppressMessages(theft_init$get_rolling_features(ohlcv, price_col = "returns"))
cols = colnames(theft_py_r)[3:ncol(theft_py_r)]
setnames(theft_py_r, cols, paste0(cols, "_r"))

# Ohlcv predictors
ohlcv_predictors_init = OhlcvFeaturesDaily$new(
  windows = ohlcv_windows,
  quantile_divergence_window = ohlcv_windows
)
ohlcv_predictors = ohlcv_predictors_init$get_ohlcv_features(copy(ohlcv_dt))

# Combine all predictors
predictors = Reduce(
  function(x, y)
    merge(x, y, by = c("symbol", "date")),
  list(
    ohlcv_predictors,
    exuber,
    backcusum,
    theft_r,
    theft_py,
    forecasts,
    tsfeatures,
    waveletarima,
    fracdiff,
    theft_r_r,
    theft_py_r
  )
)

# Save predictors

fwrite(predictors, file_name)
cat(sprintf("Saved: %s\n", file_name))

