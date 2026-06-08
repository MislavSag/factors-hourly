# remotes::install_github("MislavSag/finfeatures")
library(data.table)
library(finfeatures)

# TODO: 
# 1) Fix tsfresh after fix in theft package
# 2) add kats sometime in the future if upgrade to python 3.12

# paths
if (interactive()) {
  PATH_PRICES     = file.path("D:/predictors/prices_factors_hour")
  PATH_PREDICTORS = file.path("D:/predictors/daily")
} else {
  PATH_PRICES = file.path("prices")
  PATH_PREDICTORS = file.path("daily")
}

# Create directory if it doesnt exists
if (!dir.exists(PATH_PREDICTORS)) dir.create(PATH_PREDICTORS)

# Get index
if (interactive()) {
  i = 1L
} else {
  i = as.integer(Sys.getenv('PBS_ARRAY_INDEX'))
}

# Get symbol
symbols = gsub("\\.csv", "", list.files(PATH_PRICES))
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
python_virtualenv = Sys.getenv(
  "PADOBRAN_PYTHON_VENV",
  unset = if (interactive()) path.expand("~/projects_py/pyquant") else "/opt/venv"
)
reticulate::use_virtualenv(python_virtualenv, required = TRUE)
# theftms::init_theft(python_virtualenv) # ???
tsfel = reticulate::import("tsfel")
tsfresh = reticulate::import("tsfresh", convert = FALSE)
warnigns = reticulate::import("warnings", convert = FALSE)
warnigns$filterwarnings('ignore')

# Import Ohlcv data
ohlcv_dt = fread(file.path(PATH_PRICES, paste0(symbol_i, ".csv")))
nr = nrow(ohlcv_dt)
ohlcv = Ohlcv$new(ohlcv_dt[, .(symbol, date, open, high, low, close, volume)],
                  date_col = "date")

# Parameters
workers = 1L
lag_ = 0L
at = 1:nr
windows = c(22, 252)

# Exuber
exuber_init = RollingExuber$new(
  windows = c(windows, 504),
  at = at,
  workers = workers,
  lag = lag_,
  exuber_lag = 1L
)
exuber = exuber_init$get_rolling_features(ohlcv, TRUE)

# Backcusum
backcusum_init = RollingBackcusum$new(
  windows = c(windows, min(504, nr)),
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
  h = 22)
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
  windows = c(22, 66, 125, 252, 504),
  quantile_divergence_window = c(22, 66, 125, 252, 504)
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

