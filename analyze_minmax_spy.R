suppressPackageStartupMessages({
  library(data.table)
  library(finutils)
  library(PerformanceAnalytics)
})


# Setup -----------------------------------------------------------------------
PATH_LEAN = "C:/Users/Mislav/qc_snp/data"

INPUT_DIR = file.path("data", "derived")
OUTPUT_DIR = file.path("data", "analysis")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

TRAIN_FRAC = 0.70

# Default selected strategy for the equity curve output.
STRATEGY_MODEL = "linear_minmax"

# "exclude": do not trade the first bar of the day.
# "next_open_close": use the prior day's last signal to trade next day's first
# open-close bar. This does not hold from previous close to next open.
# "close_to_open": use the prior day's last signal to trade previous close to
# next open. This is an overnight/open-return diagnostic, not the default.
FIRST_BAR_MODE = "exclude"

# "static": fit once on the initial train sample.
# "year": expanding-window retrain at the start of each OOS year.
RETRAIN_PERIOD = "year"

# If TRUE, model fitting gives larger weight to newer train observations.
USE_TRAIN_WEIGHTS = FALSE

# "sign": always +1/-1.
# "zscore_1x": fractional sizing clipped to [-1, 1].
# "zscore_2x": fractional sizing clipped to [-2, 2].
POSITION_SIZING = "sign"

# If USE_TRAIN_WEIGHTS is TRUE, model fitting uses exponentially decaying
# train weights with this half-life measured in hourly trade observations.
TRAIN_WEIGHT_HALF_LIFE = 252L * 6L * 3L

# TRUE runs the comparison grid and writes minmax_spy_grid_performance.csv.
RUN_GRID = TRUE


# Environment overrides -------------------------------------------------------
env_value = function(name, default) {
  value = Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}

env_bool = function(name, default) {
  value = Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) {
    return(default)
  }
  tolower(value) %in% c("1", "true", "t", "yes", "y")
}

STRATEGY_MODEL = env_value("STRATEGY_MODEL", STRATEGY_MODEL)
FIRST_BAR_MODE = env_value("FIRST_BAR_MODE", FIRST_BAR_MODE)
RETRAIN_PERIOD = env_value("RETRAIN_PERIOD", RETRAIN_PERIOD)
USE_TRAIN_WEIGHTS = env_bool("USE_TRAIN_WEIGHTS", USE_TRAIN_WEIGHTS)
POSITION_SIZING = env_value("POSITION_SIZING", POSITION_SIZING)
RUN_GRID = env_bool("RUN_GRID", RUN_GRID)


# Helpers ---------------------------------------------------------------------
valid_models = c("linear_minmax", "linear_minmax_plus_hour")
valid_first_bar_modes = c("exclude", "next_open_close", "close_to_open")
valid_retrain_periods = c("static", "year")
valid_position_sizing = c("sign", "zscore_1x", "zscore_2x")

stopifnot(
  STRATEGY_MODEL %in% valid_models,
  FIRST_BAR_MODE %in% valid_first_bar_modes,
  RETRAIN_PERIOD %in% valid_retrain_periods,
  POSITION_SIZING %in% valid_position_sizing
)

make_return_xts = function(ret, dates) {
  ok = is.finite(ret) & !is.na(dates)
  xts::xts(ret[ok], order.by = dates[ok])
}

annual_scale = function(dt) {
  trades_per_day = dt[, .N, by = as.IDate(trade_date)]$N
  as.numeric(252 * median(trades_per_day, na.rm = TRUE))
}

perf_stats = function(dt, ret_col, label, scale) {
  returns_xts = make_return_xts(dt[[ret_col]], dt$trade_date)
  ret = as.numeric(returns_xts)

  data.table(
    model = label,
    n = length(ret),
    mean_bp = mean(ret) * 10000,
    sd_bp = sd(ret) * 10000,
    t_stat = mean(ret) / sd(ret) * sqrt(length(ret)),
    annualized_return = as.numeric(Return.annualized(returns_xts, scale = scale)),
    annualized_sd = as.numeric(StdDev.annualized(returns_xts, scale = scale)),
    annualized_sharpe = as.numeric(SharpeRatio.annualized(
      returns_xts,
      Rf = 0,
      scale = scale
    )),
    hit_rate = mean(ret > 0),
    cum_return = as.numeric(Return.cumulative(returns_xts)),
    max_drawdown = as.numeric(maxDrawdown(returns_xts))
  )
}

train_weights = function(n, half_life) {
  age = rev(seq_len(n) - 1L)
  w = 0.5 ^ (age / half_life)
  w / mean(w)
}

position_from_prediction = function(pred, center, pred_sd, sizing) {
  if (sizing == "sign") {
    return(fifelse(pred > center, 1, -1))
  }

  pred_sd = fifelse(is.finite(pred_sd) & pred_sd > 0, pred_sd, 1)

  raw_position = (pred - center) / pred_sd

  if (sizing == "zscore_1x") {
    return(pmax(pmin(raw_position, 1), -1))
  }

  if (sizing == "zscore_2x") {
    return(pmax(pmin(raw_position, 2), -2))
  }

  stop("Unknown sizing: ", sizing)
}


# Data ------------------------------------------------------------------------
minmax_indicators = readRDS(file.path(INPUT_DIR, "minmax_indicators.rds"))
minmax_params = readRDS(file.path(INPUT_DIR, "minmax_params.rds"))
setDT(minmax_indicators)
setDT(minmax_params)

spy = qc_hour_parquet(
  file_path = file.path(PATH_LEAN, "all_stocks_hour"),
  symbols = "spy",
  min_obs = 1,
  duplicates = "fast",
  add_dv_rank = FALSE
)
setDT(spy)
setorder(spy, date)

spy[, row_in_day := seq_len(.N), by = trading_day]
spy[, rows_in_day := .N, by = trading_day]
spy[, spy_is_first_bar := row_in_day == 1L]
spy[, spy_is_last_bar := row_in_day == rows_in_day]

spy_daily = spy[, .(
  first_open = data.table::first(open),
  first_date = data.table::first(date),
  last_close = data.table::last(close),
  last_date = data.table::last(date)
), by = trading_day]
setorder(spy_daily, trading_day)
spy_daily[, next_first_open := shift(first_open, type = "lead")]
spy_daily[, next_first_date := shift(first_date, type = "lead")]
spy_daily[, close_to_next_open := next_first_open / last_close - 1]

spy = spy_daily[
  ,
  .(trading_day, next_first_date, close_to_next_open)
][spy, on = "trading_day"]
setorder(spy, date)

spy = spy[, .(
  date,
  spy_returns_oc = returns_oc,
  spy_trading_day = trading_day,
  signal_bar_time = bar_time,
  row_in_day,
  rows_in_day,
  spy_is_first_bar,
  spy_is_last_bar,
  next_first_date,
  close_to_next_open
)]
spy[, target_next_oc := shift(spy_returns_oc, type = "lead")]
spy[, target_date_next_oc := shift(date, type = "lead")]
spy[, target_bar_time := shift(signal_bar_time, type = "lead")]
spy[, target_trading_day := shift(spy_trading_day, type = "lead")]
spy[, target_is_first_bar := shift(spy_is_first_bar, type = "lead")]
spy[, same_day_next := spy_trading_day == target_trading_day]

base_dt = merge(minmax_indicators, spy, by = "date", all = FALSE)
setorder(base_dt, date)

feature_cols = c(
  "up_count", "down_count",
  "up_severity", "down_severity",
  "count_imbalance", "severity_imbalance",
  "cs_dispersion"
)


# Target construction ---------------------------------------------------------
make_model_dt = function(first_bar_mode) {
  dt = copy(base_dt)

  if (first_bar_mode == "exclude") {
    dt = dt[same_day_next == TRUE]
    dt[, target_ret := target_next_oc]
    dt[, trade_date := target_date_next_oc]
    dt[, trade_type := "same_day_next_oc"]
  } else if (first_bar_mode == "next_open_close") {
    dt = dt[same_day_next == TRUE | target_is_first_bar == TRUE]
    dt[, target_ret := target_next_oc]
    dt[, trade_date := target_date_next_oc]
    dt[, trade_type := fifelse(
      same_day_next == TRUE,
      "same_day_next_oc",
      "next_day_first_bar_oc"
    )]
  } else if (first_bar_mode == "close_to_open") {
    dt = dt[
      same_day_next == TRUE |
        (spy_is_last_bar == TRUE & is.finite(close_to_next_open))
    ]
    dt[, target_ret := fifelse(
      same_day_next == TRUE,
      target_next_oc,
      close_to_next_open
    )]
    dt[, trade_date := fifelse(
      same_day_next == TRUE,
      target_date_next_oc,
      next_first_date
    )]
    dt[, trade_date := as.POSIXct(trade_date, origin = "1970-01-01", tz = attr(date, "tzone"))]
    dt[, trade_type := fifelse(
      same_day_next == TRUE,
      "same_day_next_oc",
      "close_to_next_open"
    )]
  } else {
    stop("Unknown first_bar_mode: ", first_bar_mode)
  }

  dt = dt[!is.na(target_ret) & !is.na(trade_date)]
  dt = dt[complete.cases(dt[, c(feature_cols, "target_ret"), with = FALSE])]
  setorder(dt, trade_date, date)
  dt
}


# Modeling --------------------------------------------------------------------
fit_predict_slice = function(train, test_slice, model_name, use_train_weights) {
  train = copy(train)
  test_slice = copy(test_slice)

  feature_mean = train[, lapply(.SD, mean, na.rm = TRUE), .SDcols = feature_cols]
  feature_sd = train[, lapply(.SD, sd, na.rm = TRUE), .SDcols = feature_cols]
  feature_sd[, (names(feature_sd)) := lapply(.SD, function(x) {
    fifelse(is.finite(x) & x > 0, x, 1)
  })]

  zcols = paste0(feature_cols, "_z")
  for (j in seq_along(feature_cols)) {
    col = feature_cols[j]
    zcol = zcols[j]

    train[, (zcol) := (get(col) - feature_mean[[col]]) / feature_sd[[col]]]
    test_slice[, (zcol) := (get(col) - feature_mean[[col]]) / feature_sd[[col]]]
  }

  fit_weights = NULL
  if (use_train_weights) {
    fit_weights = train_weights(nrow(train), TRAIN_WEIGHT_HALF_LIFE)
  }

  if (model_name == "linear_minmax") {
    f = as.formula(paste("target_ret ~", paste(zcols, collapse = " + ")))
  } else if (model_name == "linear_minmax_plus_hour") {
    f = as.formula(paste(
      "target_ret ~ factor(target_bar_time) +",
      paste(zcols, collapse = " + ")
    ))
  } else {
    stop("Unknown model: ", model_name)
  }

  fit = lm(f, data = train, weights = fit_weights)

  train[, pred := as.numeric(predict(fit, newdata = train))]
  test_slice[, pred := as.numeric(predict(fit, newdata = test_slice))]

  list(
    pred = test_slice[, .(date, trade_date, trade_type, target_ret, pred)],
    center = mean(train$pred, na.rm = TRUE),
    pred_sd = sd(train$pred, na.rm = TRUE)
  )
}

predict_oos = function(dt, model_name, retrain_period, use_train_weights) {
  split_i = floor(TRAIN_FRAC * nrow(dt))
  train0 = dt[seq_len(split_i)]
  test0 = dt[(split_i + 1L):nrow(dt)]

  if (retrain_period == "static") {
    fit_pred = fit_predict_slice(train0, test0, model_name, use_train_weights)
    pred_dt = fit_pred$pred
    center_value = fit_pred$center
    pred_sd_value = fit_pred$pred_sd
    pred_dt[, center := center_value]
    pred_dt[, pred_sd := pred_sd_value]
    pred_dt[, retrain_period_id := "static"]
    return(pred_dt)
  }

  if (retrain_period != "year") {
    stop("Only static and year retraining are implemented.")
  }

  test0[, retrain_period_id := as.character(as.integer(format(trade_date, "%Y")))]
  ids = sort(unique(test0$retrain_period_id))
  pieces = vector("list", length(ids))

  for (i in seq_along(ids)) {
    slice = test0[retrain_period_id == ids[i]]
    train_i = dt[trade_date < min(slice$trade_date)]
    fit_pred_i = fit_predict_slice(train_i, slice, model_name, use_train_weights)
    pred_i = fit_pred_i$pred
    center_i = fit_pred_i$center
    pred_sd_i = fit_pred_i$pred_sd
    pred_i[, center := center_i]
    pred_i[, pred_sd := pred_sd_i]
    pred_i[, retrain_period_id := ids[i]]
    pieces[[i]] = pred_i
  }

  rbindlist(pieces)
}

run_backtest = function(
    first_bar_mode,
    model_name,
    retrain_period,
    use_train_weights,
    position_sizing) {
  dt = make_model_dt(first_bar_mode)
  scale = annual_scale(dt)
  pred = predict_oos(dt, model_name, retrain_period, use_train_weights)

  pred[, position := position_from_prediction(pred, center, pred_sd, position_sizing)]
  pred[, strategy_ret := position * target_ret]
  pred[, benchmark_ret := target_ret]

  perf_strategy = perf_stats(pred, "strategy_ret", model_name, scale)
  perf_benchmark = perf_stats(pred, "benchmark_ret", "benchmark_spy", scale)

  perf_strategy[, `:=`(
    first_bar_mode = first_bar_mode,
    retrain_period = retrain_period,
    use_train_weights = use_train_weights,
    position_sizing = position_sizing,
    annual_scale = scale,
    long_share = mean(pred$position > 0, na.rm = TRUE),
    avg_abs_position = mean(abs(pred$position), na.rm = TRUE),
    max_abs_position = max(abs(pred$position), na.rm = TRUE),
    first_bar_trades = sum(pred$trade_type != "same_day_next_oc"),
    retrain_count = uniqueN(pred$retrain_period_id)
  )]

  perf_benchmark[, `:=`(
    first_bar_mode = first_bar_mode,
    retrain_period = retrain_period,
    use_train_weights = use_train_weights,
    position_sizing = "benchmark",
    annual_scale = scale,
    long_share = 1,
    avg_abs_position = 1,
    max_abs_position = 1,
    first_bar_trades = sum(pred$trade_type != "same_day_next_oc"),
    retrain_count = NA_integer_
  )]

  list(
    performance = rbindlist(list(perf_strategy, perf_benchmark), fill = TRUE),
    predictions = pred
  )
}


# Selected strategy -----------------------------------------------------------
selected = run_backtest(
  first_bar_mode = FIRST_BAR_MODE,
  model_name = STRATEGY_MODEL,
  retrain_period = RETRAIN_PERIOD,
  use_train_weights = USE_TRAIN_WEIGHTS,
  position_sizing = POSITION_SIZING
)

equity_curve = selected$predictions[, .(
  signal_date = date,
  trade_date,
  trade_type,
  strategy_model = STRATEGY_MODEL,
  first_bar_mode = FIRST_BAR_MODE,
  retrain_period = RETRAIN_PERIOD,
  use_train_weights = USE_TRAIN_WEIGHTS,
  position_sizing = POSITION_SIZING,
  position,
  strategy_ret,
  benchmark_ret
)]
equity_curve[, strategy_equity := cumprod(1 + strategy_ret)]
equity_curve[, benchmark_equity := cumprod(1 + benchmark_ret)]


# Grid comparison -------------------------------------------------------------
grid_performance = NULL
if (RUN_GRID) {
  grid = CJ(
    first_bar_mode = c("exclude", "next_open_close", "close_to_open"),
    model_name = valid_models,
    retrain_period = c("static", "year"),
    use_train_weights = c(FALSE, TRUE),
    position_sizing = valid_position_sizing
  )

  grid_results = vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    spec = grid[i]
    bt = run_backtest(
      first_bar_mode = spec$first_bar_mode,
      model_name = spec$model_name,
      retrain_period = spec$retrain_period,
      use_train_weights = spec$use_train_weights,
      position_sizing = spec$position_sizing
    )
    grid_results[[i]] = bt$performance[model != "benchmark_spy"]
  }

  grid_performance = rbindlist(grid_results, fill = TRUE)

  benchmark_rows = rbindlist(lapply(valid_first_bar_modes, function(mode) {
    bt = run_backtest(
      first_bar_mode = mode,
      model_name = "linear_minmax",
      retrain_period = "static",
      use_train_weights = FALSE,
      position_sizing = "sign"
    )
    bt$performance[model == "benchmark_spy"]
  }), fill = TRUE)

  grid_performance = rbindlist(list(grid_performance, benchmark_rows), fill = TRUE)
  setorder(grid_performance, -annualized_sharpe)
}


# Save outputs ----------------------------------------------------------------
fwrite(selected$performance, file.path(OUTPUT_DIR, "minmax_spy_performance.csv"))
fwrite(equity_curve, file.path(OUTPUT_DIR, "minmax_spy_equity_curve.csv"))
if (!is.null(grid_performance)) {
  fwrite(grid_performance, file.path(OUTPUT_DIR, "minmax_spy_grid_performance.csv"))
}

saveRDS(
  list(
    minmax_params = minmax_params,
    selected_performance = selected$performance,
    grid_performance = grid_performance,
    equity_curve = equity_curve,
    selected_options = list(
      strategy_model = STRATEGY_MODEL,
      first_bar_mode = FIRST_BAR_MODE,
      retrain_period = RETRAIN_PERIOD,
      use_train_weights = USE_TRAIN_WEIGHTS,
      position_sizing = POSITION_SIZING
    )
  ),
  file.path(OUTPUT_DIR, "minmax_spy_analysis.rds")
)

png(file.path(OUTPUT_DIR, "minmax_spy_equity_curve.png"), width = 1200, height = 700)
plot(
  equity_curve$trade_date,
  equity_curve$benchmark_equity,
  type = "l",
  col = "gray40",
  lwd = 2,
  xlab = "Date",
  ylab = "Equity",
  main = paste(
    "Minmax Strategy vs SPY Benchmark -",
    STRATEGY_MODEL,
    "-",
    RETRAIN_PERIOD,
    "-",
    POSITION_SIZING,
    "- first bar:",
    FIRST_BAR_MODE
  ),
  ylim = range(c(equity_curve$strategy_equity, equity_curve$benchmark_equity), na.rm = TRUE)
)
lines(equity_curve$trade_date, equity_curve$strategy_equity, col = "steelblue", lwd = 2)
graphics::legend(
  "topleft",
  legend = c("SPY benchmark", STRATEGY_MODEL),
  col = c("gray40", "steelblue"),
  lwd = 2,
  bty = "n"
)
dev.off()


# Console summary -------------------------------------------------------------
cat("\nSelected options\n")
print(list(
  strategy_model = STRATEGY_MODEL,
  first_bar_mode = FIRST_BAR_MODE,
  retrain_period = RETRAIN_PERIOD,
  use_train_weights = USE_TRAIN_WEIGHTS,
  position_sizing = POSITION_SIZING
))

cat("\nMINMAX params\n")
print(minmax_params)

cat("\nSelected performance\n")
print(selected$performance)

if (!is.null(grid_performance)) {
  cat("\nTop grid results\n")
  print(head(grid_performance, 20))
}

cat("\nFinal equity\n")
print(equity_curve[, .(
  strategy_model = data.table::last(strategy_model),
  first_bar_mode = data.table::last(first_bar_mode),
  retrain_period = data.table::last(retrain_period),
  use_train_weights = data.table::last(use_train_weights),
  position_sizing = data.table::last(position_sizing),
  strategy_equity = data.table::last(strategy_equity),
  benchmark_equity = data.table::last(benchmark_equity)
)])
