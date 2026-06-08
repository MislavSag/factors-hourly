suppressPackageStartupMessages({
  library(data.table)
  library(finutils)
  library(qlcal)
  library(lubridate)
  library(roll)
  library(arrow)
  library(dplyr)
  library(PerformanceAnalytics)
})


# Setup -----------------------------------------------------------------------
setCalendar("UnitedStates/NYSE")
PATH_LEAN = "C:/Users/Mislav/qc_snp/data"

OUTPUT_DIR = file.path("data", "analysis")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

TOP_N = as.integer(Sys.getenv("TOP_N", unset = "500"))
TOP_LABEL = paste0("top", TOP_N)
LIQUIDITY_START = as.Date("2024-01-01")
LIQUIDITY_END = as.Date("2025-01-01")
FIRST_DATE = as.Date("2010-01-01")

TRAIN_FRAC = 0.70
STRATEGY_MODEL = "linear_minmax"
FIRST_BAR_MODE = "exclude"
RETRAIN_PERIOD = "year"
USE_TRAIN_WEIGHTS = FALSE
POSITION_SIZING = "sign"
TRAIN_WEIGHT_HALF_LIFE = 252L * 6L * 3L


# Top-N universe --------------------------------------------------------------
# Static top-N universe selected by average daily dollar volume in 2024.
# This avoids using 2025 information when judging 2025 strategy performance.
daily_ds = arrow::open_dataset(file.path(PATH_LEAN, "all_stocks_daily"))

topn_liquidity = daily_ds %>%
  dplyr::filter(
    .data[["Inv Vehicle"]] == FALSE,
    Date >= LIQUIDITY_START,
    Date < LIQUIDITY_END
  ) %>%
  dplyr::mutate(dollar_volume = Close * Volume) %>%
  dplyr::group_by(Symbol) %>%
  dplyr::summarise(
    mean_dollar_volume = mean(dollar_volume, na.rm = TRUE),
    obs = dplyr::n()
  ) %>%
  dplyr::filter(obs >= 125) %>%
  dplyr::arrange(dplyr::desc(mean_dollar_volume)) %>%
  utils::head(TOP_N + 100L) %>%
  dplyr::collect()

setDT(topn_liquidity)
setnames(topn_liquidity, "Symbol", "symbol")
topn_liquidity = topn_liquidity[!grepl("\\.[0-9]+$", symbol)]
topn_liquidity = head(topn_liquidity, TOP_N)
topn_symbols = tolower(topn_liquidity$symbol)

fwrite(topn_liquidity, file.path(OUTPUT_DIR, paste0("minmax_", TOP_LABEL, "_liquidity_2024.csv")))


# Prices data -----------------------------------------------------------------
prices = qc_hour_parquet(
  file_path = file.path(PATH_LEAN, "all_stocks_hour"),
  etfs = FALSE,
  symbols = topn_symbols,
  first_date = FIRST_DATE,
  min_obs = 7 * 125,
  duplicates = "fast",
  add_dv_rank = FALSE,
  min_cross_section_n = 300
)
setDT(prices)


# MINMAX ----------------------------------------------------------------------
TARGET_EFF_N = 2500
TARGET_TAIL_EFF_OBS = 25
TARGET_CS_EXCEEDERS = 10

minmax_cs_n = prices[, .(n = .N), by = date]
MEDIAN_CROSS_SECTION_N = median(minmax_cs_n$n, na.rm = TRUE)

PROB_FROM_TS_STABILITY = 1 - TARGET_TAIL_EFF_OBS / TARGET_EFF_N
PROB_FROM_CS_STABILITY = 1 - TARGET_CS_EXCEEDERS / MEDIAN_CROSS_SECTION_N

MINMAX_PROB = min(PROB_FROM_TS_STABILITY, PROB_FROM_CS_STABILITY)
MINMAX_PROB = max(min(MINMAX_PROB, 0.99), 0.95)

MINMAX_ALPHA = (TARGET_EFF_N - 1) / (TARGET_EFF_N + 1)
MINMAX_HALF_LIFE = ceiling(log(0.5) / log(MINMAX_ALPHA))

MINMAX_WIDTH = ceiling(6 * MINMAX_HALF_LIFE)
MINMAX_MIN_OBS = 252L * 6L

MINMAX_AGE = rev(seq_len(MINMAX_WIDTH) - 1L)
MINMAX_WEIGHTS = 0.5 ^ (MINMAX_AGE / MINMAX_HALF_LIFE)
MINMAX_WEIGHTS = MINMAX_WEIGHTS / mean(MINMAX_WEIGHTS)

MINMAX_EFF_N = sum(MINMAX_WEIGHTS)^2 / sum(MINMAX_WEIGHTS^2)
MINMAX_EFF_TAIL_OBS = MINMAX_EFF_N * (1 - MINMAX_PROB)
MINMAX_EXPECTED_CS_EXCEEDERS = MEDIAN_CROSS_SECTION_N * (1 - MINMAX_PROB)

setorder(prices, symbol, date)

prices[, minmax_upper_q := roll::roll_quantile(
  returns_oc,
  width = MINMAX_WIDTH,
  weights = MINMAX_WEIGHTS,
  p = MINMAX_PROB,
  min_obs = MINMAX_MIN_OBS,
  online = FALSE
), by = symbol]

prices[, minmax_lower_q := roll::roll_quantile(
  returns_oc,
  width = MINMAX_WIDTH,
  weights = MINMAX_WEIGHTS,
  p = 1 - MINMAX_PROB,
  min_obs = MINMAX_MIN_OBS,
  online = FALSE
), by = symbol]

prices[, minmax_up_excess := pmax(returns_oc - shift(minmax_upper_q), 0), by = symbol]
prices[, minmax_down_excess := pmax(shift(minmax_lower_q) - returns_oc, 0), by = symbol]

minmax_indicators = prices[, {
  up_n = sum(minmax_up_excess > 0, na.rm = TRUE)
  down_n = sum(minmax_down_excess > 0, na.rm = TRUE)

  up_sum = sum(minmax_up_excess, na.rm = TRUE)
  down_sum = sum(minmax_down_excess, na.rm = TRUE)

  up_count = up_n / .N
  down_count = down_n / .N

  up_severity = fifelse(up_n > 0, up_sum / up_n, 0)
  down_severity = fifelse(down_n > 0, down_sum / down_n, 0)

  .(
    trading_day = data.table::first(trading_day),
    bar_time = data.table::first(bar_time),
    is_first_bar = any(is_first_bar),
    n = .N,
    up_count = up_count,
    down_count = down_count,
    up_severity = up_severity,
    down_severity = down_severity,
    count_imbalance = fifelse(
      up_count + down_count > 0,
      (up_count - down_count) / (up_count + down_count),
      0
    ),
    severity_imbalance = fifelse(
      up_severity + down_severity > 0,
      (up_severity - down_severity) / (up_severity + down_severity),
      0
    ),
    cs_dispersion = sd(returns_oc, na.rm = TRUE)
  )
}, by = date]

minmax_params = data.table(
  universe = paste0(TOP_LABEL, "_avg_daily_dollar_volume_2024"),
  first_date = FIRST_DATE,
  liquidity_start = LIQUIDITY_START,
  liquidity_end = LIQUIDITY_END,
  target_eff_n = TARGET_EFF_N,
  target_tail_eff_obs = TARGET_TAIL_EFF_OBS,
  target_cs_exceeders = TARGET_CS_EXCEEDERS,
  median_cross_section_n = MEDIAN_CROSS_SECTION_N,
  minmax_prob = MINMAX_PROB,
  minmax_half_life = MINMAX_HALF_LIFE,
  minmax_width = MINMAX_WIDTH,
  minmax_min_obs = MINMAX_MIN_OBS,
  minmax_eff_n = MINMAX_EFF_N,
  minmax_eff_tail_obs = MINMAX_EFF_TAIL_OBS,
  minmax_expected_cs_exceeders = MINMAX_EXPECTED_CS_EXCEEDERS
)

saveRDS(minmax_indicators, file.path(OUTPUT_DIR, paste0("minmax_", TOP_LABEL, "_indicators.rds")))
saveRDS(minmax_params, file.path(OUTPUT_DIR, paste0("minmax_", TOP_LABEL, "_params.rds")))


# Strategy helpers ------------------------------------------------------------
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


# SPY target data -------------------------------------------------------------
spy = qc_hour_parquet(
  file_path = file.path(PATH_LEAN, "all_stocks_hour"),
  symbols = "spy",
  first_date = FIRST_DATE,
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


# Selected top-N strategy -----------------------------------------------------
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

perf_2025_strategy = perf_stats(
  selected$predictions[format(trade_date, "%Y") == "2025"],
  "strategy_ret",
  STRATEGY_MODEL,
  selected$performance[1, annual_scale]
)
perf_2025_benchmark = perf_stats(
  selected$predictions[format(trade_date, "%Y") == "2025"],
  "benchmark_ret",
  "benchmark_spy",
  selected$performance[1, annual_scale]
)

performance_2025 = rbindlist(list(perf_2025_strategy, perf_2025_benchmark), fill = TRUE)
performance_2025[, `:=`(
  period = "2025",
  first_bar_mode = FIRST_BAR_MODE,
  retrain_period = RETRAIN_PERIOD,
  use_train_weights = USE_TRAIN_WEIGHTS,
  position_sizing = c(POSITION_SIZING, "benchmark"),
  annual_scale = selected$performance[1, annual_scale],
  long_share = c(
    mean(selected$predictions[format(trade_date, "%Y") == "2025"]$position > 0, na.rm = TRUE),
    1
  )
)]

selected$performance[, period := "full_oos"]
setcolorder(selected$performance, "period")
setcolorder(performance_2025, "period")

fwrite(selected$performance, file.path(OUTPUT_DIR, paste0("minmax_spy_", TOP_LABEL, "_performance.csv")))
fwrite(performance_2025, file.path(OUTPUT_DIR, paste0("minmax_spy_", TOP_LABEL, "_2025_performance.csv")))
fwrite(equity_curve, file.path(OUTPUT_DIR, paste0("minmax_spy_", TOP_LABEL, "_equity_curve.csv")))

saveRDS(
  list(
    topn_liquidity = topn_liquidity,
    minmax_params = minmax_params,
    performance = selected$performance,
    performance_2025 = performance_2025,
    equity_curve = equity_curve
  ),
  file.path(OUTPUT_DIR, paste0("minmax_spy_", TOP_LABEL, "_analysis.rds"))
)

print(minmax_params)
print(selected$performance)
print(performance_2025)
