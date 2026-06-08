suppressPackageStartupMessages({
  library(data.table)
})

OUTPUT_PROJECT = "C:/Users/Mislav/qcworkspace/Minmax Local Indicators SPY"
dir.create(OUTPUT_PROJECT, recursive = TRUE, showWarnings = FALSE)

eq = fread(file.path("data", "analysis", "minmax_spy_equity_curve.csv"))
eq[, signal_date := as.POSIXct(signal_date, tz = "UTC")]
eq[, trade_date := as.POSIXct(trade_date, tz = "UTC")]

signals = eq[, .(
  signal_time = format(signal_date, tz = "America/New_York", usetz = FALSE),
  trade_time = format(trade_date, tz = "America/New_York", usetz = FALSE),
  position,
  strategy_ret,
  benchmark_ret
)]

fwrite(signals, file.path(OUTPUT_PROJECT, "minmax_local_signals.csv"))

perf_stats = function(ret) {
  ret = ret[is.finite(ret)]
  n = length(ret)
  scale = 1512
  equity = cumprod(1 + ret)

  data.table(
    n = n,
    cum_return = tail(equity, 1) - 1,
    ann_return = tail(equity, 1)^(scale / n) - 1,
    ann_sd = sd(ret) * sqrt(scale),
    sharpe = mean(ret) / sd(ret) * sqrt(scale),
    max_dd = max(1 - equity / cummax(equity)),
    hit_rate = mean(ret > 0),
    mean_bp = mean(ret) * 10000
  )
}

eq_2025 = eq[format(trade_date, "%Y") == "2025"]
expected_2025 = rbindlist(list(
  cbind(
    label = "local_strategy_2025",
    perf_stats(eq_2025$strategy_ret),
    long_share = mean(eq_2025$position > 0)
  ),
  cbind(
    label = "local_benchmark_2025",
    perf_stats(eq_2025$benchmark_ret),
    long_share = 1
  )
), fill = TRUE)

fwrite(expected_2025, file.path(OUTPUT_PROJECT, "local_expected_2025.csv"))

print(head(signals))
print(expected_2025)
cat("rows=", nrow(signals), "\n")
