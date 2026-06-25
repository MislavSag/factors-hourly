suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

env_chr = function(name, default) {
  value = Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

compound_return = function(x) {
  x = as.numeric(x)
  x = x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  prod(1 + x) - 1
}

parse_utc_z = function(x) {
  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    return(as.POSIXct(x, tz = "UTC"))
  }
  x_chr = as.character(x)
  out = suppressWarnings(as.POSIXct(x_chr, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  missing = is.na(out)
  if (any(missing)) {
    out[missing] = suppressWarnings(as.POSIXct(x_chr[missing], format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  }
  out
}

parse_ny = function(x) {
  suppressWarnings(as.POSIXct(as.character(x), format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))
}

ny_key_from_instant = function(x, shift_hours = 0L) {
  shifted = x + shift_hours * 3600
  local_text = format(shifted, tz = "America/New_York", usetz = FALSE)
  local_time = as.POSIXct(local_text, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
  data.table(
    datetime_ny = local_time,
    trading_day = as.IDate(format(local_time, "%Y-%m-%d")),
    bar_time = format(local_time, "%H:%M:%S")
  )
}

key_from_ny_local = function(x) {
  data.table(
    datetime_ny = x,
    trading_day = as.IDate(format(x, "%Y-%m-%d")),
    bar_time = format(x, "%H:%M:%S")
  )
}

hour_floor = function(x) {
  as.POSIXct(floor(as.numeric(x) / 3600) * 3600, origin = "1970-01-01", tz = "America/New_York")
}

hour_ceiling = function(x) {
  as.POSIXct(ceiling(as.numeric(x) / 3600) * 3600, origin = "1970-01-01", tz = "America/New_York")
}

series_summary = function(dt, value_cols, group_name) {
  rbindlist(lapply(value_cols, function(col) {
    x = dt[[col]]
    x = x[is.finite(x)]
    data.table(
      sample = group_name,
      series = col,
      n = length(x),
      first_day = if (length(x)) min(dt[is.finite(get(col)), trading_day]) else as.IDate(NA),
      last_day = if (length(x)) max(dt[is.finite(get(col)), trading_day]) else as.IDate(NA),
      mean_hour = mean(x),
      sd_hour = sd(x),
      cumulative_return = compound_return(x),
      min_hour = min(x),
      max_hour = max(x)
    )
  }), use.names = TRUE)
}

pair_summary = function(dt, lhs, rhs, group_name) {
  ok = is.finite(dt[[lhs]]) & is.finite(dt[[rhs]])
  d = dt[ok]
  if (!nrow(d)) {
    return(data.table(
      sample = group_name, lhs = lhs, rhs = rhs, n = 0L,
      corr = NA_real_, beta_lhs_on_rhs = NA_real_, mean_diff = NA_real_,
      sd_diff = NA_real_, rmse = NA_real_, cum_lhs = NA_real_, cum_rhs = NA_real_,
      cum_diff = NA_real_
    ))
  }
  x = d[[rhs]]
  y = d[[lhs]]
  beta = if (stats::var(x) > 0) stats::cov(y, x) / stats::var(x) else NA_real_
  diff = y - x
  data.table(
    sample = group_name,
    lhs = lhs,
    rhs = rhs,
    n = nrow(d),
    corr = stats::cor(y, x),
    beta_lhs_on_rhs = beta,
    mean_diff = mean(diff),
    sd_diff = sd(diff),
    rmse = sqrt(mean(diff^2)),
    cum_lhs = compound_return(y),
    cum_rhs = compound_return(x),
    cum_diff = compound_return(y) - compound_return(x)
  )
}

read_our_market = function(file, out_col, shift_hours = 1L) {
  x = fread(file, showProgress = FALSE)
  if (!"date" %in% names(x) || !"MKT_MARKET_CAP_W" %in% names(x)) {
    stop(sprintf("%s must contain date and MKT_MARKET_CAP_W.", file))
  }
  key = ny_key_from_instant(parse_utc_z(x$date), shift_hours = shift_hours)
  x = cbind(key, x[, .(
    value = as.numeric(MKT_MARKET_CAP_W),
    n_symbols = if ("n_symbols" %in% names(x)) as.integer(n_symbols) else NA_integer_,
    sum_w = if ("sum_w" %in% names(x)) as.numeric(sum_w) else NA_real_
  )])
  setnames(x, "value", out_col)
  x
}

read_aleti_market = function(file) {
  x = fread(file, showProgress = FALSE)
  if (!"datetime" %in% names(x) || !"ff__mkt_hour" %in% names(x)) {
    stop(sprintf("%s must contain datetime and ff__mkt_hour.", file))
  }
  key = key_from_ny_local(parse_ny(x$datetime))
  cbind(key, x[, .(aleti_mkt = as.numeric(ff__mkt_hour))])
}

aggregate_spy_variant = function(spy, ret_col, label_col, out_col) {
  x = spy[is.finite(get(ret_col)) & !is.na(get(label_col))]
  out = x[, .(value = compound_return(get(ret_col))), by = .(datetime_ny = get(label_col))]
  out[, c("trading_day", "bar_time") := .(
    as.IDate(format(datetime_ny, "%Y-%m-%d")),
    format(datetime_ny, "%H:%M:%S")
  )]
  setnames(out, "value", out_col)
  out
}

drop_datetime_col = function(x) {
  if ("datetime_ny" %in% names(x)) x[, !"datetime_ny"] else x
}

read_spy_hourly = function(file) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("The arrow package is required to read SPY parquet data.")
  }
  spy = as.data.table(arrow::read_parquet(file))
  if (!all(c("date", "open", "close") %in% names(spy))) {
    stop(sprintf("%s must contain date, open, and close.", file))
  }
  spy[, datetime_ny := parse_ny(date)]
  spy[, trading_day := as.IDate(format(datetime_ny, "%Y-%m-%d"))]
  setorder(spy, trading_day, datetime_ny)
  spy[, minute_index := seq_len(.N), by = trading_day]
  spy[, ret_cc := close / shift(close) - 1, by = trading_day]
  spy[, ret_oc := close / open - 1]
  spy[minute_index == 1L, ret_cc := NA_real_]
  spy[, label_ceiling := hour_ceiling(datetime_ny)]
  spy[, label_floor_plus1 := hour_floor(datetime_ny) + 3600]
  spy[, label_floor := hour_floor(datetime_ny)]

  variants = list(
    aggregate_spy_variant(copy(spy), "ret_cc", "label_ceiling", "spy_cc_ceiling"),
    aggregate_spy_variant(copy(spy), "ret_cc", "label_floor_plus1", "spy_cc_floor_plus1"),
    aggregate_spy_variant(copy(spy), "ret_cc", "label_floor", "spy_cc_floor"),
    aggregate_spy_variant(copy(spy), "ret_oc", "label_floor_plus1", "spy_oc_floor_plus1")
  )
  out = Reduce(function(a, b) {
    merge(
      drop_datetime_col(a),
      drop_datetime_col(b),
      by = c("trading_day", "bar_time"),
      all = TRUE
    )
  }, variants)
  out[, datetime_ny := parse_ny(paste(trading_day, bar_time))]
  out
}

make_cumulative = function(dt, value_cols, group_name) {
  x = melt(
    dt[, c("datetime_ny", "trading_day", "bar_time", value_cols), with = FALSE],
    id.vars = c("datetime_ny", "trading_day", "bar_time"),
    variable.name = "series",
    value.name = "return"
  )
  x = x[is.finite(return)]
  setorder(x, series, trading_day, bar_time)
  x[, cumulative_return := cumprod(1 + return) - 1, by = series]
  x[, sample := group_name]
  x
}

PATH_OUR_OC = env_chr("PATH_OUR_OC", file.path("factor_returns_market_cap_only", "factor_returns_wide.csv"))
PATH_OUR_INTRADAY = env_chr("PATH_OUR_INTRADAY", file.path("factor_returns_market_cap_intraday_return", "factor_returns_wide.csv"))
PATH_ALETI = env_chr("PATH_ALETI", file.path("factor_returns_simple", "aleti_hourly_from_minute.csv"))
PATH_SPY = env_chr("PATH_SPY", "F:/data/equity/us/spy_minute_fmp.parquet")
PATH_OUT = env_chr("PATH_OUT", file.path("diagnostics", "market_gap"))

dir.create(PATH_OUT, recursive = TRUE, showWarnings = FALSE)

our_oc = read_our_market(PATH_OUR_OC, "our_mcap_oc", shift_hours = 1L)
our_intraday = read_our_market(PATH_OUR_INTRADAY, "our_mcap_intraday", shift_hours = 1L)
aleti = read_aleti_market(PATH_ALETI)
spy = read_spy_hourly(PATH_SPY)

dt = Reduce(function(a, b) merge(a, b, by = c("trading_day", "bar_time"), all = TRUE), list(
  aleti[, .(trading_day, bar_time, aleti_mkt)],
  our_oc[, .(trading_day, bar_time, our_mcap_oc, n_symbols_oc = n_symbols, sum_w_oc = sum_w)],
  our_intraday[, .(trading_day, bar_time, our_mcap_intraday, n_symbols_intraday = n_symbols, sum_w_intraday = sum_w)],
  spy[, !"datetime_ny"]
))
dt[, datetime_ny := parse_ny(paste(trading_day, bar_time))]
setcolorder(dt, c("datetime_ny", setdiff(names(dt), "datetime_ny")))
setorder(dt, trading_day, bar_time)
dt[, year := as.integer(format(trading_day, "%Y"))]

value_cols = c(
  "aleti_mkt",
  "our_mcap_oc",
  "our_mcap_intraday",
  "spy_cc_ceiling",
  "spy_cc_floor_plus1",
  "spy_cc_floor",
  "spy_oc_floor_plus1"
)

samples = list(
  all_common_our_aleti = dt[is.finite(aleti_mkt) & is.finite(our_mcap_oc) & is.finite(our_mcap_intraday)],
  no_10bar_common_our_aleti = dt[bar_time != "10:00:00" & is.finite(aleti_mkt) & is.finite(our_mcap_oc) & is.finite(our_mcap_intraday)],
  no_first_common_our_aleti = dt[bar_time != "11:00:00" & is.finite(aleti_mkt) & is.finite(our_mcap_oc) & is.finite(our_mcap_intraday)],
  spy_period_common = dt[is.finite(aleti_mkt) & is.finite(our_mcap_oc) & is.finite(our_mcap_intraday) & is.finite(spy_cc_ceiling)],
  spy_period_no_10bar = dt[bar_time != "10:00:00" & is.finite(aleti_mkt) & is.finite(our_mcap_oc) & is.finite(our_mcap_intraday) & is.finite(spy_cc_ceiling)],
  spy_period_no_first = dt[bar_time != "11:00:00" & is.finite(aleti_mkt) & is.finite(our_mcap_oc) & is.finite(our_mcap_intraday) & is.finite(spy_cc_ceiling)]
)

summaries = rbindlist(lapply(names(samples), function(nm) {
  present_cols = value_cols[value_cols %in% names(samples[[nm]])]
  series_summary(samples[[nm]], present_cols, nm)
}), use.names = TRUE, fill = TRUE)

main_pairs = list(
  c("our_mcap_oc", "aleti_mkt"),
  c("our_mcap_intraday", "aleti_mkt"),
  c("our_mcap_oc", "spy_cc_ceiling"),
  c("our_mcap_intraday", "spy_cc_ceiling"),
  c("aleti_mkt", "spy_cc_ceiling"),
  c("aleti_mkt", "spy_cc_floor_plus1"),
  c("aleti_mkt", "spy_cc_floor"),
  c("aleti_mkt", "spy_oc_floor_plus1")
)
pairs = rbindlist(lapply(names(samples), function(nm) {
  rbindlist(lapply(main_pairs, function(pair) {
    if (!all(pair %in% names(samples[[nm]]))) return(NULL)
    pair_summary(samples[[nm]], pair[1], pair[2], nm)
  }), use.names = TRUE, fill = TRUE)
}), use.names = TRUE, fill = TRUE)

outlier_tests = rbindlist(lapply(c(0, 0.001, 0.005, 0.01, 0.05), function(trim) {
  base = samples$all_common_our_aleti[
    is.finite(aleti_mkt) & is.finite(our_mcap_intraday)
  ][, residual := our_mcap_intraday - aleti_mkt]
  if (trim > 0) {
    cutoff = stats::quantile(abs(base$residual), probs = 1 - trim, na.rm = TRUE, type = 8)
    base = base[abs(residual) <= cutoff]
  }
  x = pair_summary(base, "our_mcap_intraday", "aleti_mkt", "trim_residual")
  x[, trim_abs_residual_top_share := trim]
  x
}), use.names = TRUE, fill = TRUE)

coverage_quantile_tests = rbindlist(lapply(c(0, 0.05, 0.10, 0.25, 0.50), function(drop_share) {
  base = samples$all_common_our_aleti[is.finite(n_symbols_intraday)]
  if (drop_share > 0) {
    cutoff = stats::quantile(base$n_symbols_intraday, probs = drop_share, na.rm = TRUE, type = 8)
    base = base[n_symbols_intraday >= cutoff]
  }
  x = pair_summary(base, "our_mcap_intraday", "aleti_mkt", "coverage_filter")
  x[, drop_low_n_symbols_share := drop_share]
  x[, min_n_symbols_kept := min(base$n_symbols_intraday, na.rm = TRUE)]
  x
}), use.names = TRUE, fill = TRUE)

yearly = rbindlist(lapply(value_cols, function(col) {
  if (!col %in% names(dt)) return(NULL)
  dt[is.finite(get(col)), .(
    n_hours = .N,
    cumulative_return = compound_return(get(col)),
    mean_hour = mean(get(col)),
    sd_hour = sd(get(col)),
    mean_n_symbols = mean(n_symbols_intraday, na.rm = TRUE)
  ), by = year][, series := col]
}), use.names = TRUE, fill = TRUE)
setcolorder(yearly, c("year", "series", setdiff(names(yearly), c("year", "series"))))

top_residuals = samples$all_common_our_aleti[
  is.finite(aleti_mkt) & is.finite(our_mcap_intraday),
  .(
    trading_day, bar_time, aleti_mkt, our_mcap_oc, our_mcap_intraday,
    residual_intraday = our_mcap_intraday - aleti_mkt,
    n_symbols_intraday, sum_w_intraday
  )
][order(-abs(residual_intraday))][1:100]

best_spy = pairs[
  grepl("^spy_period", sample) & lhs == "aleti_mkt" & grepl("^spy_", rhs)
][order(sample, -corr)]

fwrite(dt, file.path(PATH_OUT, "matched_hourly_market.csv"))
fwrite(summaries, file.path(PATH_OUT, "series_summary.csv"))
fwrite(pairs, file.path(PATH_OUT, "pair_summary.csv"))
fwrite(outlier_tests, file.path(PATH_OUT, "outlier_trim_summary.csv"))
fwrite(coverage_quantile_tests, file.path(PATH_OUT, "coverage_filter_summary.csv"))
fwrite(yearly, file.path(PATH_OUT, "yearly_returns.csv"))
fwrite(top_residuals, file.path(PATH_OUT, "top_residual_hours.csv"))
fwrite(best_spy, file.path(PATH_OUT, "best_spy_alignment.csv"))

plot_sample = samples$spy_period_common
plot_cols = c("aleti_mkt", "our_mcap_oc", "our_mcap_intraday", "spy_cc_ceiling")
cum = make_cumulative(plot_sample, plot_cols, "spy_period_common")
ggplot(cum, aes(datetime_ny, cumulative_return, color = series)) +
  geom_line(linewidth = 0.45) +
  labs(x = NULL, y = "Cumulative return", color = NULL) +
  theme_minimal(base_size = 11)
ggsave(file.path(PATH_OUT, "cumulative_market_common_spy_period.png"), width = 11, height = 6, dpi = 150)

plot_no10 = samples$spy_period_no_10bar
cum_no10 = make_cumulative(plot_no10, plot_cols, "spy_period_no_10bar")
ggplot(cum_no10, aes(datetime_ny, cumulative_return, color = series)) +
  geom_line(linewidth = 0.45) +
  labs(x = NULL, y = "Cumulative return", color = NULL) +
  theme_minimal(base_size = 11)
ggsave(file.path(PATH_OUT, "cumulative_market_common_spy_period_no_10bar.png"), width = 11, height = 6, dpi = 150)

yr_plot = yearly[
  series %in% c("aleti_mkt", "our_mcap_oc", "our_mcap_intraday", "spy_cc_ceiling") &
    year >= 2003 & year <= 2020
]
ggplot(yr_plot, aes(year, cumulative_return, color = series)) +
  geom_hline(yintercept = 0, color = "grey75") +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.2) +
  labs(x = NULL, y = "Yearly compounded return", color = NULL) +
  theme_minimal(base_size = 11)
ggsave(file.path(PATH_OUT, "yearly_market_returns.png"), width = 11, height = 6, dpi = 150)

scatter = samples$all_common_our_aleti[
  is.finite(n_symbols_intraday) & is.finite(aleti_mkt) & is.finite(our_mcap_intraday),
  .(n_symbols_intraday, residual = our_mcap_intraday - aleti_mkt, trading_day)
]
ggplot(scatter, aes(n_symbols_intraday, residual)) +
  geom_point(alpha = 0.08, size = 0.3) +
  geom_hline(yintercept = 0, color = "grey65") +
  labs(x = "N symbols in our market-cap universe", y = "Our intraday market - Aleti market") +
  theme_minimal(base_size = 11)
ggsave(file.path(PATH_OUT, "residual_vs_coverage.png"), width = 9, height = 5, dpi = 150)

cat("Wrote market gap diagnostics to:", normalizePath(PATH_OUT, winslash = "/"), "\n")
cat("Main pair summary:\n")
print(pairs[sample %in% c("all_common_our_aleti", "spy_period_common")])
cat("Outlier trim summary:\n")
print(outlier_tests)
cat("Coverage filter summary:\n")
print(coverage_quantile_tests)
