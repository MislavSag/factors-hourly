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
  parsed = suppressWarnings(as.integer(value))
  if (is.na(parsed) || parsed < 0L) {
    stop(sprintf("%s must be a non-negative integer.", name))
  }
  parsed
}

env_num = function(name, default) {
  value = env_chr(name, "")
  if (!nzchar(value)) return(default)
  parsed = suppressWarnings(as.numeric(value))
  if (!is.finite(parsed)) stop(sprintf("%s must be numeric.", name))
  parsed
}

env_bool = function(name, default) {
  value = env_chr(name, if (default) "1" else "0")
  value %in% c("1", "true", "TRUE", "yes", "YES")
}

parse_feature_list = function(value) {
  out = trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
  out[nzchar(out)]
}

safe_feature_name = function(x) {
  gsub("[^A-Za-z0-9_.-]+", "_", x)
}

make_factor_name = function(prefix, value) {
  paste0(prefix, gsub("[^A-Za-z0-9]+", "_", toupper(value)))
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
      stop("Reading parquet requires the arrow package.")
    }
    as.data.table(arrow::read_parquet(file))
  } else {
    fread(file, showProgress = FALSE)
  }
}

compound_return = function(x) {
  x = as.numeric(x)
  x = x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  prod(1 + x) - 1
}

weighted_mean = function(x, w) {
  ok = is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

first_finite = function(x) {
  x = x[is.finite(x)]
  if (length(x)) x[[1L]] else NA_real_
}

last_finite = function(x) {
  x = x[is.finite(x)]
  if (length(x)) x[[length(x)]] else NA_real_
}

min_finite = function(x) {
  x = x[is.finite(x)]
  if (length(x)) min(x) else NA_real_
}

max_finite = function(x) {
  x = x[is.finite(x)]
  if (length(x)) max(x) else NA_real_
}

empty_factor_row = function() {
  list(
    n = 0L,
    n_long = 0L,
    n_short = 0L,
    long_ret = NA_real_,
    short_ret = NA_real_,
    factor_ret = NA_real_,
    signal_q_low = NA_real_,
    signal_q_high = NA_real_
  )
}

feature_window = function(feature, prefix, suffix = "d") {
  pattern = sprintf("^%s([0-9]+)%s$", prefix, suffix)
  if (!grepl(pattern, feature)) return(NA_integer_)
  as.integer(sub(pattern, "\\1", feature))
}

threads = env_int("DATA_TABLE_THREADS", 0L)
if (threads > 0L) setDTthreads(threads)

PATH_PRICES = env_chr("PATH_PRICES", "prices_factors_hour")
PATH_FACTORS = env_chr("PATH_FACTORS", "factor_returns_daily_characteristics")
PATH_MARKET_CAP = env_chr(
  "PATH_MARKET_CAP",
  file.path("data", "findata_market_cap", "daily_market_cap_clean_sane_hourly.rds")
)
PATH_INDUSTRY_MAP = env_chr("PATH_INDUSTRY_MAP", "")
FEATURES = parse_feature_list(env_chr(
  "DAILY_FEATURES",
  paste(
    c(
      "MKT_MARKET_CAP_W",
      "zero_trades_21d",
      "zero_trades_126d",
      "zero_trades_252d",
      "turnover_proxy_126d",
      "turnover_var_proxy_126d",
      "rvol_21d",
      "ivol_capm_252d",
      "beta_tailrisk_proxy_252d",
      "mom_21d",
      "mom_126d",
      "dollar_vol_126d",
      "amihud_21d",
      "hl_range_21d"
    ),
    collapse = ","
  )
))

RETURN_COL = env_chr("DAILY_RETURN_COL", env_chr("SIMPLE_RETURN_COL", "returns_oc"))
DROP_FIRST_BAR = env_bool("DAILY_DROP_FIRST_BAR", env_bool("SIMPLE_DROP_FIRST_BAR", TRUE))
MARKET_CAP_LAG_DAYS = env_int("DAILY_MARKET_CAP_LAG_DAYS", env_int("SIMPLE_MARKET_CAP_LAG_DAYS", 1L))
TAIL_PROB = env_num("DAILY_TAIL_PROB", env_num("SIMPLE_TAIL_PROB", 0.10))
MIN_LEG_N = env_int("DAILY_MIN_LEG_N", env_int("SIMPLE_MIN_LEG_N", 10L))
MAX_ABS_RETURN = env_num("DAILY_MAX_ABS_RETURN", env_num("SIMPLE_MAX_ABS_RETURN", 0.20))
ZERO_TRADE_MODE = env_chr("ZERO_TRADE_MODE", "volume_or_flat_close")
ZERO_TRADE_CLOSE_EPS = env_num("ZERO_TRADE_CLOSE_EPS", 1e-10)
ZERO_TRADE_MIN_FLAT_BARS = env_int("ZERO_TRADE_MIN_FLAT_BARS", 2L)
MIN_SYMBOL_ROWS = env_int("DAILY_MIN_SYMBOL_ROWS", env_int("SIMPLE_MIN_SYMBOL_ROWS", 50L))
MAX_FILES = env_int("DAILY_MAX_FILES", env_int("SIMPLE_MAX_FILES", 0L))
MARKET_TAIL_Z = env_num("DAILY_MARKET_TAIL_Z", env_num("SIMPLE_MARKET_TAIL_Z", -1.0))
WRITE_DAILY_PANEL = env_bool("WRITE_DAILY_PANEL", FALSE)
FORCE = env_bool("FORCE", FALSE)

if (TAIL_PROB <= 0 || TAIL_PROB >= 0.5) {
  stop("DAILY_TAIL_PROB/SIMPLE_TAIL_PROB must be in (0, 0.5).")
}
if (!ZERO_TRADE_MODE %in% c("volume", "flat_close", "volume_or_flat_close")) {
  stop("ZERO_TRADE_MODE must be one of: volume, flat_close, volume_or_flat_close.")
}
if (!file.exists(PATH_MARKET_CAP)) {
  stop(sprintf("PATH_MARKET_CAP does not exist: %s", PATH_MARKET_CAP))
}

dir.create(PATH_FACTORS, recursive = TRUE, showWarnings = FALSE)
out_long = file.path(PATH_FACTORS, "factor_returns_long.csv")
out_wide = file.path(PATH_FACTORS, "factor_returns_wide.csv")
out_summary = file.path(PATH_FACTORS, "factor_returns_summary.csv")
if (file.exists(out_long) && file.exists(out_wide) && !FORCE) {
  cat(sprintf("Skipping existing output in %s. Set FORCE=1 to recompute.\n", PATH_FACTORS))
  quit(save = "no", status = 0L, runLast = FALSE)
}

price_files = sort(list.files(PATH_PRICES, pattern = "\\.csv$", full.names = TRUE))
if (!length(price_files)) {
  stop(sprintf("No CSV files found in PATH_PRICES: %s", PATH_PRICES))
}
if (MAX_FILES > 0L) {
  price_files = head(price_files, MAX_FILES)
}

read_header = function(file) names(fread(file, nrows = 0L, showProgress = FALSE))

read_symbol_daily = function(file) {
  header = read_header(file)
  required = c("symbol", "date", "trading_day", "volume", RETURN_COL)
  missing = setdiff(required, header)
  if (length(missing)) {
    warning(sprintf("Skipping %s; missing columns: %s", file, paste(missing, collapse = ", ")))
    return(NULL)
  }

  cols = intersect(
    c(
      "symbol",
      "date",
      "trading_day",
      "bar_time",
      "is_first_bar",
      "open",
      "high",
      "low",
      "close",
      "volume",
      RETURN_COL
    ),
    header
  )
  dt = fread(file, select = cols, showProgress = FALSE)
  if (nrow(dt) < MIN_SYMBOL_ROWS) return(NULL)

  dt[, symbol := as.character(symbol)]
  dt[, date := as.POSIXct(date, tz = "America/New_York")]
  dt[, trading_day := as.IDate(trading_day)]
  if (!"bar_time" %in% names(dt)) dt[, bar_time := format(date, "%H:%M:%S", tz = "America/New_York")]
  if (!"is_first_bar" %in% names(dt)) dt[, is_first_bar := FALSE]
  for (col in c("open", "high", "low", "close")) {
    if (!col %in% names(dt)) dt[, (col) := NA_real_]
    dt[, (col) := as.numeric(get(col))]
  }
  if (DROP_FIRST_BAR) dt = dt[is.na(is_first_bar) | is_first_bar == FALSE]
  if (!nrow(dt)) return(NULL)

  dt[, ret := as.numeric(get(RETURN_COL))]
  dt[!is.finite(ret), ret := NA_real_]
  if (is.finite(MAX_ABS_RETURN)) {
    dt[abs(ret) > MAX_ABS_RETURN, ret := NA_real_]
  }
  dt[, volume := as.numeric(volume)]
  dt[!is.finite(volume) | volume < 0, volume := NA_real_]
  dt[, dollar_vol := close * volume]
  dt[!is.finite(dollar_vol) | dollar_vol <= 0, dollar_vol := NA_real_]
  setorder(dt, symbol, trading_day, date, bar_time)

  dt[, .(
    daily_ret = compound_return(ret),
    daily_volume = sum(volume, na.rm = TRUE),
    daily_dollar_vol = sum(dollar_vol, na.rm = TRUE),
    open_first = first_finite(open),
    high_max = max_finite(high),
    low_min = min_finite(low),
    close_last = last_finite(close),
    close_n = sum(is.finite(close)),
    close_min = min_finite(close),
    close_max = max_finite(close),
    n_bars = sum(is.finite(ret) | is.finite(volume))
  ), by = .(symbol, trading_day)]
}

collect_daily_panel = function() {
  cat(sprintf("Building daily panel from %d hourly CSV files\n", length(price_files)))
  parts = vector("list", length(price_files))
  for (i in seq_along(price_files)) {
    parts[[i]] = read_symbol_daily(price_files[[i]])
    if (i %% 250L == 0L) {
      rows = sum(vapply(parts, function(x) if (is.null(x)) 0L else nrow(x), integer(1L)))
      cat(sprintf("Read %d/%d files; daily rows so far approx=%d\n", i, length(price_files), rows))
    }
  }

  daily = rbindlist(parts, use.names = TRUE, fill = TRUE)
  rm(parts)
  invisible(gc())
  if (!nrow(daily)) stop("No daily rows were produced.")

  daily = daily[!is.na(symbol) & !is.na(trading_day)]
  daily = daily[, .(
    daily_ret = compound_return(daily_ret),
    daily_volume = sum(daily_volume, na.rm = TRUE),
    daily_dollar_vol = sum(daily_dollar_vol, na.rm = TRUE),
    open_first = first_finite(open_first),
    high_max = max_finite(high_max),
    low_min = min_finite(low_min),
    close_last = last_finite(close_last),
    close_n = sum(close_n, na.rm = TRUE),
    close_min = min_finite(close_min),
    close_max = max_finite(close_max),
    n_bars = sum(n_bars, na.rm = TRUE)
  ), by = .(symbol, trading_day)]

  all_days = sort(unique(daily$trading_day))
  ranges = daily[, .(start_day = min(trading_day), end_day = max(trading_day)), by = symbol]
  calendar = ranges[
    ,
    .(trading_day = all_days[all_days >= start_day & all_days <= end_day]),
    by = symbol
  ]
  daily = daily[calendar, on = .(symbol, trading_day)]
  setorder(daily, symbol, trading_day)

  daily[is.na(n_bars), n_bars := 0L]
  daily[is.na(daily_volume), daily_volume := 0]
  daily[is.na(daily_dollar_vol), daily_dollar_vol := 0]
  daily[is.na(close_n), close_n := 0L]
  daily[, observed_day := n_bars > 0L]
  daily[, flat_close_day := as.integer(
    close_n >= ZERO_TRADE_MIN_FLAT_BARS &
      is.finite(close_min) &
      is.finite(close_max) &
      abs(close_max - close_min) <= ZERO_TRADE_CLOSE_EPS * pmax(abs(close_max), 1)
  )]
  if (ZERO_TRADE_MODE == "volume") {
    daily[, zero_trade_day := as.integer(n_bars == 0L | daily_volume <= 0)]
  } else if (ZERO_TRADE_MODE == "flat_close") {
    daily[, zero_trade_day := flat_close_day]
  } else {
    daily[, zero_trade_day := as.integer(n_bars == 0L | daily_volume <= 0 | flat_close_day == 1L)]
  }
  daily[, daily_ret0 := fifelse(is.finite(daily_ret), daily_ret, 0)]
  daily
}

load_market_cap = function() {
  market_cap = read_optional_table(PATH_MARKET_CAP)
  date_col = intersect(c("date", "trading_day", "market_cap_date"), names(market_cap))[1L]
  required = c("symbol", "market_cap")
  if (!all(required %in% names(market_cap)) || is.na(date_col)) {
    stop("PATH_MARKET_CAP must contain symbol, date/trading_day, and market_cap columns.")
  }
  if (!"shares_outstanding" %in% names(market_cap)) market_cap[, shares_outstanding := NA_real_]
  if (!"adjusted_close" %in% names(market_cap)) market_cap[, adjusted_close := NA_real_]

  market_cap[, market_symbol := normalize_market_symbol(symbol)]
  market_cap[, trading_day := as.IDate(get(date_col)) + MARKET_CAP_LAG_DAYS]
  market_cap[, market_cap := as.numeric(market_cap)]
  market_cap[, shares_outstanding := as.numeric(shares_outstanding)]
  market_cap[, adjusted_close := as.numeric(adjusted_close)]
  market_cap = market_cap[
    nzchar(market_symbol) & !is.na(trading_day),
    .(market_symbol, trading_day, market_cap, shares_outstanding, adjusted_close)
  ]
  setorder(market_cap, market_symbol, trading_day, -market_cap)
  market_cap = market_cap[, .SD[1L], by = .(market_symbol, trading_day)]
  setkey(market_cap, market_symbol, trading_day)
  market_cap
}

join_market_cap = function(daily, market_cap) {
  daily[, market_symbol := normalize_market_symbol(symbol)]
  setkey(daily, market_symbol, trading_day)
  out = market_cap[daily, roll = Inf]
  out[!is.finite(market_cap) | market_cap <= 0, market_cap := NA_real_]
  out[!is.finite(shares_outstanding) | shares_outstanding <= 0, shares_outstanding := NA_real_]
  setorder(out, symbol, trading_day)
  out
}

compute_market_daily = function(daily) {
  out = daily[is.finite(daily_ret) & is.finite(market_cap) & market_cap > 0, .(
    n = .N,
    sum_w = sum(market_cap, na.rm = TRUE),
    sum_wr = sum(daily_ret * market_cap, na.rm = TRUE)
  ), by = trading_day]
  out[, mkt_daily_ret := sum_wr / sum_w]
  out[, .(trading_day, mkt_daily_ret)]
}

compute_market_tail = function(market_daily) {
  market_tail = copy(market_daily)
  setorder(market_tail, trading_day)
  market_tail[, mkt_mean := shift(frollmean(mkt_daily_ret, n = 252L, align = "right", fill = NA_real_), 1L)]
  market_tail[, mkt_sd := shift(frollsd(mkt_daily_ret, n = 252L, align = "right", fill = NA_real_), 1L)]
  market_tail[, market_tail_day := as.integer((mkt_daily_ret - mkt_mean) / mkt_sd <= MARKET_TAIL_Z)]
  market_tail[is.na(market_tail_day), market_tail_day := 0L]
  market_tail[, .(trading_day, market_tail_day)]
}

add_signal = function(daily, feature) {
  daily[, signal := NA_real_]

  if (grepl("^zero_trades_", feature)) {
    window = feature_window(feature, "zero_trades_")
    daily[, signal := shift(frollsum(zero_trade_day, n = window, align = "right", fill = NA_real_), 1L), by = symbol]
  } else if (grepl("^turnover_proxy_", feature)) {
    window = feature_window(feature, "turnover_proxy_")
    daily[, .turnover := daily_volume / shares_outstanding]
    daily[!is.finite(.turnover) | .turnover < 0, .turnover := NA_real_]
    daily[, signal := shift(frollmean(.turnover, n = window, align = "right", fill = NA_real_), 1L), by = symbol]
    daily[, .turnover := NULL]
  } else if (grepl("^turnover_var_proxy_", feature)) {
    window = feature_window(feature, "turnover_var_proxy_")
    daily[, .turnover := daily_volume / shares_outstanding]
    daily[!is.finite(.turnover) | .turnover < 0, .turnover := NA_real_]
    daily[, signal := shift(frollsd(.turnover, n = window, align = "right", fill = NA_real_)^2, 1L), by = symbol]
    daily[, .turnover := NULL]
  } else if (grepl("^rvol_", feature)) {
    window = feature_window(feature, "rvol_")
    daily[, signal := shift(frollsd(daily_ret0, n = window, align = "right", fill = NA_real_), 1L), by = symbol]
  } else if (grepl("^mom_", feature)) {
    window = feature_window(feature, "mom_")
    daily[, .log_ret := log1p(pmax(daily_ret0, -0.999999))]
    daily[, signal := shift(exp(frollsum(.log_ret, n = window, align = "right", fill = NA_real_)) - 1, 1L), by = symbol]
    daily[, .log_ret := NULL]
  } else if (grepl("^dollar_vol_", feature)) {
    window = feature_window(feature, "dollar_vol_")
    daily[, .log_dollar_vol := log1p(pmax(daily_dollar_vol, 0))]
    daily[, signal := shift(frollmean(.log_dollar_vol, n = window, align = "right", fill = NA_real_), 1L), by = symbol]
    daily[, .log_dollar_vol := NULL]
  } else if (grepl("^amihud_", feature)) {
    window = feature_window(feature, "amihud_")
    daily[, .amihud := abs(daily_ret0) / daily_dollar_vol]
    daily[!is.finite(.amihud) | .amihud < 0, .amihud := NA_real_]
    daily[, signal := shift(frollmean(.amihud, n = window, align = "right", fill = NA_real_), 1L), by = symbol]
    daily[, .amihud := NULL]
  } else if (grepl("^hl_range_", feature)) {
    window = feature_window(feature, "hl_range_")
    daily[, .hl_range := log(high_max / low_min)]
    daily[!is.finite(.hl_range) | .hl_range < 0, .hl_range := NA_real_]
    daily[, signal := shift(frollmean(.hl_range, n = window, align = "right", fill = NA_real_), 1L), by = symbol]
    daily[, .hl_range := NULL]
  } else if (grepl("^ivol_capm_", feature)) {
    window = feature_window(feature, "ivol_capm_")
    daily[, .mean_r := frollmean(daily_ret0, n = window, align = "right", fill = NA_real_), by = symbol]
    daily[, .mean_m := frollmean(mkt_daily_ret, n = window, align = "right", fill = NA_real_), by = symbol]
    daily[, .mean_rm := frollmean(daily_ret0 * mkt_daily_ret, n = window, align = "right", fill = NA_real_), by = symbol]
    daily[, .mean_m2 := frollmean(mkt_daily_ret^2, n = window, align = "right", fill = NA_real_), by = symbol]
    daily[, .var_m := .mean_m2 - .mean_m^2]
    daily[, .beta_capm := (.mean_rm - .mean_r * .mean_m) / fifelse(abs(.var_m) > 1e-12, .var_m, NA_real_)]
    daily[, .resid_capm := daily_ret0 - .beta_capm * mkt_daily_ret]
    daily[, signal := shift(frollsd(.resid_capm, n = window, align = "right", fill = NA_real_), 1L), by = symbol]
    daily[, c(".mean_r", ".mean_m", ".mean_rm", ".mean_m2", ".var_m", ".beta_capm", ".resid_capm") := NULL]
  } else if (grepl("^beta_tailrisk_proxy_", feature)) {
    window = feature_window(feature, "beta_tailrisk_proxy_")
    daily[, .tail := as.numeric(market_tail_day)]
    daily[, .tail_n := frollsum(.tail, n = window, align = "right", fill = NA_real_), by = symbol]
    daily[, .tail_sum_r := frollsum(.tail * daily_ret0, n = window, align = "right", fill = NA_real_), by = symbol]
    daily[, .tail_sum_m := frollsum(.tail * mkt_daily_ret, n = window, align = "right", fill = NA_real_), by = symbol]
    daily[, .tail_sum_rm := frollsum(.tail * daily_ret0 * mkt_daily_ret, n = window, align = "right", fill = NA_real_), by = symbol]
    daily[, .tail_sum_m2 := frollsum(.tail * mkt_daily_ret^2, n = window, align = "right", fill = NA_real_), by = symbol]
    daily[, .tail_mean_r := .tail_sum_r / .tail_n]
    daily[, .tail_mean_m := .tail_sum_m / .tail_n]
    daily[, .tail_cov := .tail_sum_rm / .tail_n - .tail_mean_r * .tail_mean_m]
    daily[, .tail_var_m := .tail_sum_m2 / .tail_n - .tail_mean_m^2]
    daily[, signal := shift(.tail_cov / fifelse(abs(.tail_var_m) > 1e-12, .tail_var_m, NA_real_), 1L), by = symbol]
    daily[, c(
      ".tail",
      ".tail_n",
      ".tail_sum_r",
      ".tail_sum_m",
      ".tail_sum_rm",
      ".tail_sum_m2",
      ".tail_mean_r",
      ".tail_mean_m",
      ".tail_cov",
      ".tail_var_m"
    ) := NULL]
  } else {
    stop(sprintf("Unsupported DAILY_FEATURE: %s", feature))
  }

  daily[!is.finite(signal), signal := NA_real_]
  invisible(NULL)
}

compute_market_factor = function(daily) {
  out = daily[is.finite(daily_ret) & is.finite(market_cap) & market_cap > 0, .(
    n = .N,
    n_long = NA_integer_,
    n_short = NA_integer_,
    long_ret = NA_real_,
    short_ret = NA_real_,
    factor_ret = weighted_mean(daily_ret, market_cap),
    signal_q_low = NA_real_,
    signal_q_high = NA_real_
  ), by = trading_day]
  out[, `:=`(feature = "MKT_MARKET_CAP_W", source = "market_cap_weighted_market")]
  out
}

compute_signal_factor = function(daily, feature) {
  cat(sprintf("Computing daily factor %s\n", feature))
  add_signal(daily, feature)
  out = daily[, {
    ok = is.finite(signal) & is.finite(daily_ret) & is.finite(market_cap) & market_cap > 0
    n_ok = sum(ok)
    if (n_ok < 2L * MIN_LEG_N) {
      empty_factor_row()
    } else {
      q_low = as.numeric(quantile(signal[ok], probs = TAIL_PROB, na.rm = TRUE, type = 7L))
      q_high = as.numeric(quantile(signal[ok], probs = 1 - TAIL_PROB, na.rm = TRUE, type = 7L))
      long = ok & signal >= q_high
      short = ok & signal <= q_low
      n_long = sum(long)
      n_short = sum(short)
      if (n_long < MIN_LEG_N || n_short < MIN_LEG_N || !is.finite(q_low) || !is.finite(q_high)) {
        empty_factor_row()
      } else {
        long_ret = weighted_mean(daily_ret[long], market_cap[long])
        short_ret = weighted_mean(daily_ret[short], market_cap[short])
        list(
          n = n_ok,
          n_long = n_long,
          n_short = n_short,
          long_ret = long_ret,
          short_ret = short_ret,
          factor_ret = long_ret - short_ret,
          signal_q_low = q_low,
          signal_q_high = q_high
        )
      }
    }
  }, by = trading_day]
  daily[, signal := NULL]
  invisible(gc())
  out[, `:=`(feature = feature, source = "daily_characteristic_decile_spread")]
  out
}

compute_industry_factors = function(daily) {
  if (!nzchar(PATH_INDUSTRY_MAP) || !file.exists(PATH_INDUSTRY_MAP)) return(NULL)
  industry_map = fread(PATH_INDUSTRY_MAP, showProgress = FALSE)
  industry_col = intersect(c("industry", "sector", "gics_sector", "sector_name"), names(industry_map))[1L]
  if (!"symbol" %in% names(industry_map) || is.na(industry_col)) {
    warning("PATH_INDUSTRY_MAP must have symbol and industry/sector columns. Skipping industry factors.")
    return(NULL)
  }
  industry_map = unique(industry_map[, .(
    symbol = as.character(symbol),
    industry = as.character(get(industry_col))
  )])
  x = industry_map[daily, on = "symbol"]
  x = x[!is.na(industry) & nzchar(industry) & is.finite(daily_ret) & is.finite(market_cap) & market_cap > 0]
  if (!nrow(x)) return(NULL)
  out = x[, .(
    n = .N,
    n_long = NA_integer_,
    n_short = NA_integer_,
    long_ret = NA_real_,
    short_ret = NA_real_,
    factor_ret = weighted_mean(daily_ret, market_cap),
    signal_q_low = NA_real_,
    signal_q_high = NA_real_
  ), by = .(trading_day, industry)]
  out[, `:=`(
    feature = make_factor_name("IND_", industry),
    source = "industry_market_cap_weighted"
  )]
  out[, industry := NULL]
  out
}

cat(sprintf(
  "Daily characteristic factor run: output=%s files=%d return_col=%s drop_first=%s max_abs_return=%s zero_trade_mode=%s\n",
  PATH_FACTORS,
  length(price_files),
  RETURN_COL,
  DROP_FIRST_BAR,
  as.character(MAX_ABS_RETURN),
  ZERO_TRADE_MODE
))

daily = collect_daily_panel()
cat(sprintf(
  "Daily panel rows=%d symbols=%d days=%d date_range=%s..%s\n",
  nrow(daily),
  uniqueN(daily$symbol),
  uniqueN(daily$trading_day),
  min(daily$trading_day),
  max(daily$trading_day)
))

market_cap = load_market_cap()
cat(sprintf(
  "Using market-cap data from %s rows=%d symbols=%d lag_days=%d\n",
  PATH_MARKET_CAP,
  nrow(market_cap),
  uniqueN(market_cap$market_symbol),
  MARKET_CAP_LAG_DAYS
))
daily = join_market_cap(daily, market_cap)
rm(market_cap)
invisible(gc())

market_daily = compute_market_daily(daily)
daily = market_daily[daily, on = "trading_day"]
market_tail = compute_market_tail(market_daily)
daily = market_tail[daily, on = "trading_day"]
setorder(daily, symbol, trading_day)

sample_file = file.path(PATH_FACTORS, "daily_panel_sample.csv")
sample_days = tail(sort(unique(daily$trading_day)), min(5L, uniqueN(daily$trading_day)))
sample_panel = daily[
  trading_day %in% sample_days,
  .(
    symbol,
    trading_day,
    daily_ret,
    market_cap,
    shares_outstanding,
    daily_volume,
    daily_dollar_vol,
    zero_trade_day,
    flat_close_day,
    n_bars
  )
]
fwrite(head(sample_panel, 5000L), sample_file)

factor_parts = list()
if ("MKT_MARKET_CAP_W" %in% FEATURES) {
  cat("Computing daily market factor MKT_MARKET_CAP_W\n")
  factor_parts[[length(factor_parts) + 1L]] = compute_market_factor(daily)
}

signal_features = setdiff(FEATURES, "MKT_MARKET_CAP_W")
for (feature in signal_features) {
  factor_parts[[length(factor_parts) + 1L]] = compute_signal_factor(daily, feature)
}

industry_returns = compute_industry_factors(daily)
if (!is.null(industry_returns)) {
  factor_parts[[length(factor_parts) + 1L]] = industry_returns
}

factor_returns = rbindlist(factor_parts, use.names = TRUE, fill = TRUE)
factor_returns[, `:=`(
  date = as.POSIXct(paste(as.Date(trading_day), "16:00:00"), tz = "America/New_York"),
  bar_time = "16:00:00",
  is_first_bar = FALSE
)]
setcolorder(factor_returns, c(
  "date",
  "trading_day",
  "bar_time",
  "is_first_bar",
  "feature",
  "factor_ret",
  "source",
  setdiff(names(factor_returns), c("date", "trading_day", "bar_time", "is_first_bar", "feature", "factor_ret", "source"))
))
setorder(factor_returns, feature, trading_day)
fwrite(factor_returns, out_long)
saveRDS(factor_returns, sub("\\.csv$", ".rds", out_long))

wide = dcast(
  factor_returns[, .(date, trading_day, bar_time, feature, factor_ret)],
  date + trading_day + bar_time ~ feature,
  value.var = "factor_ret"
)
setorder(wide, trading_day)
fwrite(wide, out_wide)
saveRDS(wide, sub("\\.csv$", ".rds", out_wide))

summary_dt = factor_returns[, {
  ok = is.finite(factor_ret)
  ok_n = is.finite(n)
  ok_long = is.finite(n_long)
  ok_short = is.finite(n_short)
  list(
    n_obs = sum(ok),
    first_day = if (any(ok)) min(trading_day[ok]) else as.IDate(NA),
    last_day = if (any(ok)) max(trading_day[ok]) else as.IDate(NA),
    mean_ret = if (any(ok)) mean(factor_ret[ok]) else NA_real_,
    sd_ret = if (sum(ok) > 1L) sd(factor_ret[ok]) else NA_real_,
    min_ret = if (any(ok)) min(factor_ret[ok]) else NA_real_,
    max_ret = if (any(ok)) max(factor_ret[ok]) else NA_real_,
    mean_n = if (any(ok_n)) mean(n[ok_n]) else NA_real_,
    mean_n_long = if (any(ok_long)) mean(n_long[ok_long]) else NA_real_,
    mean_n_short = if (any(ok_short)) mean(n_short[ok_short]) else NA_real_
  )
}, by = .(feature, source)]
setorder(summary_dt, feature)
fwrite(summary_dt, out_summary)

manifest = data.table(
  created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  path_prices = PATH_PRICES,
  path_factors = PATH_FACTORS,
  path_market_cap = PATH_MARKET_CAP,
  path_industry_map = PATH_INDUSTRY_MAP,
  n_price_files = length(price_files),
  n_daily_rows = nrow(daily),
  n_symbols = uniqueN(daily$symbol),
  n_days = uniqueN(daily$trading_day),
  features = paste(FEATURES, collapse = ","),
  return_col = RETURN_COL,
  drop_first_bar = DROP_FIRST_BAR,
  max_abs_return = MAX_ABS_RETURN,
  market_cap_lag_days = MARKET_CAP_LAG_DAYS,
  zero_trade_mode = ZERO_TRADE_MODE,
  zero_trade_close_eps = ZERO_TRADE_CLOSE_EPS,
  zero_trade_min_flat_bars = ZERO_TRADE_MIN_FLAT_BARS,
  tail_prob = TAIL_PROB,
  min_leg_n = MIN_LEG_N,
  market_tail_z = MARKET_TAIL_Z
)
fwrite(manifest, file.path(PATH_FACTORS, "manifest.csv"))

if (WRITE_DAILY_PANEL) {
  panel_file = file.path(PATH_FACTORS, "daily_panel.rds")
  saveRDS(daily, panel_file)
  cat(sprintf("Saved daily panel: %s rows=%d\n", panel_file, nrow(daily)))
}

cat(sprintf(
  "Saved daily factor returns to %s rows=%d wide_rows=%d features=%d\n",
  PATH_FACTORS,
  nrow(factor_returns),
  nrow(wide),
  uniqueN(factor_returns$feature)
))
print(summary_dt)
quit(save = "no", status = 0L, runLast = FALSE)
