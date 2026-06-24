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

env_num = function(name, default) {
  value = env_chr(name, "")
  if (!nzchar(value)) return(default)
  value = suppressWarnings(as.numeric(value))
  if (is.na(value)) stop(sprintf("%s must be numeric.", name))
  value
}

env_bool = function(name, default) {
  value = env_chr(name, if (default) "1" else "0")
  value %in% c("1", "true", "TRUE", "yes", "YES")
}

parse_ints = function(value, default) {
  if (!nzchar(value)) return(default)
  out = suppressWarnings(as.integer(trimws(strsplit(value, ",", fixed = TRUE)[[1L]])))
  out = out[!is.na(out) & out > 1L]
  if (!length(out)) default else sort(unique(out))
}

make_factor_name = function(prefix, value) {
  cleaned = gsub("[^A-Za-z0-9]+", "_", value)
  cleaned = gsub("^_+|_+$", "", cleaned)
  paste0(prefix, cleaned)
}

PATH_PRICES = env_chr("PATH_PRICES", "prices_factors_hour")
PATH_FACTORS = env_chr("PATH_FACTORS", "factor_returns_simple")
PATH_INDUSTRY_MAP = env_chr("PATH_INDUSTRY_MAP", "")
PATH_FUNDAMENTALS = env_chr("PATH_FUNDAMENTALS", "")

RETURN_COL = env_chr("SIMPLE_RETURN_COL", "returns_oc")
WINDOWS = parse_ints(env_chr("SIMPLE_WINDOWS", ""), c(22L, 147L, 441L))
TAIL_PROB = env_num("SIMPLE_TAIL_PROB", 0.10)
MIN_LEG_N = env_int("SIMPLE_MIN_LEG_N", 10L)
MAX_NA_FRAC = env_num("SIMPLE_MAX_NA_FRAC", 0.80)
MIN_SYMBOL_ROWS = env_int("SIMPLE_MIN_SYMBOL_ROWS", 50L)
MAX_FILES = env_int("SIMPLE_MAX_FILES", 0L)
WRITE_LONG_CSV = env_bool("SIMPLE_WRITE_LONG_CSV", TRUE)
WRITE_RDS = env_bool("SIMPLE_WRITE_RDS", TRUE)
FUNDAMENTAL_LAG_DAYS = env_int("SIMPLE_FUNDAMENTAL_LAG_DAYS", 45L)

if (TAIL_PROB <= 0 || TAIL_PROB >= 0.5) {
  stop("SIMPLE_TAIL_PROB must be in (0, 0.5).")
}
if (MAX_NA_FRAC < 0 || MAX_NA_FRAC >= 1) {
  stop("SIMPLE_MAX_NA_FRAC must be in [0, 1).")
}

dir.create(PATH_FACTORS, recursive = TRUE, showWarnings = FALSE)

price_files = sort(list.files(PATH_PRICES, pattern = "\\.csv$", full.names = TRUE))
if (!length(price_files)) {
  stop(sprintf("No CSV files found in PATH_PRICES: %s", PATH_PRICES))
}
if (MAX_FILES > 0L) {
  price_files = head(price_files, MAX_FILES)
}

read_header = function(file) {
  names(fread(file, nrows = 0L, showProgress = FALSE))
}

read_price_file = function(file) {
  header = read_header(file)
  required = c("symbol", "date", "open", "high", "low", "close", "volume", RETURN_COL)
  missing = setdiff(required, header)
  if (length(missing)) {
    stop(sprintf("Missing required columns in %s: %s", file, paste(missing, collapse = ", ")))
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
  setDT(dt)

  if (nrow(dt) < MIN_SYMBOL_ROWS) return(NULL)

  dt[, date := as.POSIXct(date, tz = "America/New_York")]
  if (!"trading_day" %in% names(dt)) {
    dt[, trading_day := as.IDate(date, tz = "America/New_York")]
  } else {
    dt[, trading_day := as.IDate(trading_day)]
  }
  if (!"bar_time" %in% names(dt)) {
    dt[, bar_time := format(date, "%H:%M:%S", tz = "America/New_York")]
  }
  if (!"is_first_bar" %in% names(dt)) {
    dt[, is_first_bar := FALSE]
  }

  dt[, symbol := as.character(symbol)]
  dt[, .ret := as.numeric(get(RETURN_COL))]
  dt[, .dollar_vol := as.numeric(close) * as.numeric(volume)]
  dt[!is.finite(.dollar_vol) | .dollar_vol <= 0, .dollar_vol := NA_real_]
  dt[!is.finite(.ret), .ret := NA_real_]
  dt[, .range := (as.numeric(high) - as.numeric(low)) / pmax(abs(as.numeric(close)), 1e-8)]
  dt[!is.finite(.range), .range := NA_real_]

  keep = c(
    "symbol",
    "date",
    "trading_day",
    "bar_time",
    "is_first_bar",
    ".ret",
    ".dollar_vol",
    ".range"
  )
  dt[, ..keep]
}

parts = vector("list", length(price_files))
for (i in seq_along(price_files)) {
  parts[[i]] = read_price_file(price_files[[i]])
  if (i %% 100L == 0L) {
    cat(sprintf("Read %d/%d price files\n", i, length(price_files)))
  }
}
dt = rbindlist(parts, use.names = TRUE, fill = TRUE)
rm(parts)
if (!nrow(dt)) stop("No rows after reading price files.")

setorder(dt, symbol, date)
dt[, .weight := shift(.dollar_vol, 1L), by = symbol]
dt[!is.finite(.weight) | .weight <= 0, .weight := NA_real_]

feature_cols = character()
for (window in WINDOWS) {
  mom_col = sprintf("mom_%d", window)
  vol_col = sprintf("vol_%d", window)
  dolvol_col = sprintf("dolvol_%d", window)
  range_col = sprintf("range_%d", window)
  rsi_col = sprintf("rsi_%d", window)

  dt[, .logret_tmp := fifelse(is.finite(.ret) & .ret > -1, log1p(.ret), NA_real_)]
  dt[, (mom_col) := shift(exp(frollsum(.logret_tmp, n = window, align = "right", fill = NA_real_)) - 1, 1L), by = symbol]
  dt[, (vol_col) := shift(frollsd(.ret, n = window, align = "right", fill = NA_real_), 1L), by = symbol]
  dt[, (dolvol_col) := shift(frollmean(log1p(.dollar_vol), n = window, align = "right", fill = NA_real_), 1L), by = symbol]
  dt[, (range_col) := shift(frollmean(.range, n = window, align = "right", fill = NA_real_), 1L), by = symbol]

  dt[, .up_tmp := fifelse(is.finite(.ret) & .ret > 0, .ret, 0)]
  dt[, .down_tmp := fifelse(is.finite(.ret) & .ret < 0, -.ret, 0)]
  dt[, .avg_up_tmp := frollmean(.up_tmp, n = window, align = "right", fill = NA_real_), by = symbol]
  dt[, .avg_down_tmp := frollmean(.down_tmp, n = window, align = "right", fill = NA_real_), by = symbol]
  dt[, (rsi_col) := shift(100 - 100 / (1 + .avg_up_tmp / pmax(.avg_down_tmp, 1e-12)), 1L), by = symbol]

  feature_cols = c(feature_cols, mom_col, vol_col, dolvol_col, range_col, rsi_col)
}
dt[, c(".logret_tmp", ".up_tmp", ".down_tmp", ".avg_up_tmp", ".avg_down_tmp") := NULL]

weighted_mean = function(x, w) {
  ok = is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
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

compute_characteristic_factor = function(data, feature, prefix = "") {
  factor_name = paste0(prefix, feature)
  out = data[, {
    signal = as.numeric(get(feature))
    ret = .ret
    w = .weight
    ok = is.finite(signal) & is.finite(ret)
    n_ok = sum(ok)

    if (n_ok < 2L * MIN_LEG_N || mean(!ok) > MAX_NA_FRAC) {
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
        long_ret = weighted_mean(ret[long], w[long])
        short_ret = weighted_mean(ret[short], w[short])
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
  }, by = .(date, trading_day, bar_time, is_first_bar)]

  out[, feature := factor_name]
  out
}

market_ew = dt[, .(
  n = sum(is.finite(.ret)),
  n_long = NA_integer_,
  n_short = NA_integer_,
  long_ret = NA_real_,
  short_ret = NA_real_,
  factor_ret = mean(.ret, na.rm = TRUE),
  signal_q_low = NA_real_,
  signal_q_high = NA_real_,
  feature = "MKT_EW"
), by = .(date, trading_day, bar_time, is_first_bar)]

market_vw = dt[, .(
  n = sum(is.finite(.ret) & is.finite(.weight) & .weight > 0),
  n_long = NA_integer_,
  n_short = NA_integer_,
  long_ret = NA_real_,
  short_ret = NA_real_,
  factor_ret = weighted_mean(.ret, .weight),
  signal_q_low = NA_real_,
  signal_q_high = NA_real_,
  feature = "MKT_DOLLAR_VOL_W"
), by = .(date, trading_day, bar_time, is_first_bar)]

factor_parts = list(market_ew, market_vw)
manifest = data.table(feature = c("MKT_EW", "MKT_DOLLAR_VOL_W"), source = "market")

if (nzchar(PATH_INDUSTRY_MAP) && file.exists(PATH_INDUSTRY_MAP)) {
  industry_map = fread(PATH_INDUSTRY_MAP)
  industry_col = intersect(c("industry", "sector", "gics_sector", "sector_name"), names(industry_map))[1L]
  if (!"symbol" %in% names(industry_map) || is.na(industry_col)) {
    warning("PATH_INDUSTRY_MAP must have symbol and industry/sector columns. Skipping industry factors.")
  } else {
    industry_map = unique(industry_map[, .(symbol = as.character(symbol), industry = as.character(get(industry_col)))])
    dt = industry_map[dt, on = "symbol"]
    industry_returns = dt[!is.na(industry) & nzchar(industry), .(
      n = sum(is.finite(.ret)),
      n_long = NA_integer_,
      n_short = NA_integer_,
      long_ret = NA_real_,
      short_ret = NA_real_,
      factor_ret = mean(.ret, na.rm = TRUE),
      signal_q_low = NA_real_,
      signal_q_high = NA_real_
    ), by = .(date, trading_day, bar_time, is_first_bar, industry)]
    industry_returns[, feature := make_factor_name("IND_", industry)]
    industry_returns[, industry := NULL]
    factor_parts[[length(factor_parts) + 1L]] = industry_returns
    manifest = rbind(
      manifest,
      unique(industry_returns[, .(feature, source = "industry")]),
      use.names = TRUE
    )
  }
}

read_optional_table = function(file) {
  if (grepl("\\.rds$", file, ignore.case = TRUE)) {
    as.data.table(readRDS(file))
  } else {
    fread(file, showProgress = FALSE)
  }
}

if (nzchar(PATH_FUNDAMENTALS) && file.exists(PATH_FUNDAMENTALS)) {
  fundamentals = read_optional_table(PATH_FUNDAMENTALS)
  date_col = intersect(c("date", "trading_day", "month", "period_end", "effective_date"), names(fundamentals))[1L]
  if (!"symbol" %in% names(fundamentals) || is.na(date_col)) {
    warning("PATH_FUNDAMENTALS must have symbol and date/trading_day columns. Skipping fundamental factors.")
  } else {
    fundamentals[, symbol := as.character(symbol)]
    setnames(fundamentals, date_col, ".fund_date")
    fundamentals[, .fund_date := as.IDate(.fund_date)]
    fundamentals[, trading_day := .fund_date + FUNDAMENTAL_LAG_DAYS]

    excluded = c("symbol", ".fund_date", "trading_day", "date", "industry", "sector", "exchange", "name")
    fundamental_cols = setdiff(names(fundamentals), excluded)
    requested = env_chr("SIMPLE_FUNDAMENTAL_COLS", "")
    if (nzchar(requested)) {
      requested_cols = trimws(strsplit(requested, ",", fixed = TRUE)[[1L]])
      fundamental_cols = intersect(fundamental_cols, requested_cols)
    }
    fundamental_cols = fundamental_cols[vapply(fundamentals[, ..fundamental_cols], is.numeric, logical(1L))]

    if (length(fundamental_cols)) {
      new_names = paste0("fund_", fundamental_cols)
      setnames(fundamentals, fundamental_cols, new_names)
      keep_cols = c("symbol", "trading_day", new_names)
      fundamentals = fundamentals[, ..keep_cols]
      setkey(fundamentals, symbol, trading_day)
      setkey(dt, symbol, trading_day)
      dt = fundamentals[dt, roll = Inf]
      feature_cols = c(feature_cols, new_names)
      manifest = rbind(
        manifest,
        data.table(feature = new_names, source = "fundamental"),
        use.names = TRUE,
        fill = TRUE
      )
    } else {
      warning("No numeric fundamental columns found. Skipping fundamental factors.")
    }
  }
}

for (feature in feature_cols) {
  cat(sprintf("Computing factor: %s\n", feature))
  factor_parts[[length(factor_parts) + 1L]] = compute_characteristic_factor(dt, feature)
}
manifest = rbind(
  manifest,
  data.table(feature = feature_cols, source = fifelse(grepl("^fund_", feature_cols), "fundamental", "ohlcv")),
  use.names = TRUE,
  fill = TRUE
)
manifest = unique(manifest, by = "feature")

factor_returns = rbindlist(factor_parts, use.names = TRUE, fill = TRUE)
setcolorder(factor_returns, c(
  "date",
  "trading_day",
  "bar_time",
  "is_first_bar",
  "feature",
  "factor_ret",
  "long_ret",
  "short_ret",
  "n",
  "n_long",
  "n_short",
  "signal_q_low",
  "signal_q_high"
))
setorder(factor_returns, feature, date)

summary_dt = factor_returns[, .(
  n_obs = sum(is.finite(factor_ret)),
  first_date = min(date[is.finite(factor_ret)], na.rm = TRUE),
  last_date = max(date[is.finite(factor_ret)], na.rm = TRUE),
  mean_ret = mean(factor_ret, na.rm = TRUE),
  sd_ret = sd(factor_ret, na.rm = TRUE),
  mean_n = mean(n, na.rm = TRUE),
  mean_n_long = mean(n_long, na.rm = TRUE),
  mean_n_short = mean(n_short, na.rm = TRUE)
), by = feature]
setorder(summary_dt, -n_obs, feature)

wide = dcast(
  factor_returns[, .(date, trading_day, bar_time, is_first_bar, feature, factor_ret)],
  date + trading_day + bar_time + is_first_bar ~ feature,
  value.var = "factor_ret"
)
setorder(wide, date)

fwrite(manifest, file.path(PATH_FACTORS, "factor_feature_manifest.csv"))
fwrite(summary_dt, file.path(PATH_FACTORS, "factor_returns_summary.csv"))
fwrite(wide, file.path(PATH_FACTORS, "factor_returns_wide.csv"))
if (WRITE_LONG_CSV) {
  fwrite(factor_returns, file.path(PATH_FACTORS, "factor_returns_long.csv"))
}
if (WRITE_RDS) {
  saveRDS(factor_returns, file.path(PATH_FACTORS, "factor_returns_long.rds"))
  saveRDS(wide, file.path(PATH_FACTORS, "factor_returns_wide.rds"))
}

cat(sprintf(
  "Saved simple hourly factors to %s rows=%d wide_cols=%d features=%d\n",
  PATH_FACTORS,
  nrow(wide),
  ncol(wide),
  uniqueN(factor_returns$feature)
))

quit(save = "no", status = 0L, runLast = FALSE)
