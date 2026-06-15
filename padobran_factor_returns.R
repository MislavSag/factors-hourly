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
  if (is.na(value) || value <= 0L) {
    stop(sprintf("%s must be a positive integer.", name))
  }
  value
}

env_num = function(name, default) {
  value = env_chr(name, "")
  if (!nzchar(value)) return(default)
  value = suppressWarnings(as.numeric(value))
  if (is.na(value)) {
    stop(sprintf("%s must be numeric.", name))
  }
  value
}

PATH_PRICES = env_chr("PATH_PRICES", "prices_factors_hour")
PATH_PREDICTORS = env_chr("PATH_PREDICTORS", "hourly")
PATH_FACTORS = env_chr("PATH_FACTORS", "factor_returns")

RETURN_COL = env_chr("FACTOR_RETURN_COL", "returns_oc")
WEIGHT_COL = env_chr("FACTOR_WEIGHT_COL", "")
TAIL_PROB = env_num("FACTOR_TAIL_PROB", 0.10)
MIN_LEG_N = env_int("FACTOR_MIN_LEG_N", 10L)
LAG_BARS = env_int("FACTOR_LAG_BARS", 1L)
COLS_PER_JOB = env_int("FACTOR_COLS_PER_JOB", 5L)
MAX_NA_FRAC = env_num("FACTOR_MAX_NA_FRAC", 0.80)

if (TAIL_PROB <= 0 || TAIL_PROB >= 0.5) {
  stop("FACTOR_TAIL_PROB must be in (0, 0.5).")
}
if (MAX_NA_FRAC < 0 || MAX_NA_FRAC >= 1) {
  stop("FACTOR_MAX_NA_FRAC must be in [0, 1).")
}

dir.create(PATH_FACTORS, recursive = TRUE, showWarnings = FALSE)
PARTS_DIR = file.path(PATH_FACTORS, "parts")
dir.create(PARTS_DIR, recursive = TRUE, showWarnings = FALSE)

predictor_files = list.files(PATH_PREDICTORS, pattern = "\\.csv$", full.names = TRUE)
if (length(predictor_files) == 0L) {
  stop(sprintf("No predictor CSV files found in PATH_PREDICTORS: %s", PATH_PREDICTORS))
}
predictor_files = sort(predictor_files)

price_file_for = function(predictor_file) {
  candidate = file.path(PATH_PRICES, basename(predictor_file))
  if (file.exists(candidate)) return(candidate)
  stop(sprintf(
    "Matching price file not found for %s. Expected: %s",
    predictor_file,
    candidate
  ))
}

read_header = function(file) {
  names(fread(file, nrows = 0L, showProgress = FALSE))
}

exclude_cols = c(
  "id",
  "symbol",
  "date",
  "trading_day",
  "bar_time",
  "is_first_bar",
  "row_in_day",
  "rows_in_day",
  "open",
  "high",
  "low",
  "close",
  "close_raw",
  "volume",
  "dollar_vol",
  "market_cap",
  "returns",
  "returns_cc",
  "returns_intraday",
  "returns_oc"
)

predictor_headers = unique(unlist(lapply(predictor_files, read_header), use.names = FALSE))
feature_cols = setdiff(predictor_headers, exclude_cols)
feature_cols = feature_cols[!grepl("^(target_|pred_|benchmark_|strategy_)", feature_cols)]
feature_cols = sort(unique(feature_cols))

if (length(feature_cols) == 0L) {
  stop("No candidate numeric predictor columns found.")
}

manifest = data.table(
  feature_index = seq_along(feature_cols),
  feature = feature_cols,
  job_index = ceiling(seq_along(feature_cols) / COLS_PER_JOB)
)
fwrite(manifest, file.path(PATH_FACTORS, "factor_feature_manifest.csv"))

n_jobs = max(manifest$job_index)
job_id = if (interactive()) 1L else as.integer(Sys.getenv("PBS_ARRAY_INDEX", unset = "1"))
if (is.na(job_id) || job_id < 1L) {
  stop("PBS_ARRAY_INDEX must be a positive integer.")
}
if (job_id > n_jobs) {
  cat(sprintf("PBS_ARRAY_INDEX=%d exceeds required jobs=%d. Nothing to do.\n", job_id, n_jobs))
  quit(save = "no", status = 0L)
}

selected_features = manifest[job_index == job_id, feature]
cat(sprintf(
  "Factor job %d/%d: %d features, %d predictor files, return_col=%s, tail_prob=%.4f, lag_bars=%d\n",
  job_id,
  n_jobs,
  length(selected_features),
  length(predictor_files),
  RETURN_COL,
  TAIL_PROB,
  LAG_BARS
))

read_pair = function(predictor_file, selected_features) {
  predictor_header = read_header(predictor_file)
  these_features = intersect(selected_features, predictor_header)
  if (length(these_features) == 0L) return(NULL)

  predictor_cols = c("symbol", "date", these_features)
  predictors = fread(
    predictor_file,
    select = predictor_cols,
    showProgress = FALSE
  )

  price_file = price_file_for(predictor_file)
  price_header = read_header(price_file)
  price_cols = intersect(
    c("symbol", "date", RETURN_COL, "trading_day", "bar_time", "is_first_bar", WEIGHT_COL),
    price_header
  )
  required_price_cols = c("symbol", "date", RETURN_COL)
  missing_price_cols = setdiff(required_price_cols, price_cols)
  if (length(missing_price_cols) > 0L) {
    stop(sprintf(
      "Missing required price columns in %s: %s",
      price_file,
      paste(missing_price_cols, collapse = ", ")
    ))
  }

  prices = fread(
    price_file,
    select = price_cols,
    showProgress = FALSE
  )

  dt = merge(predictors, prices, by = c("symbol", "date"), all = FALSE)
  setDT(dt)
  dt
}

parts = vector("list", length(predictor_files))
for (k in seq_along(predictor_files)) {
  parts[[k]] = read_pair(predictor_files[[k]], selected_features)
  if (k %% 50L == 0L) {
    cat(sprintf("Read %d/%d predictor files\n", k, length(predictor_files)))
  }
}
dt = rbindlist(parts, fill = TRUE, use.names = TRUE)
rm(parts)

if (nrow(dt) == 0L) {
  stop("No rows after merging predictors and prices.")
}

dt[, date := as.POSIXct(date, tz = "America/New_York")]
setorder(dt, symbol, date)

if (!"trading_day" %in% names(dt)) {
  dt[, trading_day := as.IDate(date, tz = "America/New_York")]
}
if (!"bar_time" %in% names(dt)) {
  dt[, bar_time := format(date, "%H:%M:%S", tz = "America/New_York")]
}
if (!"is_first_bar" %in% names(dt)) {
  dt[, is_first_bar := FALSE]
}

selected_features = intersect(selected_features, names(dt))
selected_features = selected_features[vapply(dt[, ..selected_features], is.numeric, logical(1L))]
if (length(selected_features) == 0L) {
  stop("Selected feature chunk has no numeric columns after reading data.")
}

if (nzchar(WEIGHT_COL) && WEIGHT_COL %in% names(dt)) {
  dt[, .factor_weight := shift(get(WEIGHT_COL), n = LAG_BARS), by = symbol]
  dt[!is.finite(.factor_weight) | .factor_weight <= 0, .factor_weight := NA_real_]
} else {
  dt[, .factor_weight := 1.0]
}

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

compute_one_factor = function(feature) {
  lag_col = paste0(".lag_", feature)
  dt[, (lag_col) := shift(get(feature), n = LAG_BARS), by = symbol]

  out = dt[, {
    signal = as.numeric(get(lag_col))
    ret = as.numeric(get(RETURN_COL))
    w = as.numeric(.factor_weight)
    ok = is.finite(signal) & is.finite(ret) & is.finite(w) & w > 0
    n_ok = sum(ok)

    if (n_ok < 2L * MIN_LEG_N) {
      empty_factor_row()
    } else if (mean(!ok) > MAX_NA_FRAC) {
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

  out[, feature := feature]
  setcolorder(out, c(
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

  dt[, (lag_col) := NULL]
  out
}

factor_parts = vector("list", length(selected_features))
for (j in seq_along(selected_features)) {
  feature = selected_features[[j]]
  cat(sprintf("Computing factor %d/%d: %s\n", j, length(selected_features), feature))
  factor_parts[[j]] = compute_one_factor(feature)
}

factor_returns = rbindlist(factor_parts, use.names = TRUE, fill = TRUE)
setorder(factor_returns, feature, date)

out_file = file.path(PARTS_DIR, sprintf("factor_returns_part_%04d.csv", job_id))
fwrite(factor_returns, out_file)

cat(sprintf("Saved %s rows=%d features=%d\n", out_file, nrow(factor_returns), length(selected_features)))
