library(data.table)
library(finutils)
library(qlcal)
library(lubridate)
library(roll)


# Setup
setCalendar("UnitedStates/NYSE")
PATH_LEAN = "C:/Users/Mislav/qc_snp/data"

# Prices data
prices = qc_hour_parquet(
  file_path = file.path(PATH_LEAN, "all_stocks_hour"),
  etfs = FALSE,
  # etf_cons = file.path(PATH_LEAN, "equity", "usa", "universes", c("spy", "iwm")),
  min_obs = 7*125,
  duplicates = "fast",
  add_dv_rank = FALSE,
  min_cross_section_n = 300
)


# MINMAX ---------------------------------------

# Target effective sample size used to calibrate EW weights.
# This is not the raw rolling window length; it is the number of equally weighted
# observations that would contain roughly the same information as the EW window.
TARGET_EFF_N = 2500

# Desired effective number of observations in the quantile tail.
# Example: if MINMAX_PROB ends up at 0.99, this targets about 25 effective
# observations beyond the 99th percentile.
TARGET_TAIL_EFF_OBS = 25

# Desired expected number of cross-sectional exceeders per hour.
# This prevents the signal from being driven by only a few stocks in each bar.
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

# PRA ------------------------------------------

# EW-PRA is an exponentially weighted percent rank of each stock's close within
# its own history. The old PRA project used fixed windows; here the horizons are
# derived from target effective sample sizes, similar to the minmax calibration.
if (!requireNamespace("Rcpp", quietly = TRUE)) {
  stop("Package 'Rcpp' is required for EW-PRA.")
}

Rcpp::sourceCpp(code = '
#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <cmath>
using namespace Rcpp;

class FenwickTree {
private:
  int size_;
  std::vector<double> tree_;

public:
  FenwickTree(int size) : size_(size), tree_(size + 1, 0.0) {}

  void add(int index, double value) {
    for (int i = index; i <= size_; i += i & -i) {
      tree_[i] += value;
    }
  }

  double sum(int index) const {
    double total = 0.0;
    for (int i = index; i > 0; i -= i & -i) {
      total += tree_[i];
    }
    return total;
  }

  void scale(double value) {
    for (int i = 1; i <= size_; ++i) {
      tree_[i] *= value;
    }
  }
};

// [[Rcpp::export]]
NumericMatrix ew_percent_rank_multi_cpp(NumericVector x,
                                        IntegerVector widths,
                                        NumericVector alphas,
                                        IntegerVector min_obs) {
  const int n = x.size();
  const int k = widths.size();

  if (alphas.size() != k || min_obs.size() != k) {
    Rcpp::stop("widths, alphas, and min_obs must have the same length");
  }

  NumericMatrix out(n, k);
  std::fill(out.begin(), out.end(), NA_REAL);

  std::vector<double> values;
  values.reserve(n);
  for (int i = 0; i < n; ++i) {
    const double value = x[i];
    if (R_finite(value)) {
      values.push_back(value);
    }
  }

  if (values.empty()) {
    return out;
  }

  std::sort(values.begin(), values.end());
  values.erase(std::unique(values.begin(), values.end()), values.end());

  const int m = values.size();
  std::vector<int> index_by_time(n, 0);

  for (int i = 0; i < n; ++i) {
    const double value = x[i];
    if (R_finite(value)) {
      index_by_time[i] = std::lower_bound(values.begin(), values.end(), value) - values.begin() + 1;
    }
  }

  for (int h = 0; h < k; ++h) {
    const int width = widths[h];
    const int min_obs_h = min_obs[h];
    const double alpha = alphas[h];

    if (width < 1) {
      Rcpp::stop("width must be positive");
    }
    if (min_obs_h < 1) {
      Rcpp::stop("min_obs must be positive");
    }
    if (!(alpha > 0.0 && alpha <= 1.0)) {
      Rcpp::stop("alpha must be in (0, 1]");
    }

    FenwickTree tree(m);
    std::vector<double> weight_by_time(n, 0.0);
    int obs_count = 0;
    double current_weight = 1.0;
    const double inv_alpha = 1.0 / alpha;

    for (int t = 0; t < n; ++t) {
      const int index = index_by_time[t];

      if (index > 0) {
        weight_by_time[t] = current_weight;
        tree.add(index, current_weight);
        obs_count += 1;
      }

      const int old = t - width;
      if (old >= 0 && index_by_time[old] > 0) {
        tree.add(index_by_time[old], -weight_by_time[old]);
        obs_count -= 1;
      }

      if (index > 0 && obs_count >= min_obs_h) {
        const double denominator = tree.sum(m);
        if (denominator > 0.0) {
          double rank = tree.sum(index) / denominator;
          if (rank < 0.0) rank = 0.0;
          if (rank > 1.0) rank = 1.0;
          out(t, h) = rank;
        }
      }

      current_weight *= inv_alpha;

      if (!R_finite(current_weight) || current_weight > 1e100) {
        tree.scale(1e-50);
        for (int j = 0; j <= t; ++j) {
          weight_by_time[j] *= 1e-50;
        }
        current_weight *= 1e-50;
      }
    }
  }

  return out;
}
')

# Main EW-PRA memory target.
# This is the central effective sample size, not a raw rolling window length.
# It says how many equally weighted hourly closes would contain roughly the same
# information as the central exponentially weighted PRA horizon.
PRA_TARGET_EFF_N = 7L * 252L * 2L

# Width of the horizon ensemble around PRA_TARGET_EFF_N.
# With spread = 2 and target = 3528, the model uses horizons centered around
# target / 2, target, and target * 2. This reduces dependence on one horizon.
PRA_HORIZON_SPREAD = 2

# Number of EW-PRA horizons in the ensemble.
# With 3 horizons and spread = 2, this creates short / medium / long PRA views.
PRA_N_HORIZONS = 3L

# Desired expected number of stocks in each PRA tail per hourly bar.
# This controls the extreme threshold in a cross-section-aware way. For example,
# if the median universe has 1000 stocks and this is 10, the tail prob is 1%.
PRA_TARGET_CS_EXTREMES = 10

# Lower bound for the PRA tail probability.
# Prevents the threshold from becoming too extreme when the universe is large.
PRA_MIN_TAIL_PROB = 0.001

# Upper bound for the PRA tail probability.
# Prevents the threshold from becoming too loose when the universe is small.
PRA_MAX_TAIL_PROB = 0.05

# Minimum amount of raw history required before returning EW-PRA.
# Example: 0.5 means at least half of the target effective N is needed, subject
# to the hard lower floor below. This avoids unstable early-history ranks.
PRA_MIN_OBS_FRACTION = 0.5

if (PRA_N_HORIZONS <= 1L) {
  PRA_EFF_N = as.integer(round(PRA_TARGET_EFF_N))
} else {
  PRA_EFF_N = as.integer(round(exp(seq(
    log(PRA_TARGET_EFF_N / PRA_HORIZON_SPREAD),
    log(PRA_TARGET_EFF_N * PRA_HORIZON_SPREAD),
    length.out = PRA_N_HORIZONS
  ))))
}

# Effective sample sizes actually used by the EW-PRA ensemble.
# These are derived from target/spread/n_horizons and are the main horizon labels.
PRA_EFF_N = sort(unique(pmax(PRA_EFF_N, 10L)))

# EW decay parameter for each PRA horizon.
# Larger alpha means slower decay and more memory. It is derived from PRA_EFF_N,
# so it should usually not be set directly.
PRA_ALPHA = (PRA_EFF_N - 1) / (PRA_EFF_N + 1)

# Half-life for each PRA horizon, in hourly observations.
# After this many observations, an old close has half the weight of a new close.
PRA_HALF_LIFE = ceiling(log(0.5) / log(PRA_ALPHA))

# Raw rolling cutoff used for computation.
# Six half-lives keeps almost all useful EW mass while dropping very old data.
PRA_WIDTH = ceiling(6 * PRA_HALF_LIFE)

# Minimum raw observations required before EW-PRA is allowed to be non-NA.
# Uses the larger of a 6-month hourly floor and PRA_MIN_OBS_FRACTION * PRA_EFF_N,
# capped at PRA_WIDTH.
PRA_MIN_OBS = pmin(PRA_WIDTH, pmax(252L * 6L, ceiling(PRA_MIN_OBS_FRACTION * PRA_EFF_N)))

# Cross-section-calibrated PRA tail probability.
# This is the expected tail count divided by the median number of stocks, then
# clamped by PRA_MIN_TAIL_PROB and PRA_MAX_TAIL_PROB.
PRA_TAIL_PROB = PRA_TARGET_CS_EXTREMES / MEDIAN_CROSS_SECTION_N
PRA_TAIL_PROB = max(min(PRA_TAIL_PROB, PRA_MAX_TAIL_PROB), PRA_MIN_TAIL_PROB)

# Lower PRA threshold. A stock is in the lower tail when EW-PRA < this value.
PRA_LOWER_PROB = PRA_TAIL_PROB

# Upper PRA threshold. A stock is in the upper tail when EW-PRA > this value.
PRA_UPPER_PROB = 1 - PRA_TAIL_PROB

setorder(prices, symbol, date)

# Raw EW-PRA rank columns, one per effective horizon.
# Values are between 0 and 1: low values mean the current close is low versus
# its weighted history; high values mean the current close is high versus history.
pra_cols = paste0("ew_pra_eff_", PRA_EFF_N)
prices[, (pra_cols) := as.data.table(ew_percent_rank_multi_cpp(
  close,
  widths = PRA_WIDTH,
  alphas = PRA_ALPHA,
  min_obs = PRA_MIN_OBS
)), by = symbol]

# Labels used only in column names. Example: 0.0100 becomes "0100".
lower_label = sprintf("%04d", as.integer(round(PRA_LOWER_PROB * 10000)))
upper_label = sprintf("%04d", as.integer(round(PRA_UPPER_PROB * 10000)))

# Upper-tail dummy columns: 1 when a stock is unusually high versus its EW
# price history for that horizon, otherwise 0.
pra_above_cols = paste0("ew_pra_above_", upper_label, "_eff_", PRA_EFF_N)

# Lower-tail dummy columns: 1 when a stock is unusually low versus its EW price
# history for that horizon, otherwise 0.
pra_below_cols = paste0("ew_pra_below_", lower_label, "_eff_", PRA_EFF_N)

# Directional tail-breadth columns: +1 for upper-tail, -1 for lower-tail, 0
# otherwise. These are often the most interpretable PRA market-state signals.
pra_net_cols = paste0("ew_pra_net_", lower_label, "_", upper_label, "_eff_", PRA_EFF_N)

prices[, (pra_above_cols) := lapply(.SD, function(x) as.integer(x > PRA_UPPER_PROB)),
       .SDcols = pra_cols]
prices[, (pra_below_cols) := lapply(.SD, function(x) as.integer(x < PRA_LOWER_PROB)),
       .SDcols = pra_cols]
prices[, (pra_net_cols) := Map(function(above, below) above - below,
                               .SD[, ..pra_above_cols],
                               .SD[, ..pra_below_cols])]

# All per-symbol PRA columns that will be aggregated cross-sectionally by date.
pra_signal_cols = c(pra_cols, pra_above_cols, pra_below_cols, pra_net_cols)

# Cross-sectional means: normalized signals that are less sensitive to changing
# number of stocks in the universe.
pra_indicators_mean = prices[, lapply(.SD, mean, na.rm = TRUE),
                             by = date,
                             .SDcols = pra_signal_cols]
setnames(pra_indicators_mean, c("date", paste0("mean_", names(pra_indicators_mean)[-1])))

# Cross-sectional standard deviations for raw PRA ranks only. For dummy/net
# columns, sd is mostly redundant with the mean, so it is intentionally skipped.
pra_indicators_sd = prices[, lapply(.SD, sd, na.rm = TRUE),
                           by = date,
                           .SDcols = pra_cols]
setnames(pra_indicators_sd, c("date", paste0("sd_", names(pra_indicators_sd)[-1])))

pra_indicators_meta = prices[, .(
  trading_day = data.table::first(trading_day),
  bar_time = data.table::first(bar_time),
  is_first_bar = any(is_first_bar),
  n = .N
), by = date]

pra_indicators = Reduce(
  function(x, y) merge(x, y, by = "date", all = TRUE),
  list(pra_indicators_meta, pra_indicators_mean, pra_indicators_sd)
)
setorder(pra_indicators, date)

# Per-horizon audit table. This is useful for checking exactly which EW decay,
# half-life, raw width, and min_obs correspond to each saved PRA column.
pra_horizon_params = data.table(
  pra_col = pra_cols,
  pra_eff_n = PRA_EFF_N,
  pra_alpha = PRA_ALPHA,
  pra_half_life = PRA_HALF_LIFE,
  pra_width = PRA_WIDTH,
  pra_min_obs = PRA_MIN_OBS
)

# Single-row parameter summary saved with the indicators for reproducibility.
pra_params = data.table(
  pra_method = "ew_percent_rank",
  pra_target_eff_n = PRA_TARGET_EFF_N,
  pra_horizon_spread = PRA_HORIZON_SPREAD,
  pra_requested_n_horizons = PRA_N_HORIZONS,
  pra_n_horizons = length(PRA_EFF_N),
  pra_target_cs_extremes = PRA_TARGET_CS_EXTREMES,
  pra_min_tail_prob = PRA_MIN_TAIL_PROB,
  pra_max_tail_prob = PRA_MAX_TAIL_PROB,
  pra_tail_prob = PRA_TAIL_PROB,
  pra_lower_prob = PRA_LOWER_PROB,
  pra_upper_prob = PRA_UPPER_PROB,
  pra_min_obs_fraction = PRA_MIN_OBS_FRACTION,
  pra_eff_n = paste(PRA_EFF_N, collapse = ","),
  pra_half_life = paste(PRA_HALF_LIFE, collapse = ","),
  pra_width = paste(PRA_WIDTH, collapse = ","),
  pra_min_obs = paste(PRA_MIN_OBS, collapse = ","),
  pra_n_signal_cols = length(pra_signal_cols),
  pra_n_net_cols = length(pra_net_cols)
)


# Save the full per-symbol table with minmax/PRA columns, plus separate
# aggregated market-state indicator tables for downstream analysis.
OUTPUT_DIR = file.path("data", "derived")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

market_state_indicators = merge(
  minmax_indicators,
  pra_indicators,
  by = c("date", "trading_day", "bar_time", "is_first_bar", "n"),
  all = TRUE,
  suffixes = c("_minmax", "_pra")
)
setorder(market_state_indicators, date)

saveRDS(prices, file.path(OUTPUT_DIR, "prices_with_indicators.rds"))
saveRDS(minmax_indicators, file.path(OUTPUT_DIR, "minmax_indicators.rds"))
saveRDS(minmax_params, file.path(OUTPUT_DIR, "minmax_params.rds"))
saveRDS(pra_indicators, file.path(OUTPUT_DIR, "pra_indicators.rds"))
saveRDS(pra_params, file.path(OUTPUT_DIR, "pra_params.rds"))
saveRDS(pra_horizon_params, file.path(OUTPUT_DIR, "pra_horizon_params.rds"))
saveRDS(market_state_indicators, file.path(OUTPUT_DIR, "market_state_indicators.rds"))
