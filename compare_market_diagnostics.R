suppressPackageStartupMessages({
  library(data.table)
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

parse_utc = function(x) {
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

ny_key_shift1 = function(x) {
  shifted = parse_utc(x) + 3600
  local_text = format(shifted, tz = "America/New_York", usetz = FALSE)
  local_time = as.POSIXct(local_text, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
  data.table(
    trading_day = as.IDate(format(local_time, "%Y-%m-%d")),
    bar_time = format(local_time, "%H:%M:%S")
  )
}

PATH_MARKET_DIAG = env_chr("PATH_MARKET_DIAG", file.path("factor_returns_market_diagnostics", "factor_returns_wide.csv"))
PATH_ALETI = env_chr("PATH_ALETI", file.path("factor_returns_simple", "aleti_hourly_from_minute.csv"))
PATH_OUT = env_chr("PATH_OUT", file.path(dirname(PATH_MARKET_DIAG), "aleti_market_variant_comparison.csv"))

diag = fread(PATH_MARKET_DIAG, showProgress = FALSE)
diag_key = ny_key_shift1(diag$date)
diag_value_cols = setdiff(names(diag), c("trading_day", "bar_time"))
diag = cbind(diag_key, diag[, ..diag_value_cols])

aleti = fread(PATH_ALETI, showProgress = FALSE)
aleti_time = as.POSIXct(
  as.character(aleti$datetime),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "America/New_York"
)
aleti = data.table(
  trading_day = as.IDate(format(aleti_time, "%Y-%m-%d")),
  bar_time = format(aleti_time, "%H:%M:%S"),
  aleti_mkt = as.numeric(aleti$ff__mkt_hour)
)

matched = merge(aleti, diag, by = c("trading_day", "bar_time"))
variant_cols = grep("^mkt_", names(matched), value = TRUE)
variant_cols = setdiff(variant_cols, c("mkt_hhi"))

summary = rbindlist(lapply(variant_cols, function(col) {
  ok = is.finite(matched[[col]]) & is.finite(matched$aleti_mkt)
  x = matched[[col]][ok]
  y = matched$aleti_mkt[ok]
  data.table(
    series = col,
    n = sum(ok),
    corr = cor(x, y),
    beta_on_aleti = cov(x, y) / var(y),
    mean_diff = mean(x - y),
    sd_diff = sd(x - y),
    cumulative_return = compound_return(x),
    cumulative_aleti = compound_return(y),
    cumulative_diff = compound_return(x) - compound_return(y),
    sd_hour = sd(x),
    sd_aleti = sd(y),
    max_abs_hour = max(abs(x)),
    mean_n_all = mean(matched$n_all[ok], na.rm = TRUE),
    mean_n_mcap = mean(matched$n_mcap[ok], na.rm = TRUE),
    mean_top1_mcap_share = mean(matched$mcap_top1_share[ok], na.rm = TRUE)
  )
}), use.names = TRUE)
setorder(summary, -corr)

fwrite(summary, PATH_OUT)
cat("Wrote:", normalizePath(PATH_OUT, winslash = "/", mustWork = FALSE), "\n")
print(summary)
