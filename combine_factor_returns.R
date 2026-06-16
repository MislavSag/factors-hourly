suppressPackageStartupMessages({
  library(data.table)
})

env_chr = function(name, default) {
  value = Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

PATH_FACTORS = env_chr("PATH_FACTORS", "factor_returns")
WRITE_WIDE = env_chr("FACTOR_WRITE_WIDE", "1") %in% c("1", "true", "TRUE", "yes", "YES")

parts_dir = file.path(PATH_FACTORS, "parts")
part_files = list.files(parts_dir, pattern = "^factor_returns_part_.*\\.csv$", full.names = TRUE)
if (length(part_files) == 0L) {
  stop(sprintf("No factor return part files found in %s", parts_dir))
}
part_files = sort(part_files)

cat(sprintf("Combining %d factor return part files\n", length(part_files)))

factor_returns = rbindlist(
  lapply(part_files, fread, showProgress = FALSE),
  use.names = TRUE,
  fill = TRUE
)

factor_returns[, date := as.POSIXct(date, tz = "America/New_York")]
setorder(factor_returns, feature, date)
long_rows = nrow(factor_returns)
long_features = uniqueN(factor_returns$feature)

long_file = file.path(PATH_FACTORS, "factor_returns_long.csv")
fwrite(factor_returns, long_file)
saveRDS(factor_returns, file.path(PATH_FACTORS, "factor_returns_long.rds"))

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
fwrite(summary_dt, file.path(PATH_FACTORS, "factor_returns_summary.csv"))

if (WRITE_WIDE) {
  wide_input = factor_returns[, .(
    date,
    trading_day,
    bar_time,
    is_first_bar,
    feature,
    factor_ret
  )]
  rm(factor_returns)
  invisible(gc())

  wide = dcast(
    wide_input,
    date + trading_day + bar_time + is_first_bar ~ feature,
    value.var = "factor_ret"
  )
  rm(wide_input)
  invisible(gc())

  setorder(wide, date)
  wide_file = file.path(PATH_FACTORS, "factor_returns_wide.csv")
  fwrite(wide, wide_file)
  saveRDS(wide, file.path(PATH_FACTORS, "factor_returns_wide.rds"))
  cat(sprintf("Saved wide factor matrix: %s rows=%d cols=%d\n", wide_file, nrow(wide), ncol(wide)))
  rm(wide)
  invisible(gc())
} else {
  rm(factor_returns)
  invisible(gc())
}

cat(sprintf(
  "Saved long factor returns: %s rows=%d features=%d\n",
  long_file,
  long_rows,
  long_features
))

quit(save = "no", status = 0L, runLast = FALSE)
