library(data.table)
library(finutils)
library(ggplot2)


# Setup
PATH_LEAN = "C:/Users/Mislav/qc_snp/data"
PATH_PRICES = "D:/predictors"
PATH_DATA = "F:/data/equity/us"

# fs::dir_delete(PATH_PRICES)
if (!dir.exists(PATH_PRICES)) dir.create(PATH_PRICES, recursive = TRUE)

# Coarse universe
coarse_universe = coarse(
  min_mean_mon_price = 5,
  min_mean_mon_volume = 150000,
  dollar_vol_n = 4000,
  file_path = file.path(PATH_LEAN, "all_stocks_daily"),
  min_obs = 2*252,
  price_threshold = 1e-008,
  duplicates = "fast",
  add_dv_rank = FALSE,
  add_day_of_month = FALSE,
  etfs = FALSE,
  profiles_fmp_file = "F:/data/equity/us/fundamentals/prfiles.parquet",
  min_last_mon_mcap = file.path(PATH_DATA, "fundamentals", "market_cap.parquet")
)

# Summary statistics
coarse_universe[, length(unique(symbol))]
coarse_universe[, .N, by = date] |>
  ggplot(aes(date, N)) + geom_line() +
  labs(title = "Number of stocks in uvnierse through time")
coarse_universe[, mean(volume), by = date] |>
  _[order(date)] |>
  _[, roll_vol_mean := frollmean(V1, 22)] |>
  ggplot(aes(date, roll_vol_mean)) + geom_line() +
  labs(title = "Mean volume")
coarse_universe[, mean(dollar_vol), by = date] |>
  _[order(date)] |>
  _[, roll_dolvol_mean := frollmean(V1, 22)] |>
  ggplot(aes(date, roll_dolvol_mean)) + geom_line() +
  labs(title = "Mean volume")
coarse_universe[, mean(close_raw), by = date] |>
  _[order(date)] |>
  ggplot(aes(date, V1)) + geom_line() +
  labs(title = "Mean close raw")

# Symbols to keep
symbols = coarse_universe[, sort(unique(symbol))]

# Free memory
rm(coarse_universe)

# Prices data
prices = qc_hour_parquet(
  file_path = file.path(PATH_LEAN, "all_stocks_hour"),,
  symbols = symbols,
  min_cross_section_n = 50,
  etfs = FALSE,
  min_obs = 252*6, # we can use 2 years of data + some lookback
  duplicates = "fast",
  add_dv_rank = FALSE
)

# Remove columns we dont need
remove_cols = c("inv_vehicle")
prices = prices[, .SD, .SDcols = -remove_cols]

# Split symbols to 10000 chunk eleemnts
symbols_chunks = prices[, unique(symbol)]
length(symbols_chunks)
symbols_chunks = split(symbols_chunks, ceiling(seq_along(symbols_chunks) / (length(symbols_chunks) / 10000)))
length(symbols_chunks)
lengths(symbols_chunks)
symbols_chunks = rbindlist(lapply(symbols_chunks, as.data.table), idcol = "id")
setnames(symbols_chunks, c("id", "symbol"))

# Merge symbols ids and pricers
prices = symbols_chunks[prices, on = "symbol"]

# Save every symbol separately
prices_dir = file.path(PATH_PRICES, "prices_factors_hour")
if (!dir.exists(prices_dir)) dir.create(prices_dir)
for (i in prices[, unique(id)]) {
  prices_ = prices[id == i]
  if (nrow(prices_) < 22) next
  file_name = file.path(prices_dir, paste0("prices_", i, ".csv"))
  fwrite(prices_, file_name)
}

# Create sh file for predictors
cont = sprintf(
  "#!/bin/bash

#PBS -N predictors
#PBS -l ncpus=1
#PBS -l mem=5GB
#PBS -J 1-%d
#PBS -o logs
#PBS -j oe

cd ${PBS_O_WORKDIR}

apptainer run image.sif padobran_predictors.R",
  length(list.files(prices_dir)))
writeLines(cont, "padobran_predictors.sh")

# Add to padobran
# scp -r /home/sn/data/strategies/factors/prices padobran:/home/jmaric/factors/prices

# Add file to padobran
# cd C:\Users\Mislav\projects_r\alpha_erf
# scp.exe -r .\data padobran:/home/jmaric/alpha_erf/
# scp -r /home/sn/projects_r/alpha_erf/data padobran:/home/jmaric/alpha_erf/data/
