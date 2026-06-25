#!/bin/bash

#PBS -N market_xcontrib
#PBS -l ncpus=1
#PBS -l mem=16GB
#PBS -l walltime=04:00:00
#PBS -o logs
#PBS -j oe

cd ${PBS_O_WORKDIR}

export PATH_PRICES=${PATH_PRICES:-prices_factors_hour}
export PATH_MARKET_CAP=${PATH_MARKET_CAP:-data/eodhd_market_cap/daily_market_cap.rds}
export PATH_TARGETS=${PATH_TARGETS:-diagnostics/market_gap/top_residual_hours.csv}
export PATH_OUT=${PATH_OUT:-factor_returns_market_diagnostics/extreme_contributors.csv}
export PATH_SUMMARY_OUT=${PATH_SUMMARY_OUT:-factor_returns_market_diagnostics/extreme_contributors_summary.csv}
export SIMPLE_MARKET_CAP_LAG_DAYS=${SIMPLE_MARKET_CAP_LAG_DAYS:-1}
export TARGET_TOP_N=${TARGET_TOP_N:-25}
export TOP_CONTRIBUTORS=${TOP_CONTRIBUTORS:-20}

apptainer run image.sif diagnose_market_extreme_contributors.R
