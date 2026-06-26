#!/bin/bash

#PBS -N rvol_findata
#PBS -l ncpus=1
#PBS -l mem=16GB
#PBS -l walltime=168:00:00
#PBS -o logs
#PBS -j oe

cd ${PBS_O_WORKDIR}

export PATH_PRICES=${PATH_PRICES:-prices_factors_hour}
export PATH_FACTORS=${PATH_FACTORS:-factor_returns_rvol_findata_sane}
export PATH_MARKET_CAP=${PATH_MARKET_CAP:-data/findata_market_cap/daily_market_cap_clean_sane_hourly.rds}
export SIMPLE_RETURN_COL=${SIMPLE_RETURN_COL:-returns_oc}
export SIMPLE_DROP_FIRST_BAR=${SIMPLE_DROP_FIRST_BAR:-1}
export SIMPLE_MARKET_CAP_LAG_DAYS=${SIMPLE_MARKET_CAP_LAG_DAYS:-1}
export SIMPLE_RVOL_WINDOW_DAYS=${SIMPLE_RVOL_WINDOW_DAYS:-21}
export SIMPLE_TAIL_PROB=${SIMPLE_TAIL_PROB:-0.10}
export SIMPLE_MIN_LEG_N=${SIMPLE_MIN_LEG_N:-10}

apptainer run image.sif compute_rvol_factor_findata.R
