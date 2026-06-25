#!/bin/bash

#PBS -N mcap_market_factor
#PBS -l ncpus=1
#PBS -l mem=16GB
#PBS -l walltime=168:00:00
#PBS -o logs
#PBS -j oe

cd ${PBS_O_WORKDIR}

export PATH_PRICES=${PATH_PRICES:-prices_factors_hour}
export PATH_MARKET_CAP=${PATH_MARKET_CAP:-data/eodhd_market_cap/daily_market_cap.rds}
export PATH_OUT=${PATH_OUT:-factor_returns_market_cap_only/factor_returns_wide.csv}
export SIMPLE_RETURN_COL=${SIMPLE_RETURN_COL:-returns_oc}
export SIMPLE_DROP_FIRST_BAR=${SIMPLE_DROP_FIRST_BAR:-1}
export SIMPLE_MARKET_CAP_LAG_DAYS=${SIMPLE_MARKET_CAP_LAG_DAYS:-1}

apptainer run image.sif compute_market_cap_market_factor.R
