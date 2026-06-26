#!/bin/bash

#PBS -N zero_flat
#PBS -l ncpus=1
#PBS -l mem=16GB
#PBS -l walltime=168:00:00
#PBS -J 1-3
#PBS -o logs
#PBS -j oe

cd ${PBS_O_WORKDIR}

export PATH_PRICES=${PATH_PRICES:-prices_factors_hour}
export PATH_FACTORS=${PATH_FACTORS:-factor_returns_zero_trades_flatclose}
export PATH_MARKET_CAP=${PATH_MARKET_CAP:-data/findata_market_cap/daily_market_cap_clean_sane_hourly.rds}
export DAILY_FEATURES=${DAILY_FEATURES:-zero_trades_21d,zero_trades_126d,zero_trades_252d}

export SIMPLE_RETURN_COL=${SIMPLE_RETURN_COL:-returns_oc}
export SIMPLE_DROP_FIRST_BAR=${SIMPLE_DROP_FIRST_BAR:-1}
export SIMPLE_MARKET_CAP_LAG_DAYS=${SIMPLE_MARKET_CAP_LAG_DAYS:-1}
export SIMPLE_MAX_ABS_RETURN=${SIMPLE_MAX_ABS_RETURN:-0.2}
export SIMPLE_TAIL_PROB=${SIMPLE_TAIL_PROB:-0.10}
export SIMPLE_MIN_LEG_N=${SIMPLE_MIN_LEG_N:-10}
export ZERO_TRADE_MODE=${ZERO_TRADE_MODE:-flat_close}
export ZERO_TRADE_CLOSE_EPS=${ZERO_TRADE_CLOSE_EPS:-1e-10}
export ZERO_TRADE_MIN_FLAT_BARS=${ZERO_TRADE_MIN_FLAT_BARS:-2}

apptainer run image.sif compute_daily_predictor_factor_findata.R
