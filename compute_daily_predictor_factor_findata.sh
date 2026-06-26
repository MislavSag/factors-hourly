#!/bin/bash

#PBS -N daily_findata
#PBS -l ncpus=1
#PBS -l mem=24GB
#PBS -l walltime=168:00:00
#PBS -J 1-9
#PBS -o logs
#PBS -j oe

cd ${PBS_O_WORKDIR}

export PATH_PRICES=${PATH_PRICES:-prices_factors_hour}
export PATH_FACTORS=${PATH_FACTORS:-factor_returns_daily_findata_sane_retcap20}
export PATH_MARKET_CAP=${PATH_MARKET_CAP:-data/findata_market_cap/daily_market_cap_clean_sane_hourly.rds}
export DAILY_FEATURES=${DAILY_FEATURES:-MKT_MARKET_CAP_W,zero_trades_21d,zero_trades_126d,zero_trades_252d,turnover_proxy_126d,turnover_var_proxy_126d,ivol_capm_252d,beta_tailrisk_proxy_252d,rvol_21d}

export SIMPLE_RETURN_COL=${SIMPLE_RETURN_COL:-returns_oc}
export SIMPLE_DROP_FIRST_BAR=${SIMPLE_DROP_FIRST_BAR:-1}
export SIMPLE_MARKET_CAP_LAG_DAYS=${SIMPLE_MARKET_CAP_LAG_DAYS:-1}
export SIMPLE_MAX_ABS_RETURN=${SIMPLE_MAX_ABS_RETURN:-0.2}
export SIMPLE_TAIL_PROB=${SIMPLE_TAIL_PROB:-0.10}
export SIMPLE_MIN_LEG_N=${SIMPLE_MIN_LEG_N:-10}
export SIMPLE_MARKET_TAIL_Z=${SIMPLE_MARKET_TAIL_Z:--1.0}

apptainer run image.sif compute_daily_predictor_factor_findata.R
