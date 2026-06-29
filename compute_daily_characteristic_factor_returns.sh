#!/bin/bash

#PBS -N daily_char_factors
#PBS -l ncpus=4
#PBS -l mem=32GB
#PBS -l walltime=168:00:00
#PBS -o logs
#PBS -j oe

cd ${PBS_O_WORKDIR}

export PATH_PRICES=${PATH_PRICES:-prices_factors_hour}
export PATH_FACTORS=${PATH_FACTORS:-factor_returns_daily_characteristics}
export PATH_MARKET_CAP=${PATH_MARKET_CAP:-data/findata_market_cap/daily_market_cap_clean_sane_hourly.rds}
export DAILY_FEATURES=${DAILY_FEATURES:-MKT_MARKET_CAP_W,zero_trades_21d,zero_trades_126d,zero_trades_252d,turnover_proxy_126d,turnover_var_proxy_126d,rvol_21d,ivol_capm_252d,beta_tailrisk_proxy_252d,mom_21d,mom_126d,dollar_vol_126d,amihud_21d,hl_range_21d}

export DAILY_RETURN_COL=${DAILY_RETURN_COL:-returns_oc}
export DAILY_DROP_FIRST_BAR=${DAILY_DROP_FIRST_BAR:-1}
export DAILY_MARKET_CAP_LAG_DAYS=${DAILY_MARKET_CAP_LAG_DAYS:-1}
export DAILY_MAX_ABS_RETURN=${DAILY_MAX_ABS_RETURN:-0.2}
export DAILY_TAIL_PROB=${DAILY_TAIL_PROB:-0.10}
export DAILY_MIN_LEG_N=${DAILY_MIN_LEG_N:-10}
export DAILY_MARKET_TAIL_Z=${DAILY_MARKET_TAIL_Z:--1.0}
export ZERO_TRADE_MODE=${ZERO_TRADE_MODE:-volume_or_flat_close}
export DATA_TABLE_THREADS=${DATA_TABLE_THREADS:-${PBS_NCPUS:-4}}

apptainer run image.sif compute_daily_characteristic_factor_returns.R
