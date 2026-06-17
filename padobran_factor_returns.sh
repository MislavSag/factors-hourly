#!/bin/bash

#PBS -N factor_returns
#PBS -l ncpus=1
#PBS -l mem=12GB
#PBS -l walltime=168:00:00
#PBS -J 1-1000
#PBS -o logs
#PBS -j oe

cd ${PBS_O_WORKDIR}

export PATH_PRICES=${PATH_PRICES:-prices_factors_hour}
export PATH_PREDICTORS=${PATH_PREDICTORS:-hourly}
export PATH_FACTORS=${PATH_FACTORS:-factor_returns}

export FACTOR_RETURN_COL=${FACTOR_RETURN_COL:-returns_oc}
export FACTOR_WEIGHT_COL=${FACTOR_WEIGHT_COL:-}
export FACTOR_TAIL_PROB=${FACTOR_TAIL_PROB:-0.10}
export FACTOR_MIN_LEG_N=${FACTOR_MIN_LEG_N:-10}
export FACTOR_LAG_BARS=${FACTOR_LAG_BARS:-1}
export FACTOR_COLS_PER_JOB=${FACTOR_COLS_PER_JOB:-5}

apptainer run image.sif padobran_factor_returns.R
