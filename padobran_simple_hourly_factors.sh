#!/bin/bash

#PBS -N simple_hourly_factors
#PBS -l ncpus=1
#PBS -l mem=24GB
#PBS -l walltime=168:00:00
#PBS -o logs
#PBS -j oe

cd ${PBS_O_WORKDIR}

export PATH_PRICES=${PATH_PRICES:-prices_factors_hour}
export PATH_FACTORS=${PATH_FACTORS:-factor_returns_simple}

export SIMPLE_RETURN_COL=${SIMPLE_RETURN_COL:-returns_oc}
export SIMPLE_WINDOWS=${SIMPLE_WINDOWS:-22,147,441}
export SIMPLE_TAIL_PROB=${SIMPLE_TAIL_PROB:-0.10}
export SIMPLE_MIN_LEG_N=${SIMPLE_MIN_LEG_N:-10}
export SIMPLE_MIN_SYMBOL_ROWS=${SIMPLE_MIN_SYMBOL_ROWS:-50}
export SIMPLE_WRITE_LONG_CSV=${SIMPLE_WRITE_LONG_CSV:-1}
export SIMPLE_WRITE_RDS=${SIMPLE_WRITE_RDS:-1}

apptainer run image.sif padobran_simple_hourly_factors.R
