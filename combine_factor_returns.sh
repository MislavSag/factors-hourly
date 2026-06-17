#!/bin/bash

#PBS -N combine_factor_returns
#PBS -l ncpus=1
#PBS -l mem=24GB
#PBS -l walltime=168:00:00
#PBS -o logs
#PBS -j oe

cd ${PBS_O_WORKDIR}

export PATH_FACTORS=${PATH_FACTORS:-factor_returns}
export FACTOR_WRITE_WIDE=${FACTOR_WRITE_WIDE:-1}

apptainer run image.sif combine_factor_returns.R
