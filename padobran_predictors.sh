#!/bin/bash

#PBS -N predictors
#PBS -l ncpus=1
#PBS -l mem=5GB
#PBS -l walltime=168:00:00
#PBS -J 1-10000
#PBS -o logs
#PBS -j oe

cd ${PBS_O_WORKDIR}

apptainer run image.sif padobran_predictors.R
