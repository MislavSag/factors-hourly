#!/bin/bash

#PBS -N ai_predictors
#PBS -l ncpus=1
#PBS -l mem=6GB
#PBS -J 1-10000
#PBS -o logs
#PBS -j oe

cd ${PBS_O_WORKDIR}

export PATH_PRICES=${PATH_PRICES:-prices_factors_hour}
export PATH_PREDICTORS_AI=${PATH_PREDICTORS_AI:-hourly_ai}

export AI_PROFILE=${AI_PROFILE:-alpha_v1}
export AI_FEATURE_SET=${AI_FEATURE_SET:-alpha_core}
export AI_CROSS_SECTIONAL=${AI_CROSS_SECTIONAL:-rank}
export AI_RETURN_COL=${AI_RETURN_COL:-returns_intraday}

apptainer run image.sif padobran_aifinfeatures.R
