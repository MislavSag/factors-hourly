# factors-hourly

## Padobran Python virtualenv

Create the Python environment on Padobran with:

```bash
cd ~/factors-hourly
bash setup_padobran_venv.sh
```

By default this creates:

```bash
$HOME/projects_py/pyquant
```

`padobran_predictors.R` uses that virtualenv when running interactively. To force the same environment for a non-interactive `Rscript` run:

```bash
export PADOBRAN_PYTHON_VENV="$HOME/projects_py/pyquant"
Rscript padobran_predictors.R
```

The Apptainer/PBS path still defaults to `/opt/venv` unless `PADOBRAN_PYTHON_VENV` is set.
