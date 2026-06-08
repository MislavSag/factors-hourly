# factors-hourly

## Local Python virtualenv

Create the Python environment on Windows with:

```powershell
.\setup_local_venv.ps1
```

By default this creates `.venv` in this project. `padobran_predictors.R` uses that environment in interactive/local runs, unless `FACTORS_PYTHON_VENV` is set.

For a custom local environment:

```powershell
.\setup_local_venv.ps1 -VenvDir "D:\predictors\pyquant"
$env:FACTORS_PYTHON_VENV = "D:\predictors\pyquant"
```

Non-interactive Apptainer/PBS runs still default to `/opt/venv`.
