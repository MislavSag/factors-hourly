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

## Daily characteristic factor returns

The cheaper Padobran workflow aggregates hourly symbol files to one row per
symbol/day, computes daily characteristics, and writes daily factor returns:

```bash
qsub compute_daily_characteristic_factor_returns.sh
```

Main outputs are written to `factor_returns_daily_characteristics/`:

- `factor_returns_long.csv`
- `factor_returns_wide.csv`
- `factor_returns_summary.csv`
- `daily_panel_sample.csv`

Defaults use `prices_factors_hour`, findata market-cap weights, `returns_oc`,
drop the first intraday bar, cap individual hourly returns at 20%, and compute
market, zero-trade, turnover, volatility, momentum, dollar-volume, Amihud, and
high-low range factors. Set `DAILY_FEATURES`, `PATH_FACTORS`, `DAILY_MAX_FILES`,
or `FORCE=1` to override a run.
