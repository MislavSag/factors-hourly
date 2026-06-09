param(
  [string]$VenvDir = (Join-Path $PSScriptRoot ".venv"),
  [string]$RequirementsFile = (Join-Path $PSScriptRoot "requirements-python.txt"),
  [string]$Python = ""
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

function Get-PythonSpec {
  if ($Python -ne "") {
    return @{
      Command = $Python
      Args = @()
    }
  }

  $candidates = @(
    @{ Command = "py"; Args = @("-3.10") },
    @{ Command = "py"; Args = @("-3.12") },
    @{ Command = "py"; Args = @("-3.11") },
    @{ Command = "py"; Args = @("-3") },
    @{ Command = "python"; Args = @() }
  )

  foreach ($candidate in $candidates) {
    if ($null -eq (Get-Command $candidate.Command -ErrorAction SilentlyContinue)) {
      continue
    }

    $command = $candidate.Command
    $args = @($candidate.Args) + @("-c", "import sys")
    $previousErrorActionPreference = $ErrorActionPreference
    $ErrorActionPreference = "Continue"

    try {
      & $command @args *> $null

      if ($LASTEXITCODE -eq 0) {
        return $candidate
      }
    } catch {
      # Candidate exists but requested Python runtime is unavailable.
    } finally {
      $ErrorActionPreference = $previousErrorActionPreference
    }
  }

  throw "No usable Python found. Install Python or pass -Python C:\path\to\python.exe."
}

if (-not (Test-Path $RequirementsFile)) {
  throw "Missing requirements file: $RequirementsFile"
}

$venvParent = Split-Path -Parent $VenvDir
if ($venvParent -ne "" -and -not (Test-Path $venvParent)) {
  New-Item -ItemType Directory -Path $venvParent | Out-Null
}

$pythonSpec = Get-PythonSpec
$pythonCommand = $pythonSpec.Command
$pythonArgs = @($pythonSpec.Args)
$venvPython = Join-Path $VenvDir "Scripts\python.exe"

if (-not (Test-Path $venvPython)) {
  Write-Host "Creating virtualenv: $VenvDir"
  & $pythonCommand @($pythonArgs + @("-m", "venv", $VenvDir))

  if ($LASTEXITCODE -ne 0) {
    throw "Python venv creation failed."
  }
} else {
  Write-Host "Using existing virtualenv: $VenvDir"
}

& $venvPython -m pip install --upgrade pip setuptools wheel
if ($LASTEXITCODE -ne 0) {
  throw "pip upgrade failed."
}

& $venvPython -m pip install --upgrade -r $RequirementsFile
if ($LASTEXITCODE -ne 0) {
  throw "requirements install failed."
}

& $venvPython -c "import sys, tsfel, tsfresh; print('Python:', sys.executable); print('tsfel:', getattr(tsfel, '__version__', 'unknown')); print('tsfresh:', getattr(tsfresh, '__version__', 'unknown'))"
if ($LASTEXITCODE -ne 0) {
  throw "Python import check failed."
}

$resolvedVenv = (Resolve-Path $VenvDir).Path
$rVenvPath = $resolvedVenv.Replace("\", "/")

Write-Host ""
Write-Host "Virtualenv is ready."
Write-Host "R path:"
Write-Host "  Sys.setenv(FACTORS_PYTHON_VENV = `"$rVenvPath`")"
