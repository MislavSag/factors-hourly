#!/usr/bin/env bash
set -euo pipefail

VENV_DIR="${1:-${PADOBRAN_PYTHON_VENV:-$HOME/projects_py/pyquant}}"
REQUIREMENTS_FILE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/requirements-padobran.txt"

find_python() {
  if [[ -n "${PYTHON:-}" ]]; then
    command -v "$PYTHON"
    return
  fi

  for candidate in python3.11 python3.10 python3.12 python3; do
    if command -v "$candidate" >/dev/null 2>&1; then
      command -v "$candidate"
      return
    fi
  done

  return 1
}

PYTHON_BIN="$(find_python)" || {
  echo "No usable Python found. Load a Python module first or set PYTHON=/path/to/python." >&2
  exit 1
}

if [[ ! -f "$REQUIREMENTS_FILE" ]]; then
  echo "Missing requirements file: $REQUIREMENTS_FILE" >&2
  exit 1
fi

mkdir -p "$(dirname "$VENV_DIR")"

if [[ ! -x "$VENV_DIR/bin/python" ]]; then
  echo "Creating virtualenv: $VENV_DIR"
  "$PYTHON_BIN" -m venv "$VENV_DIR"
else
  echo "Using existing virtualenv: $VENV_DIR"
fi

# shellcheck disable=SC1091
source "$VENV_DIR/bin/activate"

python -m pip install --upgrade pip setuptools wheel
python -m pip install --upgrade -r "$REQUIREMENTS_FILE"

python - <<'PY'
import sys
import tsfel
import tsfresh

print("Python:", sys.executable)
print("tsfel:", getattr(tsfel, "__version__", "unknown"))
print("tsfresh:", getattr(tsfresh, "__version__", "unknown"))
PY

cat <<EOF

Virtualenv is ready.
Use this path in R:

  Sys.setenv(PADOBRAN_PYTHON_VENV = "$VENV_DIR")
  reticulate::use_virtualenv("$VENV_DIR", required = TRUE)

EOF
