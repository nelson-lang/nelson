# Nelson Engine API for Python

`nelson.engine` starts or connects to Nelson from Python and exposes Nelson
functions as Python methods.

```python
import nelson.engine

eng = nelson.engine.start_nelson()
print(eng.sqrt(4.0))
eng.quit()
```

## Install

From a Nelson source or build tree:

```powershell
python -m pip install .\modules\python_engine\resources\python
```

For offline build environments where `setuptools` and `wheel` are already
installed:

```powershell
python -m pip install --no-build-isolation .\modules\python_engine\resources\python
```

When using the package outside the Nelson tree, set `NELSON_ROOT` to the Nelson
installation or build root. You can also set `NELSON_ENGINE_LIBRARY` to the full
path of the native engine library.

## Optional Dependencies

NumPy and pandas are optional. They enable higher fidelity conversion for arrays
and tables:

```powershell
python -m pip install "nelson[all]"
```

## Public API

The Python API uses Nelson names only:

- `nelson.engine.start_nelson(...)`
- `nelson.engine.connect_nelson(...)`
- `nelson.engine.find_nelson()`
- `nelson.engine.NelsonEngine`
- `nelson.engine.FutureResult`

MATLAB public names such as `start_matlab` are intentionally not provided.
