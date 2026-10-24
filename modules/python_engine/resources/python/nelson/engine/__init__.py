# Copyright (c) 2016-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later

from __future__ import annotations

import ctypes
import json
import os
import re
import sys
import threading
import uuid
from concurrent.futures import CancelledError as ConcurrentCancelledError
from concurrent.futures import Future, ThreadPoolExecutor, TimeoutError as ConcurrentTimeoutError
from pathlib import Path
from collections.abc import MutableMapping

from nelson.arrays import (
    NelsonArray,
    cell,
    char,
    dictionary,
    double,
    int8,
    int16,
    int32,
    int64,
    logical,
    sparse,
    struct,
    table,
    single,
    uint8,
    uint16,
    uint32,
    uint64,
)

__all__ = [
    "start_nelson",
    "find_nelson",
    "connect_nelson",
    "NelsonEngine",
    "FutureResult",
    "EngineError",
    "NelsonExecutionError",
    "CancelledError",
    "InterruptedError",
    "RejectedExecutionError",
    "TimeoutError",
    "NelsonObject",
]

_ARRAY_CLASSES = {
    "double": double,
    "single": single,
    "int8": int8,
    "int16": int16,
    "int32": int32,
    "int64": int64,
    "uint8": uint8,
    "uint16": uint16,
    "uint32": uint32,
    "uint64": uint64,
    "logical": logical,
}
_VALID_STRUCT_FIELD = re.compile(r"^[A-Za-z]\w*$")

_EXECUTOR = ThreadPoolExecutor(max_workers=4, thread_name_prefix="nelson-engine")


class EngineError(RuntimeError):
    pass


class NelsonExecutionError(RuntimeError):
    pass


class CancelledError(EngineError):
    pass


class InterruptedError(EngineError):
    pass


class RejectedExecutionError(EngineError):
    pass


class TimeoutError(EngineError):
    pass


class NelsonObject:
    def __init__(self, class_name, payload=None, engine=None, workspace_name=None):
        self.class_name = class_name
        self.payload = payload
        self._engine = engine
        self._workspace_name = workspace_name

    def release(self):
        if self._engine is not None and self._workspace_name:
            self._engine._release_object_handle(self._workspace_name)
            self._engine = None
            self._workspace_name = None

    def get(self, property_name=None):
        if self._engine is None:
            raise EngineError("NelsonObject handle is no longer attached to an engine")
        if property_name is None:
            return self._engine.feval("get", self)
        return self._engine.feval("get", self, property_name)

    def set(self, property_name, value):
        if self._engine is None:
            raise EngineError("NelsonObject handle is no longer attached to an engine")
        return self._engine.feval("set", self, property_name, value, nargout=0)

    def __repr__(self):
        if self._workspace_name:
            return f"NelsonObject({self.class_name!r}, workspace_name={self._workspace_name!r})"
        return f"NelsonObject({self.class_name!r})"


def _candidate_roots():
    here = Path(__file__).resolve()
    yield here.parents[5] if len(here.parents) > 5 else here.parent
    env_root = os.environ.get("NELSON_ROOT")
    if env_root:
        yield Path(env_root)
    cwd = Path.cwd()
    yield cwd
    for parent in cwd.parents:
        yield parent


def _library_names():
    if sys.platform.startswith("win"):
        return ["libnlsEngine.dll", "nlsEngine.dll"]
    if sys.platform == "darwin":
        return ["libnlsEngine.dylib"]
    return ["libnlsEngine.so"]


def _candidate_library_paths():
    names = _library_names()
    for root in _candidate_roots():
        for subdir in ("bin/x64", "bin/win32", "bin/ARM64", "bin/linux", "bin/macOS", "lib", "."):
            for name in names:
                yield root / subdir / name


def _find_library_path():
    explicit = os.environ.get("NELSON_ENGINE_LIBRARY")
    if explicit:
        path = Path(explicit)
        if path.exists():
            return path
        raise EngineError(f"NELSON_ENGINE_LIBRARY does not exist: {path}")

    candidates = []
    for candidate in _candidate_library_paths():
        candidates.append(candidate)
        if candidate.exists():
            return candidate
    attempted = "\n  ".join(str(candidate) for candidate in candidates[:20])
    extra = "\n  ..." if len(candidates) > 20 else ""
    raise EngineError(
        "Cannot locate Nelson engine library. Set NELSON_ENGINE_LIBRARY to the full "
        "library path or NELSON_ROOT to the Nelson installation/build root. Tried:\n  "
        + attempted
        + extra
    )


def _load_library():
    explicit = os.environ.get("NELSON_ENGINE_LIBRARY")
    if explicit:
        return ctypes.CDLL(str(_find_library_path()))

    names = _library_names()
    candidates = []
    for candidate in _candidate_library_paths():
        candidates.append(candidate)
    for candidate in candidates:
        if candidate.exists():
            return ctypes.CDLL(str(candidate))
    for name in names:
        try:
            return ctypes.CDLL(name)
        except OSError:
            pass
    raise EngineError("Cannot locate Nelson engine library. Set NELSON_ENGINE_LIBRARY or NELSON_ROOT.")


class _NativeBackend:
    def __init__(self):
        self.lib = _load_library()
        self._configure()

    def _configure(self):
        lib = self.lib
        lib.nelson_engine_start.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p)]
        lib.nelson_engine_start.restype = ctypes.c_void_p
        lib.nelson_engine_connect.argtypes = [ctypes.c_int, ctypes.POINTER(ctypes.c_char_p)]
        lib.nelson_engine_connect.restype = ctypes.c_void_p
        lib.nelson_engine_find.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.c_int]
        lib.nelson_engine_find.restype = ctypes.c_int
        lib.nelson_engine_eval.argtypes = [
            ctypes.c_void_p,
            ctypes.c_char_p,
            ctypes.POINTER(ctypes.c_char_p),
            ctypes.POINTER(ctypes.c_char_p),
        ]
        lib.nelson_engine_eval.restype = ctypes.c_int
        lib.nelson_engine_get_variable_json.argtypes = [
            ctypes.c_void_p,
            ctypes.c_char_p,
            ctypes.POINTER(ctypes.c_char_p),
            ctypes.POINTER(ctypes.c_char_p),
        ]
        lib.nelson_engine_get_variable_json.restype = ctypes.c_int
        lib.nelson_engine_put_variable_json.argtypes = [
            ctypes.c_void_p,
            ctypes.c_char_p,
            ctypes.c_char_p,
            ctypes.POINTER(ctypes.c_char_p),
        ]
        lib.nelson_engine_put_variable_json.restype = ctypes.c_int
        lib.nelson_engine_close.argtypes = [ctypes.c_void_p, ctypes.POINTER(ctypes.c_char_p)]
        lib.nelson_engine_close.restype = ctypes.c_int
        lib.nelson_engine_free_string.argtypes = [ctypes.c_void_p]
        lib.nelson_engine_free_string.restype = None

    def _consume(self, value):
        if not value:
            return ""
        text = ctypes.string_at(value).decode("utf-8", errors="replace")
        self.lib.nelson_engine_free_string(ctypes.cast(value, ctypes.c_void_p))
        return text

    def _error(self, message):
        if message:
            return message
        return "Nelson engine operation failed."

    def start(self, option):
        error = ctypes.c_char_p()
        handle = self.lib.nelson_engine_start(_bytes(option), ctypes.byref(error))
        message = self._consume(error)
        if not handle:
            raise EngineError(self._error(message))
        return handle

    def connect(self, pid):
        error = ctypes.c_char_p()
        handle = self.lib.nelson_engine_connect(int(pid or 0), ctypes.byref(error))
        message = self._consume(error)
        if not handle:
            raise EngineError(self._error(message))
        return handle

    def find(self):
        count = self.lib.nelson_engine_find(None, 0)
        if count <= 0:
            return ()
        values = (ctypes.c_int * count)()
        self.lib.nelson_engine_find(values, count)
        return tuple(int(v) for v in values)

    def eval(self, handle, command):
        output = ctypes.c_char_p()
        error = ctypes.c_char_p()
        status = self.lib.nelson_engine_eval(handle, _bytes(command), ctypes.byref(output), ctypes.byref(error))
        out = self._consume(output)
        message = self._consume(error)
        if status:
            raise NelsonExecutionError(self._error(message))
        return out

    def get(self, handle, name):
        result = ctypes.c_char_p()
        error = ctypes.c_char_p()
        status = self.lib.nelson_engine_get_variable_json(handle, _bytes(name), ctypes.byref(result), ctypes.byref(error))
        payload = self._consume(result)
        message = self._consume(error)
        if status:
            raise NelsonExecutionError(self._error(message))
        return _from_engine_json(json.loads(payload))

    def put(self, handle, name, value):
        error = ctypes.c_char_p()
        payload = json.dumps(_to_engine_json(value))
        status = self.lib.nelson_engine_put_variable_json(handle, _bytes(name), _bytes(payload), ctypes.byref(error))
        message = self._consume(error)
        if status:
            raise NelsonExecutionError(self._error(message))

    def close(self, handle):
        error = ctypes.c_char_p()
        status = self.lib.nelson_engine_close(handle, ctypes.byref(error))
        message = self._consume(error)
        if status:
            raise EngineError(self._error(message))


def _bytes(value):
    if value is None:
        return None
    return str(value).encode("utf-8")


def _contains_complex(value):
    if isinstance(value, complex):
        return True
    if isinstance(value, (list, tuple, range)):
        return any(_contains_complex(item) for item in value)
    return False


def _array_from_numpy(value):
    import numpy as np

    dtype = value.dtype
    if dtype == np.bool_:
        return logical(value.tolist())
    if np.issubdtype(dtype, np.complexfloating):
        cls = single if dtype == np.complex64 else double
        return cls(value.tolist(), is_complex=True)
    if np.issubdtype(dtype, np.floating):
        cls = single if dtype == np.float32 else double
        return cls(value.tolist())
    if np.issubdtype(dtype, np.signedinteger):
        if dtype == np.int8:
            cls = int8
        elif dtype == np.int16:
            cls = int16
        elif dtype == np.int32:
            cls = int32
        else:
            cls = int64
        return cls(value.tolist())
    if np.issubdtype(dtype, np.unsignedinteger):
        if dtype == np.uint8:
            cls = uint8
        elif dtype == np.uint16:
            cls = uint16
        elif dtype == np.uint32:
            cls = uint32
        else:
            cls = uint64
        return cls(value.tolist())
    raise TypeError(f"Unsupported NumPy dtype for Nelson engine transfer: {dtype}")


def _dataframe_row_names(value):
    try:
        import pandas as pd
    except Exception:
        pd = None
    if pd is not None and isinstance(value.index, pd.RangeIndex):
        default_index = pd.RangeIndex(start=0, stop=len(value), step=1)
        if value.index.equals(default_index):
            return []
    return [str(item) for item in value.index]


def _to_engine_json(value):
    if hasattr(value, "_to_engine_json"):
        return value._to_engine_json()
    if isinstance(value, bool):
        return logical([[value]])._to_engine_json()
    if isinstance(value, complex):
        return double([[value]], is_complex=True)._to_engine_json()
    if isinstance(value, int):
        return double([[float(value)]])._to_engine_json()
    if isinstance(value, float):
        return double([[value]])._to_engine_json()
    if isinstance(value, str):
        return char(value)._to_engine_json()
    if isinstance(value, (list, tuple, range)):
        if all(isinstance(item, (dict, struct)) for item in value):
            raise TypeError("non-scalar struct array transfer is not supported; assign scalar nelson.struct values or keep the struct array as a NelsonObject handle")
        return double(value, is_complex=_contains_complex(value))._to_engine_json()
    try:
        import numpy as np
    except Exception:
        np = None
    if np is not None and isinstance(value, np.ndarray):
        return _array_from_numpy(value)._to_engine_json()
    try:
        import pandas as pd
    except Exception:
        pd = None
    if pd is not None and isinstance(value, pd.DataFrame):
        columns = {str(name): value[name].to_numpy() for name in value.columns}
        return table(columns, row_names=_dataframe_row_names(value))._to_engine_json()
    if isinstance(value, dictionary):
        return struct(dict(value))._to_engine_json()
    if isinstance(value, dict):
        if all(isinstance(key, str) and _VALID_STRUCT_FIELD.match(key) for key in value):
            return struct(value)._to_engine_json()
        raise TypeError("dict keys must be valid Nelson struct field names for engine transfer")
    raise TypeError(f"Unsupported value for Nelson engine transfer: {type(value).__name__}")


def _from_engine_json(payload):
    class_name = payload.get("class")
    if class_name == "char":
        return char(payload.get("data", ""))
    if class_name == "cell":
        values = [_from_engine_json(item) for item in payload.get("data", [])]
        return cell._from_column_major(values, tuple(payload.get("size", (0, 0))))
    if class_name == "struct":
        data = payload.get("data", {})
        return struct({key: _from_engine_json(data[key]) for key in payload.get("fields", data.keys())})
    if class_name == "table":
        data = payload.get("data", {})
        tbl = table(
            {key: _from_engine_json(data[key]) for key in payload.get("variables", data.keys())},
            row_names=payload.get("row_names", []),
        )
        try:
            import pandas as pd
        except Exception:
            pd = None
        if pd is not None:
            columns = {}
            for key, value in tbl.columns.items():
                if isinstance(value, NelsonArray):
                    rows = list(value)
                    if value.size[1] == 1:
                        columns[key] = [row[0] for row in rows]
                    elif value.size[0] == 1:
                        columns[key] = rows[0]
                    else:
                        columns[key] = rows
                else:
                    columns[key] = value
            return pd.DataFrame(columns, index=tbl.row_names or None)
        return tbl
    if class_name in _ARRAY_CLASSES:
        is_complex = payload.get("is_complex", False)
        data = payload.get("data", [])
        if payload.get("is_sparse", False):
            if is_complex:
                real = data.get("real", [])
                imag = data.get("imag", [])
                values = [complex(r, i) for r, i in zip(real, imag)]
            else:
                values = data
            return sparse(
                payload.get("rows", []),
                payload.get("cols", []),
                values,
                tuple(payload.get("size", (0, 0))),
                class_name=class_name,
                is_complex=is_complex,
            )
        if is_complex and isinstance(data, dict):
            real = data.get("real", [])
            imag = data.get("imag", [])
            data = [complex(r, i) for r, i in zip(real, imag)]
        cls = _ARRAY_CLASSES[class_name]
        return cls._from_column_major(data, tuple(payload.get("size", (0, 0))), is_complex)
    return NelsonObject(class_name or "object", payload)


def _flat_values(value):
    if isinstance(value, NelsonArray):
        return list(value._data)
    if isinstance(value, (list, tuple)):
        return list(value)
    return [value]


def _looks_like_nelson_error(output):
    return isinstance(output, str) and output.lstrip().startswith("Error:")


class FutureResult:
    def __init__(self, future: Future):
        self._future = future

    def result(self, timeout=None):
        try:
            return self._future.result(timeout=timeout)
        except ConcurrentTimeoutError as exc:
            raise TimeoutError("Nelson engine operation timed out") from exc
        except ConcurrentCancelledError as exc:
            raise CancelledError("Nelson engine operation was cancelled") from exc

    def done(self):
        return self._future.done()

    def cancel(self):
        return self._future.cancel()

    def cancelled(self):
        return self._future.cancelled()


class _Workspace(MutableMapping):
    def __init__(self, engine):
        self._engine = engine

    def __getitem__(self, name):
        if self._engine._is_dictionary(name):
            return self._engine._get_dictionary(name)
        if self._engine._is_sparse(name):
            return self._engine._get_sparse(name)
        if self._engine._is_table(name):
            return self._engine._get_table(name)
        if self._engine._is_struct(name):
            return self._engine._get_struct(name)
        object_class = self._engine._object_handle_class(name)
        if object_class:
            return self._engine._get_object_handle(name, object_class)
        return self._engine._backend.get(self._engine._handle, name)

    def __setitem__(self, name, value):
        if isinstance(value, NelsonObject):
            self._engine._put_object_handle(name, value)
            return
        if isinstance(value, sparse):
            self._engine._put_sparse(name, value)
            return
        if isinstance(value, struct):
            self._engine._put_struct(name, value)
            return
        if isinstance(value, table):
            self._engine._put_table(name, value)
            return
        try:
            import pandas as pd
        except Exception:
            pd = None
        if pd is not None and isinstance(value, pd.DataFrame):
            self._engine._put_table(name, table({str(col): value[col].to_numpy() for col in value.columns},
                                                row_names=_dataframe_row_names(value)))
            return
        if isinstance(value, dictionary) or (
            isinstance(value, dict)
            and not all(isinstance(key, str) and _VALID_STRUCT_FIELD.match(key) for key in value)
        ):
            self._engine._put_dictionary(name, dictionary(value))
            return
        if isinstance(value, dict):
            self._engine._put_struct(name, struct(value))
            return
        self._engine._backend.put(self._engine._handle, name, value)

    def __delitem__(self, name):
        self._engine.eval(f"clear {name}", nargout=0)

    def __iter__(self):
        raise TypeError("Nelson workspace iteration is not implemented")

    def __len__(self):
        raise TypeError("Nelson workspace length is not implemented")


class NelsonEngine:
    def __init__(self, handle, backend=None, owned=True):
        self._handle = handle
        self._backend = backend or _NativeBackend()
        self._owned = owned
        self._lock = threading.RLock()
        self._object_handle_names = set()
        self.workspace = _Workspace(self)

    def eval(self, command, nargout=0, stdout=None, stderr=None):
        try:
            with self._lock:
                output = self._backend.eval(self._handle, command)
        except NelsonExecutionError as exc:
            if stderr is not None:
                stderr.write(str(exc))
            raise
        if _looks_like_nelson_error(output):
            error = NelsonExecutionError(output.strip())
            if stderr is not None:
                stderr.write(str(error))
            raise error
        if stdout is not None and output:
            stdout.write(output)
        if nargout == 0:
            return None
        return output

    def feval(self, function_name, *args, nargout=1, stdout=None, stderr=None, background=False):
        if background:
            return FutureResult(
                _EXECUTOR.submit(
                    lambda: self.feval(
                        function_name, *args, nargout=nargout, stdout=stdout, stderr=stderr
                    )
                )
            )
        prefix = f"__nelson_py_{uuid.uuid4().hex}"
        arg_names = []
        out_names = [f"{prefix}_out_{i}" for i in range(int(nargout))]
        try:
            for index, value in enumerate(args):
                name = f"{prefix}_arg_{index}"
                self.workspace[name] = value
                arg_names.append(name)
            call = f"{function_name}({', '.join(arg_names)})"
            if nargout == 0:
                self.eval(f"{call};", nargout=0, stdout=stdout, stderr=stderr)
                return None
            lhs = ", ".join(out_names)
            self.eval(f"[{lhs}] = {call};", nargout=0, stdout=stdout, stderr=stderr)
            values = tuple(self.workspace[name] for name in out_names)
            return values[0] if nargout == 1 else values
        finally:
            names = arg_names + out_names
            if names:
                try:
                    self.eval("clear " + " ".join(names), nargout=0)
                except Exception:
                    pass

    def _put_sparse(self, name, value):
        prefix = f"__nelson_py_{uuid.uuid4().hex}"
        row_name = f"{prefix}_rows"
        col_name = f"{prefix}_cols"
        val_name = f"{prefix}_values"
        rows = [row + 1 for row in value.rows]
        cols = [col + 1 for col in value.cols]
        try:
            self._backend.put(self._handle, row_name, double(vector=rows))
            self._backend.put(self._handle, col_name, double(vector=cols))
            self._backend.put(
                self._handle, val_name, double(vector=value.values, is_complex=value.is_complex)
            )
            command = f"{name} = sparse({row_name}, {col_name}, {val_name}, {value.size[0]}, {value.size[1]});"
            if value.class_name == "logical":
                command = f"{name} = logical(sparse({row_name}, {col_name}, {val_name}, {value.size[0]}, {value.size[1]}));"
            self.eval(command, nargout=0)
        finally:
            try:
                self.eval(f"clear {row_name} {col_name} {val_name}", nargout=0)
            except Exception:
                pass

    def _put_dictionary(self, name, value):
        prefix = f"__nelson_py_{uuid.uuid4().hex}"
        key_name = f"{prefix}_keys"
        value_name = f"{prefix}_values"
        try:
            keys = cell([list(value.keys())])
            values = cell([list(value.values())])
            self._backend.put(self._handle, key_name, keys)
            self._backend.put(self._handle, value_name, values)
            self.eval(f"{name} = dictionary({key_name}, {value_name});", nargout=0)
        finally:
            try:
                self.eval(f"clear {key_name} {value_name}", nargout=0)
            except Exception:
                pass

    def _put_struct(self, name, value):
        prefix = f"__nelson_py_{uuid.uuid4().hex}"
        temp_names = []
        try:
            args = []
            for index, (field_name, field_value) in enumerate(value.items()):
                temp_name = f"{prefix}_field_{index}"
                self._backend.put(self._handle, temp_name, field_value)
                temp_names.append(temp_name)
                escaped_field = field_name.replace("'", "''")
                args.append(f"'{escaped_field}'")
                args.append(temp_name)
            self.eval(f"{name} = struct({', '.join(args)});", nargout=0)
        finally:
            if temp_names:
                try:
                    self.eval("clear " + " ".join(temp_names), nargout=0)
                except Exception:
                    pass

    def _put_table(self, name, value):
        prefix = f"__nelson_py_{uuid.uuid4().hex}"
        temp_names = []
        column_names = []
        try:
            for index, (column_name, column_value) in enumerate(value.columns.items()):
                temp_name = f"{prefix}_col_{index}"
                self._backend.put(self._handle, temp_name, column_value)
                self.eval(f"{temp_name} = {temp_name}(:);", nargout=0)
                temp_names.append(temp_name)
                column_names.append(temp_name)
            variable_names = "{" + ", ".join(
                "'" + column_name.replace("'", "''") + "'" for column_name in value.columns
            ) + "}"
            row_names_arg = ""
            if value.row_names:
                row_names_name = f"{prefix}_row_names"
                self._backend.put(self._handle, row_names_name, cell([value.row_names]))
                temp_names.append(row_names_name)
                row_names_arg = f", 'RowNames', {row_names_name}"
            self.eval(
                f"{name} = table({', '.join(column_names)}, 'VariableNames', {variable_names}{row_names_arg});",
                nargout=0,
            )
        finally:
            if temp_names:
                try:
                    self.eval("clear " + " ".join(temp_names), nargout=0)
                except Exception:
                    pass

    def _is_dictionary(self, name):
        flag_name = f"__nelson_py_{uuid.uuid4().hex}_isdict"
        try:
            self.eval(f"{flag_name} = isa({name}, 'dictionary');", nargout=0)
            flag = self._backend.get(self._handle, flag_name)
            values = _flat_values(flag)
            return bool(values and values[0])
        except Exception:
            return False
        finally:
            try:
                self.eval(f"clear {flag_name}", nargout=0)
            except Exception:
                pass

    def _is_struct(self, name):
        flag_name = f"__nelson_py_{uuid.uuid4().hex}_isstruct"
        try:
            self.eval(f"{flag_name} = isstruct({name});", nargout=0)
            flag = self._backend.get(self._handle, flag_name)
            values = _flat_values(flag)
            return bool(values and values[0])
        except Exception:
            return False
        finally:
            try:
                self.eval(f"clear {flag_name}", nargout=0)
            except Exception:
                pass

    def _is_table(self, name):
        flag_name = f"__nelson_py_{uuid.uuid4().hex}_istable"
        try:
            self.eval(f"{flag_name} = istable({name});", nargout=0)
            flag = self._backend.get(self._handle, flag_name)
            values = _flat_values(flag)
            return bool(values and values[0])
        except Exception:
            return False
        finally:
            try:
                self.eval(f"clear {flag_name}", nargout=0)
            except Exception:
                pass

    def _get_table(self, name):
        names_var = f"__nelson_py_{uuid.uuid4().hex}_varnames"
        row_names_var = f"__nelson_py_{uuid.uuid4().hex}_rownames"
        temp_names = []
        try:
            self.eval(f"{names_var} = properties({name});", nargout=0)
            names_value = self._backend.get(self._handle, names_var)
            variable_names = [
                str(item) for item in (names_value._data if isinstance(names_value, cell) else _flat_values(names_value))
            ]
            variable_names = [
                item for item in variable_names if item not in ("Properties", "Row", "Variables")
            ]
            columns = {}
            for index, variable_name in enumerate(variable_names):
                temp_name = f"{names_var}_col_{index}"
                temp_names.append(temp_name)
                escaped_name = variable_name.replace("'", "''")
                self.eval(f"{temp_name} = {name}.('{escaped_name}');", nargout=0)
                columns[variable_name] = self.workspace[temp_name]
            row_names = []
            try:
                self.eval(f"{row_names_var} = {name}.Properties.('RowNames');", nargout=0)
                row_names_value = self._backend.get(self._handle, row_names_var)
                row_names = [
                    str(item) for item in (
                        row_names_value._data if isinstance(row_names_value, cell) else _flat_values(row_names_value)
                    )
                ]
            except Exception:
                row_names = []
            tbl = table(columns, row_names=row_names)
            try:
                import pandas as pd
            except Exception:
                pd = None
            if pd is None:
                return tbl
            frame_columns = {}
            for key, value in columns.items():
                if isinstance(value, NelsonArray):
                    rows = list(value)
                    if value.size[1] == 1:
                        frame_columns[key] = [row[0] for row in rows]
                    elif value.size[0] == 1:
                        frame_columns[key] = rows[0]
                    else:
                        frame_columns[key] = rows
                else:
                    frame_columns[key] = value
            return pd.DataFrame(frame_columns, index=row_names or None)
        finally:
            try:
                self.eval("clear " + " ".join([names_var, row_names_var] + temp_names), nargout=0)
            except Exception:
                pass

    def _get_struct(self, name):
        size_name = f"__nelson_py_{uuid.uuid4().hex}_struct_numel"
        fields_name = f"__nelson_py_{uuid.uuid4().hex}_fields"
        temp_names = []
        try:
            self.eval(f"{size_name} = numel({name});", nargout=0)
            size_values = _flat_values(self._backend.get(self._handle, size_name))
            if size_values and int(size_values[0]) != 1:
                return self._get_object_handle(name, "struct")
            self.eval(f"{fields_name} = fieldnames({name});", nargout=0)
            fields_value = self._backend.get(self._handle, fields_name)
            field_names = [str(item) for item in (fields_value._data if isinstance(fields_value, cell) else _flat_values(fields_value))]
            result = {}
            for index, field_name in enumerate(field_names):
                temp_name = f"{fields_name}_value_{index}"
                temp_names.append(temp_name)
                escaped_field = field_name.replace("'", "''")
                self.eval(f"{temp_name} = {name}.('{escaped_field}');", nargout=0)
                result[field_name] = self.workspace[temp_name]
            return struct(result)
        finally:
            try:
                self.eval("clear " + " ".join([size_name, fields_name] + temp_names), nargout=0)
            except Exception:
                pass

    def _get_dictionary(self, name):
        prefix = f"__nelson_py_{uuid.uuid4().hex}"
        key_name = f"{prefix}_keys"
        value_name = f"{prefix}_values"
        try:
            self.eval(f"{key_name} = keys({name}); {value_name} = values({name});", nargout=0)
            keys_value = self._backend.get(self._handle, key_name)
            values_value = self._backend.get(self._handle, value_name)
            keys_list = _flat_values(keys_value)
            values_list = _flat_values(values_value)
            if isinstance(keys_value, cell):
                keys_list = list(keys_value._data)
            if isinstance(values_value, cell):
                values_list = list(values_value._data)
            return dictionary(zip(keys_list, values_list))
        finally:
            try:
                self.eval(f"clear {key_name} {value_name}", nargout=0)
            except Exception:
                pass

    def _is_sparse(self, name):
        flag_name = f"__nelson_py_{uuid.uuid4().hex}_issparse"
        try:
            self.eval(f"{flag_name} = issparse({name});", nargout=0)
            flag = self._backend.get(self._handle, flag_name)
            values = _flat_values(flag)
            return bool(values and values[0])
        except Exception:
            return False
        finally:
            try:
                self.eval(f"clear {flag_name}", nargout=0)
            except Exception:
                pass

    def _object_handle_class(self, name):
        flag_name = f"__nelson_py_{uuid.uuid4().hex}_isobject"
        class_name = f"__nelson_py_{uuid.uuid4().hex}_class"
        try:
            self.eval(
                f"{flag_name} = false; "
                f"try, {flag_name} = isgraphics({name}); catch, end; "
                f"if ~{flag_name}, try, {flag_name} = isobject({name}); catch, end; end; "
                f"if {flag_name}, {class_name} = class({name}); end;",
                nargout=0,
            )
            flag = self._backend.get(self._handle, flag_name)
            values = _flat_values(flag)
            if not (values and values[0]):
                return None
            try:
                class_value = self._backend.get(self._handle, class_name)
                class_values = _flat_values(class_value)
                return str(class_values[0]) if class_values else "object"
            except Exception:
                return "object"
        except Exception:
            return None
        finally:
            try:
                self.eval(f"clear {flag_name} {class_name}", nargout=0)
            except Exception:
                pass

    def _get_object_handle(self, name, class_name):
        handle_name = f"__nelson_py_handle_{uuid.uuid4().hex}"
        self.eval(f"{handle_name} = {name};", nargout=0)
        self._object_handle_names.add(handle_name)
        return NelsonObject(class_name or "object", {"workspace": handle_name}, self, handle_name)

    def _put_object_handle(self, name, value):
        if value._engine is not self or not value._workspace_name:
            raise TypeError("NelsonObject handles can only be passed back to the engine that created them")
        self.eval(f"{name} = {value._workspace_name};", nargout=0)

    def _release_object_handle(self, name):
        if name in self._object_handle_names:
            self._object_handle_names.discard(name)
            try:
                self.eval(f"clear {name}", nargout=0)
            except Exception:
                pass

    def _clear_object_handles(self):
        names = list(self._object_handle_names)
        self._object_handle_names.clear()
        if names:
            try:
                self.eval("clear " + " ".join(names), nargout=0)
            except Exception:
                pass

    def _get_sparse(self, name):
        prefix = f"__nelson_py_{uuid.uuid4().hex}"
        full_name = f"{prefix}_full"
        size_name = f"{prefix}_size"
        logical_name = f"{prefix}_logical"
        try:
            self.eval(
                f"{full_name} = full({name}); "
                f"{size_name} = size({name}); {logical_name} = islogical({name});",
                nargout=0,
            )
            full_array = self._backend.get(self._handle, full_name)
            dims = [int(v) for v in _flat_values(self._backend.get(self._handle, size_name))]
            is_logical = bool(_flat_values(self._backend.get(self._handle, logical_name))[0])
            rows = []
            cols = []
            values = []
            nrows = dims[0] if dims else 0
            for index, item in enumerate(_flat_values(full_array)):
                if item:
                    rows.append(index % nrows)
                    cols.append(index // nrows)
                    values.append(item)
            return sparse(
                rows,
                cols,
                values,
                tuple(dims[:2]),
                class_name="logical" if is_logical else "double",
                is_complex=getattr(full_array, "is_complex", False),
            )
        finally:
            try:
                self.eval(f"clear {full_name} {size_name} {logical_name}", nargout=0)
            except Exception:
                pass

    def quit(self):
        self.close()

    def close(self):
        handle = self._handle
        if handle:
            self._clear_object_handles()
            self._handle = None
            self._backend.close(handle)

    def genpath(self, path, nargout=1, stdout=None, stderr=None, background=False):
        if background:
            return FutureResult(
                _EXECUTOR.submit(
                    lambda: self.genpath(path, nargout=nargout, stdout=stdout, stderr=stderr)
                )
            )
        try:
            return self.feval("genpath", path, nargout=nargout, stdout=stdout, stderr=stderr)
        except NelsonExecutionError as exc:
            if "Undefined variable or function: genpath" not in str(exc):
                raise
            if nargout == 0:
                return None
            root = Path(path)
            paths = [str(root)]
            paths.extend(str(item) for item in root.rglob("*") if item.is_dir())
            return os.pathsep.join(paths)

    def addpath(self, path, *args, nargout=0, stdout=None, stderr=None, background=False):
        if background:
            return FutureResult(
                _EXECUTOR.submit(
                    lambda: self.addpath(
                        path, *args, nargout=nargout, stdout=stdout, stderr=stderr
                    )
                )
            )
        if isinstance(path, str) and os.pathsep in path and not args and nargout == 0:
            for item in [part for part in path.split(os.pathsep) if part]:
                self.feval("addpath", item, nargout=0, stdout=stdout, stderr=stderr)
            return None
        return self.feval("addpath", path, *args, nargout=nargout, stdout=stdout, stderr=stderr)

    def __getattr__(self, name):
        if name.startswith("_"):
            raise AttributeError(name)

        def caller(*args, **kwargs):
            return self.feval(name, *args, **kwargs)

        return caller

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb):
        self.close()

    def __del__(self):
        try:
            if self._owned:
                self.close()
        except Exception:
            pass


def _pop_async_alias(kwargs):
    if "async" in kwargs:
        return kwargs.pop("async")
    return None


def start_nelson(option="-minimize", background=False, **kwargs):
    async_value = _pop_async_alias(kwargs)
    if async_value is not None:
        background = async_value
    if kwargs:
        raise TypeError(f"unexpected keyword argument: {next(iter(kwargs))}")

    def start():
        backend = _NativeBackend()
        return NelsonEngine(backend.start(option), backend=backend, owned=True)

    return FutureResult(_EXECUTOR.submit(start)) if background else start()


def connect_nelson(name=None, background=False, **kwargs):
    async_value = _pop_async_alias(kwargs)
    if async_value is not None:
        background = async_value
    if kwargs:
        raise TypeError(f"unexpected keyword argument: {next(iter(kwargs))}")

    def connect():
        backend = _NativeBackend()
        return NelsonEngine(backend.connect(name or 0), backend=backend, owned=False)

    return FutureResult(_EXECUTOR.submit(connect)) if background else connect()


def find_nelson():
    return _NativeBackend().find()
