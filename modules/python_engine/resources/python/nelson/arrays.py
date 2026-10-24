# Copyright (c) 2016-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later

from __future__ import annotations

from array import array
from collections.abc import Iterable, MutableMapping
from copy import deepcopy
from functools import reduce
from operator import mul


_TYPECODES = {
    "double": "d",
    "single": "f",
    "int8": "b",
    "uint8": "B",
    "int16": "h",
    "uint16": "H",
    "int32": "i",
    "uint32": "I",
    "int64": "q",
    "uint64": "Q",
    "logical": "B",
}

_ITEMSIZES = {
    "double": 8,
    "single": 4,
    "int8": 1,
    "uint8": 1,
    "int16": 2,
    "uint16": 2,
    "int32": 4,
    "uint32": 4,
    "int64": 8,
    "uint64": 8,
    "logical": 1,
}


def _prod(values):
    return reduce(mul, values, 1)


def _is_sequence(value):
    return isinstance(value, Iterable) and not isinstance(value, (str, bytes, bytearray))


def _infer_size(value):
    if not _is_sequence(value):
        return (1, 1)
    seq = list(value)
    if not seq:
        return (0, 0)
    if _is_sequence(seq[0]):
        return (len(seq), len(list(seq[0])))
    return (1, len(seq))


def _flatten_rows(value):
    if not _is_sequence(value):
        return [value]
    rows = list(value)
    if not rows:
        return []
    if not _is_sequence(rows[0]):
        return list(rows)
    width = len(list(rows[0]))
    flat = []
    for row in rows:
        row = list(row)
        if len(row) != width:
            raise ValueError("initializer sequences must be rectangular")
        flat.extend(row)
    return flat


def _row_major_to_column_major(flat, size):
    if len(size) != 2:
        return list(flat)
    rows, cols = size
    return [flat[r * cols + c] for c in range(cols) for r in range(rows)]


def _column_major_to_rows(data, size):
    if len(size) != 2:
        return list(data)
    rows, cols = size
    return [[data[c * rows + r] for c in range(cols)] for r in range(rows)]


class NelsonArray:
    _nelson_class = "double"

    def __init__(self, initializer=None, *, vector=None, size=None, is_complex=False):
        if self._nelson_class == "logical" and is_complex:
            raise TypeError("logical arrays cannot be complex")
        if initializer is not None and vector is not None:
            raise TypeError("initializer and vector cannot both be specified")

        if vector is not None:
            values = list(vector)
            inferred_size = (1, len(values))
            column_major = values
        elif initializer is None:
            values = []
            inferred_size = (0, 0)
            column_major = []
        else:
            inferred_size = _infer_size(initializer)
            values = _flatten_rows(initializer)
            column_major = _row_major_to_column_major(values, inferred_size)

        self.size = tuple(size if size is not None else inferred_size)
        if _prod(self.size) != len(column_major):
            raise ValueError("size does not match number of elements")
        self.is_complex = bool(is_complex)
        self._data = [self._coerce_value(v) for v in column_major]

    @classmethod
    def _from_column_major(cls, data, size, is_complex=False):
        obj = cls()
        obj.size = tuple(size)
        obj.is_complex = bool(is_complex)
        obj._data = [obj._coerce_value(v) for v in data]
        return obj

    @property
    def itemsize(self):
        return _ITEMSIZES[self._nelson_class]

    @property
    def _rows(self):
        return _column_major_to_rows(self._data, self.size)

    def _coerce_value(self, value):
        if self._nelson_class == "logical":
            return bool(value)
        if self.is_complex:
            return complex(value)
        if self._nelson_class in ("double", "single"):
            return float(value)
        return int(value)

    def __len__(self):
        return self.size[0] if self.size else 0

    def __iter__(self):
        return iter(self._rows)

    def __getitem__(self, key):
        return self._rows[key]

    def __setitem__(self, key, value):
        rows = self._rows
        rows[key] = value
        flat = _flatten_rows(rows)
        self._data = [self._coerce_value(v) for v in _row_major_to_column_major(flat, self.size)]

    def __repr__(self):
        return f"nelson.{self._nelson_class}({self._rows!r})"

    def clone(self):
        return deepcopy(self)

    def real(self):
        data = [v.real if isinstance(v, complex) else v for v in self._data]
        return self.__class__._from_column_major(data, self.size, False)

    def imag(self):
        data = [v.imag if isinstance(v, complex) else 0 for v in self._data]
        return self.__class__._from_column_major(data, self.size, False)

    def noncomplex(self):
        return self.__class__._from_column_major(list(self._data), self.size, False)

    def reshape(self, *dims):
        if len(dims) == 1 and isinstance(dims[0], (list, tuple)):
            dims = tuple(dims[0])
        dims = tuple(int(d) for d in dims)
        if _prod(dims) != len(self._data):
            raise ValueError("reshape dimensions must preserve element count")
        self.size = dims
        return self

    def toarray(self):
        if self.is_complex:
            return array(_TYPECODES[self._nelson_class], self._split_complex()[0])
        return array(_TYPECODES[self._nelson_class], self._data)

    def tomemoryview(self):
        return memoryview(self.toarray())

    def to_numpy(self):
        try:
            import numpy as np
        except Exception as exc:
            raise RuntimeError("NumPy is not available") from exc
        dtype = {
            "double": np.complex128 if self.is_complex else np.float64,
            "single": np.complex64 if self.is_complex else np.float32,
            "int8": np.int8,
            "uint8": np.uint8,
            "int16": np.int16,
            "uint16": np.uint16,
            "int32": np.int32,
            "uint32": np.uint32,
            "int64": np.int64,
            "uint64": np.uint64,
            "logical": np.bool_,
        }[self._nelson_class]
        arr = np.array(self._data, dtype=dtype)
        if len(self.size) == 2:
            return arr.reshape(tuple(reversed(self.size))).T
        return arr.reshape(self.size, order="F")

    def __array__(self, dtype=None):
        arr = self.to_numpy()
        return arr.astype(dtype) if dtype is not None else arr

    def _split_complex(self):
        return (
            [complex(v).real for v in self._data],
            [complex(v).imag for v in self._data],
        )

    def _to_engine_json(self):
        data = self._data
        if self.is_complex:
            real, imag = self._split_complex()
            data = {"real": real, "imag": imag}
        return {
            "class": self._nelson_class,
            "size": list(self.size),
            "is_complex": self.is_complex,
            "is_sparse": False,
            "data": data,
        }


def _make_array_class(name):
    return type(name, (NelsonArray,), {"_nelson_class": name, "__module__": __name__})


double = _make_array_class("double")
single = _make_array_class("single")
int8 = _make_array_class("int8")
int16 = _make_array_class("int16")
int32 = _make_array_class("int32")
int64 = _make_array_class("int64")
uint8 = _make_array_class("uint8")
uint16 = _make_array_class("uint16")
uint32 = _make_array_class("uint32")
uint64 = _make_array_class("uint64")
logical = _make_array_class("logical")


class char(str):
    def _to_engine_json(self):
        return {
            "class": "char",
            "size": [1, len(self)],
            "is_complex": False,
            "is_sparse": False,
            "data": str(self),
        }


class dictionary(MutableMapping):
    def __init__(self, mapping=(), **kwargs):
        self._data = dict(mapping, **kwargs)

    def __getitem__(self, key):
        return self._data[key]

    def __setitem__(self, key, value):
        self._data[key] = value

    def __delitem__(self, key):
        del self._data[key]

    def __iter__(self):
        return iter(self._data)

    def __len__(self):
        return len(self._data)

    def __repr__(self):
        return f"nelson.dictionary({self._data!r})"


class struct(MutableMapping):
    def __init__(self, mapping=(), **kwargs):
        self._data = dict(mapping, **kwargs)

    def __getitem__(self, key):
        return self._data[key]

    def __setitem__(self, key, value):
        self._data[key] = value

    def __delitem__(self, key):
        del self._data[key]

    def __iter__(self):
        return iter(self._data)

    def __len__(self):
        return len(self._data)

    def __repr__(self):
        return f"nelson.struct({self._data!r})"

    def _to_engine_json(self):
        from nelson.engine import _to_engine_json

        return {
            "class": "struct",
            "size": [1, 1],
            "is_complex": False,
            "is_sparse": False,
            "fields": list(self._data),
            "data": {key: _to_engine_json(value) for key, value in self._data.items()},
        }


class cell:
    def __init__(self, initializer=None, *, size=None):
        if initializer is None:
            initializer = []
        inferred_size = _infer_size(initializer)
        values = _flatten_rows(initializer)
        self.size = tuple(size if size is not None else inferred_size)
        if _prod(self.size) != len(values):
            raise ValueError("size does not match number of elements")
        self._data = _row_major_to_column_major(values, self.size)

    @classmethod
    def _from_column_major(cls, data, size):
        obj = cls()
        obj.size = tuple(size)
        obj._data = list(data)
        return obj

    @property
    def _rows(self):
        return _column_major_to_rows(self._data, self.size)

    def __len__(self):
        return self.size[0] if self.size else 0

    def __iter__(self):
        return iter(self._rows)

    def __getitem__(self, key):
        return self._rows[key]

    def __repr__(self):
        return f"nelson.cell({self._rows!r})"

    def _to_engine_json(self):
        from nelson.engine import _to_engine_json

        return {
            "class": "cell",
            "size": list(self.size),
            "is_complex": False,
            "is_sparse": False,
            "data": [_to_engine_json(value) for value in self._data],
        }


class table:
    def __init__(self, columns=(), *, row_names=None):
        self.columns = dict(columns)
        self.row_names = list(row_names or [])

    def __getitem__(self, key):
        return self.columns[key]

    def __setitem__(self, key, value):
        self.columns[key] = value

    def __iter__(self):
        return iter(self.columns)

    def __len__(self):
        return len(self.columns)

    def __repr__(self):
        return f"nelson.table({self.columns!r})"

    def _to_engine_json(self):
        from nelson.engine import _to_engine_json

        return {
            "class": "table",
            "size": [1, 1],
            "is_complex": False,
            "is_sparse": False,
            "variables": list(self.columns),
            "row_names": self.row_names,
            "data": {key: _to_engine_json(value) for key, value in self.columns.items()},
        }


class sparse:
    def __init__(self, rows, cols, values, size, *, class_name="double", is_complex=False):
        if len(rows) != len(cols) or len(rows) != len(values):
            raise ValueError("rows, cols, and values must have the same length")
        if class_name not in ("double", "logical"):
            raise TypeError("only sparse double, sparse complex double, and sparse logical are supported")
        self.rows = [int(v) for v in rows]
        self.cols = [int(v) for v in cols]
        self.values = [complex(v) if is_complex else v for v in values]
        self.size = tuple(int(v) for v in size)
        self.class_name = class_name
        self.is_complex = bool(is_complex)

    def __repr__(self):
        return (
            f"nelson.sparse(rows={self.rows!r}, cols={self.cols!r}, "
            f"values={self.values!r}, size={self.size!r})"
        )

    @property
    def nnz(self):
        return len(self.values)

    def todense(self):
        cls = logical if self.class_name == "logical" else double
        dense = [[False if self.class_name == "logical" else 0 for _ in range(self.size[1])]
                 for _ in range(self.size[0])]
        for r, c, v in zip(self.rows, self.cols, self.values):
            dense[r][c] = v
        return cls(dense, is_complex=self.is_complex)

    def to_numpy(self):
        return self.todense().to_numpy()

    def __array__(self, dtype=None):
        arr = self.to_numpy()
        return arr.astype(dtype) if dtype is not None else arr

    def _to_engine_json(self):
        if self.is_complex:
            real = [complex(v).real for v in self.values]
            imag = [complex(v).imag for v in self.values]
            data = {"real": real, "imag": imag}
        else:
            data = list(self.values)
        return {
            "class": self.class_name,
            "size": list(self.size),
            "is_complex": self.is_complex,
            "is_sparse": True,
            "rows": self.rows,
            "cols": self.cols,
            "data": data,
        }
