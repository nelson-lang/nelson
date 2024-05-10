//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "PythonLibraryWrapper.hpp"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
enum PythonType
{
    PY_NONE_TYPE = 0,
    PY_STR_TYPE,
    PY_FLOAT_TYPE,
    PY_BOOL_TYPE,
    PY_COMPLEX_TYPE,
    PY_LONG_TYPE,
    PY_BYTES_TYPE,
    PY_BYTE_ARRAY_TYPE,
    PY_MEMORY_VIEW_TYPE,
    PY_LIST_TYPE,
    PY_TUPLE_TYPE,
    PY_DICT_TYPE,
    PY_ARRAY_ARRAY_TYPE,
    PY_NUMPY_TYPE,
    PY_NOT_MANAGED
};
//=============================================================================
PythonType
getPythonType(PyObject* po);
//=============================================================================
std::wstring
PyObjectToStringRepresentation(PyObject* po);
//=============================================================================
std::wstring
TypeName(PyObject* po);
//=============================================================================
wstringVector
getPyObjectMethods(PyObject* po, bool withUnderscoreMethods = false);
//=============================================================================
wstringVector
getPyObjectProperties(PyObject* po, bool withUnderscoreMethods = false);
//=============================================================================
bool
isArrayArrayOfTypeCode(const char* typeName, PyObject* pyObject);
//=============================================================================
const char*
getArrayArrayTypeCode(PyObject* pyObject);
//=============================================================================
NelsonType
PyTypecodeToNelsonType(const char* memoryViewType);
//=============================================================================
char*
nelsonTypeToTypeCode(NelsonType nelsonType);
//=============================================================================
bool
PyIsEqual(PyObject* pyObjectA, PyObject* pyObjectB);
//=============================================================================
uint64
PyGetHashValue(PyObject* pyObject);
//=============================================================================
}
//=============================================================================
