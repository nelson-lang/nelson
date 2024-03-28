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
PyObject*
deepCopyPyObject(PyObject* obj);
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
}
//=============================================================================
