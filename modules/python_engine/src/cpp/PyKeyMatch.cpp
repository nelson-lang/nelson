//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "PyKeyMatch.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "PythonObjectHandle.hpp"
#include "PyObjectHelpers.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
PyKeyMatch(const ArrayOf& A, const ArrayOf& B)
{
    if (A.dataClass() != B.dataClass()) {
        return false;
    }
    if (A.handleCategory() != B.handleCategory()) {
        return false;
    }
    if (A.handleCategory() != NLS_HANDLE_PYOBJECT_CATEGORY_STR) {
        return false;
    }
    if (!A.isScalar() && !B.isScalar()) {
        return false;
    }
    PythonObjectHandle* pyObjectA = (PythonObjectHandle*)A.asHandleScalar();
    PythonObjectHandle* pyObjectB = (PythonObjectHandle*)B.asHandleScalar();
    PyObject* p1 = (PyObject*)pyObjectA->pointer();
    PyObject* p2 = (PyObject*)pyObjectB->pointer();
    return PyKeyMatch(p1, p2);
}
//=============================================================================
bool
PyKeyMatch(PyObject* p1, PyObject* p2)
{
    if (p1 != p2) {
        return false;
    }

    if (!p1 || !p2) {
        Error(L"Nelson:Python:invalidObject", L"Invalid Python object.");
    }
    uint64 hashValue1 = PyGetHashValue(p1);
    uint64 hashValue2 = PyGetHashValue(p2);

    return (hashValue1 == hashValue2);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
