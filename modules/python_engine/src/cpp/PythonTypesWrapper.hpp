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
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
PyObjectToArrayOf(PyObject* pyObject, bool& needToDecreaseReference);
//=============================================================================
ArrayOf
PyArrayArrayToArrayOf(PyObject* pyObject);
//=============================================================================
PyObject*
arrayToMemoryView(const void* data, NelsonType nelsonType, const Dimensions& dims);
//=============================================================================
PyObject*
arrayOfToPyObject(const ArrayOf& A);
//=============================================================================
PyObject*
PyArgsArrayOfToPyObject(const ArrayOf& A);
//=============================================================================
ArrayOf
PyMemoryViewToArrayOf(PyObject* pyObject);
//=============================================================================

}
//=============================================================================
