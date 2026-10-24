//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
ArrayOf
PyMemoryViewToArrayOf(PyObject* pyObject);
//=============================================================================
// Never raises: returns false when pandas is not importable.
bool
isPandasDataFrame(PyObject* pyObject);
//=============================================================================
// Never raises: returns false when pandas is not importable.
bool
isPandasSeries(PyObject* pyObject);
//=============================================================================
// Splits a pandas DataFrame or Series into the pieces of a Nelson table: one column per
// DataFrame column, the column labels as variable names and the row names. A non-default
// index becomes a leading 'index' variable, so rowNames is always empty. Returns false and
// leaves the outputs untouched for any other object or when pandas is unavailable.
bool
PyPandasToTableData(
    PyObject* pyObject, ArrayOf& columns, ArrayOf& variableNames, ArrayOf& rowNames);
//=============================================================================
} // namespace Nelson
//=============================================================================
