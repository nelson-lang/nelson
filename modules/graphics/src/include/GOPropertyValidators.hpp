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
#include <cmath>
#include <string>
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Numeric property data: numeric or logical and real (no text, cell, struct,
// function handle or graphics object).
inline void
checkRealNumericPropertyValue(const ArrayOf& value)
{
    if (!(value.isNumeric() || value.isLogical()) || value.isSparse()) {
        Error(L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical.",
            L"Value");
    }
    if (value.isComplex()) {
        Error(L"Nelson:validators:mustBeReal", L"%s must be real.", L"Value");
    }
}
//=============================================================================
inline void
checkNoNaNPropertyValue(const double* values, indexType count)
{
    for (indexType k = 0; k < count; k++) {
        if (std::isnan(values[k])) {
            Error(L"Nelson:validators:mustBeNonNan", L"%s must not be NaN.", L"Value");
        }
    }
}
//=============================================================================
// Numeric property with a fixed number of elements (a scalar when count is 1).
inline void
checkFixedLengthPropertyValue(const ArrayOf& value, indexType count)
{
    checkRealNumericPropertyValue(value);
    if (value.elementCount() != count) {
        if (count == 1) {
            Error(L"Nelson:validators:mustBeScalar", L"%s must be a scalar.", L"Value");
        }
        Error(L"Nelson:graphics:expectedNumericVectorLength",
            L"Value must be a real numeric vector with %s elements.", std::to_wstring(count));
    }
}
//=============================================================================
// Text property: a character vector (possibly empty) or a string scalar.
inline void
checkTextPropertyValue(const ArrayOf& value)
{
    bool isText = value.isRowVectorCharacterArray() || (value.isCharacterArray() && value.isEmpty())
        || value.isScalarStringArray();
    if (!isText) {
        Error(L"Nelson:graphics:expectedStringProperty", L"Expecting a string for property.");
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
