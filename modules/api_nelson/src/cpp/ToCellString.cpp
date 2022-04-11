//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ToCellString.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
ToCellStringAs(const stringVector& vectorStr, bool bAsColumn)
{
    ArrayOf* elements = nullptr;
    size_t nbElements = vectorStr.size();
    if (nbElements > 0) {
        try {
            elements = new ArrayOf[nbElements];
        } catch (const std::bad_alloc&) {
            Error(ERROR_MEMORY_ALLOCATION);
        }
        for (size_t k = 0; k < nbElements; k++) {
            elements[k] = ArrayOf::characterArrayConstructor(vectorStr[k]);
        }
    }
    ArrayOf c;
    if (bAsColumn) {
        Dimensions dims(nbElements, 1);
        c = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    } else {
        Dimensions dims(1, nbElements);
        c = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    }
    return c;
}
//=============================================================================
static ArrayOf
ToCellStringAs(const wstringVector& vectorStr, bool bAsColumn)
{
    ArrayOf* elements = nullptr;
    size_t nbElements = vectorStr.size();
    if (nbElements > 0) {
        try {
            elements = new ArrayOf[nbElements];
        } catch (const std::bad_alloc&) {
            Error(ERROR_MEMORY_ALLOCATION);
        }
        for (size_t k = 0; k < nbElements; k++) {
            elements[k] = ArrayOf::characterArrayConstructor(vectorStr[k]);
        }
    }
    ArrayOf c;
    if (bAsColumn) {
        Dimensions dims(nbElements, 1);
        c = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    } else {
        Dimensions dims(1, nbElements);
        c = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    }
    return c;
}
//=============================================================================
ArrayOf
ToCellStringAsRow(const wstringVector& vectorStr)
{
    return ToCellStringAs(vectorStr, false);
}
//=============================================================================
ArrayOf
ToCellStringAsColumn(const wstringVector& vectorStr)
{
    return ToCellStringAs(vectorStr, true);
}
//=============================================================================
ArrayOf
ToCellStringAsRow(const stringVector& vectorStr)
{
    return ToCellStringAs(vectorStr, false);
}
//=============================================================================
ArrayOf
ToCellStringAsColumn(const stringVector& vectorStr)
{
    return ToCellStringAs(vectorStr, true);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
