//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "StringTrim.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
trimCellElement(const ArrayOf& element);
//=============================================================================
ArrayOf
TrimCharacterArray(const ArrayOf& A, bool trimLeft, bool nulIsBlank)
{
    if (!A.is2D()) {
        Error(L"Nelson:string:trim:notTwoDimensional",
            L"Only character arrays with up to two dimensions are accepted.");
    }
    Dimensions dims = A.dimensions();
    indexType rows = dims.rows();
    indexType cols = dims.columns();
    const auto* data = static_cast<const charType*>(A.dataPointer());
    auto blankColumn = [&](indexType c) {
        for (indexType r = 0; r < rows; ++r) {
            charType ch = data[r + c * rows];
            if (!IsTextWhitespace(ch) && !(nulIsBlank && ch == 0)) {
                return false;
            }
        }
        return true;
    };
    indexType first = 0;
    if (trimLeft) {
        while (first < cols && blankColumn(first)) {
            ++first;
        }
    }
    indexType last = cols;
    while (last > first && blankColumn(last - 1)) {
        --last;
    }
    if (first == last) {
        return ArrayOf::characterArray(L"");
    }
    if (first == 0 && last == cols) {
        return A;
    }
    indexType width = last - first;
    auto* out = static_cast<charType*>(
        ArrayOf::allocateArrayOf(NLS_CHAR, rows * width, stringVector(), false));
    memcpy(out, data + first * rows, static_cast<size_t>(rows * width) * sizeof(charType));
    return ArrayOf(NLS_CHAR, Dimensions(rows, width), out);
}
//=============================================================================
ArrayOf
StringTrim(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    if (A.isCharacterArray()) {
        return TrimCharacterArray(A, true, false);
    }
    if (A.isCell()) {
        if (A.isEmpty()) {
            return A;
        }
        ArrayOf res(A);
        res.ensureSingleOwner();
        auto* element = (ArrayOf*)res.dataPointer();
        indexType elementCount = A.elementCount();
        for (indexType k = 0; k < elementCount; k++) {
            element[k] = trimCellElement(element[k]);
        }
        return res;
    }
    if (A.isStringArray()) {
        if (A.isEmpty()) {
            return A;
        }
        ArrayOf res(A);
        res.ensureSingleOwner();
        auto* element = (ArrayOf*)res.dataPointer();
        ompIndexType elementCount = A.elementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            if (element[k].isCharacterArray()) {
                element[k] = TrimCharacterArray(element[k], true, false);
            }
        }
        return res;
    }
    needToOverload = true;
    return {};
}
//=============================================================================
// A cell element is a character array (trimmed), a string scalar (trimmed),
// or a cell (kept as is); any other type is rejected.
static ArrayOf
trimCellElement(const ArrayOf& element)
{
    if (element.isCharacterArray()) {
        return TrimCharacterArray(element, true, false);
    }
    if (element.isCell()) {
        return element;
    }
    if (element.isStringArray() && element.isScalar()) {
        bool over = false;
        return StringTrim(element, over);
    }
    Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
        L"cell of strings");
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
