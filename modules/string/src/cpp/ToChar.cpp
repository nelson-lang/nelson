//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ToChar.hpp"
#include "IEEEFP.hpp"
#include "VertCat.hpp"
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
ArrayOfDoubleToChar(const ArrayOf& A)
{
    Dimensions dimsA = A.dimensions();
    charType* res = (charType*)ArrayOf::allocateArrayOf(
        NLS_CHAR, dimsA.elementCount(), stringVector(), false);
    auto* pDouble = (double*)A.dataPointer();
    for (indexType k = 0; k < A.elementCount(); k++) {
        double v = pDouble[k];
        if (IsFinite(v)) {
            if (v < 0) {
                res[k] = static_cast<charType>(0);
            } else {
                if (v > 65535) {
                    res[k] = static_cast<charType>(65535);
                } else {
                    res[k] = static_cast<charType>(v);
                }
            }
        } else {
            if (IsNaN(v)) {
                res[k] = static_cast<charType>(0);
            } else {
                if (v <= 0) {
                    res[k] = static_cast<charType>(0);
                } else if (IsInfinite(v)) {
                    res[k] = static_cast<charType>(65535);
                }
            }
        }
    }
    return ArrayOf(NLS_CHAR, dimsA, res);
}
//=============================================================================
ArrayOf
ToChar(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    ArrayOf res;
    ArrayOf charA = ToChar(A, needToOverload);
    if (needToOverload) {
        return res;
    }
    ArrayOf charB = ToChar(B, needToOverload);
    if (needToOverload) {
        return res;
    }
    Dimensions dimsA = charA.dimensions();
    Dimensions dimsB = charB.dimensions();
    wstringVector vA = charA.asWideStringVector();
    if (vA.size() == 0) {
        vA.push_back(L"");
    }
    wstringVector vB = charB.asWideStringVector();
    if (vB.size() == 0) {
        vB.push_back(L"");
    }
    size_t lenMax = 0;
    for (auto& i : vA) {
        if (lenMax < i.size()) {
            lenMax = i.size();
        }
    }
    for (auto& i : vB) {
        if (lenMax < i.size()) {
            lenMax = i.size();
        }
    }
    for (auto& i : vA) {
        size_t newLen = lenMax - i.size();
        if (newLen > 0) {
            for (size_t q = 0; q < newLen; q++) {
                i = i + std::wstring(L" ");
            }
        }
    }
    for (auto& i : vB) {
        size_t newLen = lenMax - i.size();
        if (newLen > 0) {
            for (size_t q = 0; q < newLen; q++) {
                i = i + std::wstring(L" ");
            }
        }
    }
    if (lenMax == 0) {
        // Both operands are empty: the result is a 0x0 character array.
        return ArrayOf(NLS_CHAR, Dimensions(0, 0), nullptr);
    }
    // vA and vB are already padded to the common width; stack all rows in one
    // pass instead of vertically concatenating one row at a time (O(n^2)).
    wstringVector allRows;
    allRows.reserve(vA.size() + vB.size());
    for (const auto& i : vA) {
        allRows.push_back(i);
    }
    for (const auto& i : vB) {
        allRows.push_back(i);
    }
    return ArrayOf::characterArrayFromRows(allRows, false);
}
//=============================================================================
ArrayOf
ToChar(const ArrayOf& A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    switch (A.dataClass()) {
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY: {
        auto* arg = (ArrayOf*)(A.dataPointer());
        indexType elementCount = A.elementCount();
        // Collect every row of every element, then stack once. Accumulating with the
        // two-argument ToChar would re-pad and re-copy the growing result (O(N^2)).
        wstringVector allRows;
        for (indexType k = 0; k < elementCount; k++) {
            ArrayOf val = ToChar(arg[k], needToOverload);
            if (needToOverload) {
                return res;
            }
            wstringVector rows = val.asWideStringVector();
            if (rows.empty()) {
                allRows.push_back(L"");
            } else {
                for (auto& r : rows) {
                    allRows.push_back(r);
                }
            }
        }
        if (allRows.empty()) {
            return ArrayOf(NLS_CHAR, Dimensions(0, 0), nullptr);
        }
        size_t maxLen = 0;
        for (const auto& r : allRows) {
            maxLen = (maxLen < r.size()) ? r.size() : maxLen;
        }
        if (maxLen == 0) {
            // Every row is empty: the result is a 0x0 character array.
            return ArrayOf(NLS_CHAR, Dimensions(0, 0), nullptr);
        }
        res = ArrayOf::characterArrayFromRows(allRows, false);
    } break;
    case NLS_CHAR: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    } break;
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE: {
        ArrayOf AA = A;
        AA.promoteType(NLS_DOUBLE);
        res = ArrayOfDoubleToChar(AA);
    } break;
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
    default: {
        needToOverload = true;
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
