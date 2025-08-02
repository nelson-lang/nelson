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
    Dimensions dimsA = A.getDimensions();
    std::wstring res;
    res.reserve(A.getElementCount());
    auto* pDouble = (double*)A.getDataPointer();
    for (indexType k = 0; k < A.getElementCount(); k++) {
        double v = pDouble[k];
        if (IsFinite(v)) {
            if (v < 0) {
                res.push_back(static_cast<charType>(0));
            } else {
                if (v > 65535) {
                    res.push_back(static_cast<charType>(65535));
                } else {
                    res.push_back(static_cast<charType>(v));
                }
            }
        } else {
            if (IsNaN(v)) {
                res.push_back(static_cast<charType>(0));
            } else {
                if (v <= 0) {
                    res.push_back(static_cast<charType>(0));
                } else if (IsInfinite(v)) {
                    res.push_back(static_cast<charType>(65535));
                }
            }
        }
    }
    ArrayOf result = ArrayOf::characterArrayConstructor(res);
    result.reshape(dimsA);
    return result;
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
    Dimensions dimsA = charA.getDimensions();
    Dimensions dimsB = charB.getDimensions();
    wstringVector vA = charA.getContentAsWideStringVector();
    if (vA.size() == 0) {
        vA.push_back(L"");
    }
    wstringVector vB = charB.getContentAsWideStringVector();
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
    ArrayOfVector _args;
    res = ArrayOf::characterArrayConstructor(vA[0]);
    for (size_t i = 0; i < vA.size(); i++) {
        _args << ArrayOf::characterArrayConstructor(vA[i]);
    }
    res = VertCat(_args, NLS_CHAR);
    _args.clear();
    _args << res;
    for (auto& i : vB) {
        _args << ArrayOf::characterArrayConstructor(i);
    }
    return VertCat(_args, NLS_CHAR);
}
//=============================================================================
ArrayOf
ToChar(const ArrayOf& A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    switch (A.getDataClass()) {
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY: {
        ArrayOfVector V;
        auto* arg = (ArrayOf*)(A.getDataPointer());
        indexType elementCount = A.getElementCount();
        for (indexType k = 0; k < elementCount; k++) {
            ArrayOf val = ToChar(arg[k], needToOverload);
            if (needToOverload) {
                return res;
            }
            V.push_back(val);
        }
        res = V[0];
        for (size_t k = 1; k < V.size(); k++) {
            ArrayOf val = ToChar(res, V[k], needToOverload);
            if (needToOverload) {
                return res;
            }
            res = val;
        }
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
