//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "IntegerToString.hpp"
#include "RealPart.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
toString(const ArrayOf& A)
{
    wstringVector result;
    auto* ptrValue = (double*)A.getDataPointer();
    ompIndexType elementCount = A.getElementCount();
    for (ompIndexType k = 0; k < elementCount; k++) {
        std::wstring str;
        double dvalue = ptrValue[k];
        if (std::isnan(dvalue)) {
            str = L"NaN";
        } else {
            if (dvalue > 0) {
                if (std::isinf(dvalue)) {
                    str = L"Inf";
                } else {
                    auto ivalue = static_cast<uint64>(dvalue);
                    str = std::to_wstring(ivalue);
                }
            } else {
                if (std::isinf(dvalue)) {
                    str = L"-Inf";
                } else {
                    auto ivalue = static_cast<int64>(dvalue);
                    str = std::to_wstring(ivalue);
                }
            }
        }
        result.push_back(str);
    }
    return result;
}
//=============================================================================
wstringVector
uint64ToString(const ArrayOf& A)
{
    wstringVector result;
    auto* ptrValue = (uint64*)A.getDataPointer();
    indexType elementCount = A.getElementCount();
    for (indexType k = 0; k < elementCount; k++) {
        std::wstring str;
        uint64 ivalue = ptrValue[k];
        str = std::to_wstring(ivalue);
        result.push_back(str);
    }
    return result;
}
//=============================================================================
bool
IntegerToString(ArrayOf A, wstringVector& result, std::wstring& error_message)
{
    result.clear();
    error_message.clear();
    if (A.isEmpty()) {
        result.push_back(L"");
        return true;
    }
    bool bRes = false;
    if (A.isSparse()) {
        error_message = _W("Type not managed in this case.");
        return false;
    }
    NelsonType classA = A.getDataClass();
    switch (classA) {
    case NLS_GO_HANDLE:
    case NLS_HANDLE:
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_FUNCTION_HANDLE:
    default: {
        error_message = _W("Type not managed in this case.");
        return false;
    } break;
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_CHAR: {
        A.promoteType(NLS_DOUBLE);
        result = toString(A);
        bRes = true;
    } break;
    case NLS_UINT64: {
        result = uint64ToString(A);
        bRes = true;
    } break;
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX: {
        ArrayOf B = RealPart(A);
        B.promoteType(NLS_DOUBLE);
        result = toString(B);
        bRes = true;
    } break;
    }
    return bRes;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
