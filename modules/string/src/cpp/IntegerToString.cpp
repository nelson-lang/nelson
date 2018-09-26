//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "IntegerToString.hpp"
#include "RealPart.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
toString(ArrayOf A)
{
    wstringVector result;
    double* ptrValue = (double*)A.getDataPointer();
    for (size_t k = 0; k < A.getDimensions().getElementCount(); k++) {
        std::wstring str;
        double dvalue = ptrValue[k];
        if (std::isnan(dvalue)) {
            str = L"NaN";
        } else {
            if (dvalue > 0) {
                if (std::isinf(dvalue)) {
                    str = L"Inf";
                } else {
                    uint64 ivalue = (uint64)dvalue;
                    str = std::to_wstring(ivalue);
                }
            } else {
                if (std::isinf(dvalue)) {
                    str = L"-Inf";
                } else {
                    int64 ivalue = (int64)dvalue;
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
uint64ToString(ArrayOf A)
{
    wstringVector result;
    uint64* ptrValue = (uint64*)A.getDataPointer();
    for (size_t k = 0; k < A.getDimensions().getElementCount(); k++) {
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
    error_message = L"";
    if (A.isEmpty()) {
        result.push_back(L"");
        return true;
    } else {
        bool bRes = false;
        if (A.isSparse()) {
            error_message = _W("Type not managed in this case.");
            return false;
        }
        Class classA = A.getDataClass();
        switch (classA) {
        case NLS_HANDLE:
        case NLS_STRING_ARRAY:
        case NLS_CELL_ARRAY:
        case NLS_STRUCT_ARRAY:
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
}
//=============================================================================
}
//=============================================================================
