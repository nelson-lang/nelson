//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
#include "ImagPart.hpp"
#include "ClassName.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ImagPart(ArrayOf arrayIn)
{
    ArrayOf res;
    if (arrayIn.isSparse()) {
        Error(_W("Undefined function '") + utf8_to_wstring(ClassName(arrayIn)) + L"_imag'");
    }
    switch (arrayIn.getDataClass()) {
    case NLS_SCOMPLEX: {
        size_t len = arrayIn.getLength();
        void* ptr = ArrayOf::allocateArrayOf(arrayIn.getDataClass(), len);
        auto* rp = static_cast<single*>(ptr);
        auto* sp = (single*)arrayIn.getDataPointer();
        for (size_t i = 0; i < len; i++) {
            rp[i] = sp[2 * i + 1];
        }
        res = ArrayOf(NLS_SINGLE, arrayIn.getDimensions(), rp);
    } break;
    case NLS_DCOMPLEX: {
        size_t len = arrayIn.getLength();
        void* ptr = ArrayOf::allocateArrayOf(arrayIn.getDataClass(), len);
        auto* rp = static_cast<double*>(ptr);
        auto* dp = (double*)arrayIn.getDataPointer();
        for (size_t i = 0; i < len; i++) {
            rp[i] = dp[2 * i + 1];
        }
        res = ArrayOf(NLS_DOUBLE, arrayIn.getDimensions(), rp);
    } break;
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_STRUCT_ARRAY:
    default: {
        Error(_W("Undefined function '") + utf8_to_wstring(ClassName(arrayIn)) + L"_imag'");
    } break;
    case NLS_CHAR:
    case NLS_DOUBLE:
    case NLS_LOGICAL: {
        size_t len = arrayIn.getLength();
        void* ptr = ArrayOf::allocateArrayOf(arrayIn.getDataClass(), len);
        res = ArrayOf(NLS_DOUBLE, arrayIn.getDimensions(), ptr);
    } break;
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE: {
        size_t len = arrayIn.getLength();
        void* ptr = ArrayOf::allocateArrayOf(arrayIn.getDataClass(), len);
        res = ArrayOf(arrayIn.getDataClass(), arrayIn.getDimensions(), ptr);
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
