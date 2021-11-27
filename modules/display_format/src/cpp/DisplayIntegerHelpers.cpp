//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "DisplayIntegerHelpers.hpp"
#include "Hexify.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename T>
std::wstring
formatSignedInteger(const void* ptr, indexType index, NumericFormatDisplay currentNumericFormat)
{
    const T* values = static_cast<const T*>(ptr);
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_HEX: {
        return hexifyInteger<T>(values[index]);
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        if (values[index] == (T)0) {
            return L" ";
        } else if (values[index] < (T)0) {
            return L"-";
        } else {
            return L"+";
        }
    }
    default: {
        return fmt::to_wstring(values[index]);
    } break;
    }
    return L"";
}
//=============================================================================
template <typename T>
std::wstring
formatUnsignedInteger(const void* ptr, indexType index, NumericFormatDisplay currentNumericFormat)
{
    const T* values = static_cast<const T*>(ptr);
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_HEX: {
        return hexifyInteger<T>(values[index]);
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        return (values[index] == (T)0) ? L" " : L"+";
    }
    default: {
        return fmt::to_wstring(values[index]);
    } break;
    }
    return L"";
}
//=============================================================================
std::wstring
formatInteger(const void* ptrScalar, Class integerType, indexType index,
    NumericFormatDisplay currentNumericFormat)
{
    switch (integerType) {
    case NLS_UINT8: {
        return formatUnsignedInteger<uint8>(ptrScalar, index, currentNumericFormat);
    } break;
    case NLS_INT8: {
        return formatSignedInteger<int8>(ptrScalar, index, currentNumericFormat);
    } break;
    case NLS_UINT16: {
        return formatUnsignedInteger<uint16>(ptrScalar, index, currentNumericFormat);
    } break;
    case NLS_INT16: {
        return formatSignedInteger<int16>(ptrScalar, index, currentNumericFormat);
    } break;
    case NLS_UINT32: {
        return formatUnsignedInteger<uint32>(ptrScalar, index, currentNumericFormat);
    } break;
    case NLS_INT32: {
        return formatSignedInteger<int32>(ptrScalar, index, currentNumericFormat);
    } break;
    case NLS_UINT64: {
        return formatUnsignedInteger<uint64>(ptrScalar, index, currentNumericFormat);
    } break;
    case NLS_INT64: {
        return formatSignedInteger<int64>(ptrScalar, index, currentNumericFormat);
    } break;
    default: { } break; }
    return L"";
}
//=============================================================================

}
//=============================================================================
