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
#include "ArrayOf.hpp"
#include "Types.hpp"
#include "Error.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
void*
ArrayOf::allocateArrayOf(
    Class type, indexType length, const stringVector& names, bool initializeValues)
{
    switch (type) {
    case NLS_HANDLE: {
        return (void*)new_with_exception<nelson_handle>(length);
    } break;
    case NLS_STRING_ARRAY: {
        ArrayOf* dp = new_with_exception<ArrayOf>(length);
        for (indexType i = 0; i < length; i++) {
            dp[i] = ArrayOf(NLS_DOUBLE);
        }
        return dp;
    } break;
    case NLS_CELL_ARRAY: {
        ArrayOf* dp = new_with_exception<ArrayOf>(length);
        for (indexType i = 0; i < length; i++) {
            dp[i] = ArrayOf(NLS_DOUBLE);
        }
        return dp;
    } break;
    case NLS_STRUCT_ARRAY: {
        if (!haveValidFieldNames(names)) {
            Error(_W("Field names must be valid."));
        }
        if (!haveUniqueFieldNames(names)) {
            Error(_W("Duplicated field detected."));
        }
        indexType n = (indexType)(length * names.size());
        ArrayOf* dp = new_with_exception<ArrayOf>(n);
        for (indexType i = 0; i < (indexType)(n); i++) {
            dp[i] = ArrayOf(NLS_DOUBLE);
        }
        return dp;
    } break;
    case NLS_LOGICAL: {
        return (void*)new_with_exception<logical>(length, initializeValues);
    } break;
    case NLS_UINT8: {
        return (void*)new_with_exception<uint8>(length, initializeValues);
    } break;
    case NLS_INT8: {
        return (void*)new_with_exception<int8>(length, initializeValues);
    } break;
    case NLS_UINT16: {
        return (void*)new_with_exception<uint16>(length, initializeValues);
    } break;
    case NLS_INT16: {
        return (void*)new_with_exception<int16>(length, initializeValues);
    } break;
    case NLS_UINT32: {
        return (void*)new_with_exception<uint32>(length, initializeValues);
    } break;
    case NLS_INT32: {
        return (void*)new_with_exception<int32>(length, initializeValues);
    } break;
    case NLS_UINT64: {
        return (void*)new_with_exception<uint64>(length, initializeValues);
    } break;
    case NLS_INT64: {
        return (void*)new_with_exception<int64>(length, initializeValues);
    } break;
    case NLS_SINGLE: {
        return (void*)new_with_exception<single>(length, initializeValues);
    } break;
    case NLS_DOUBLE: {
        return (void*)new_with_exception<double>(length, initializeValues);
    } break;
    case NLS_SCOMPLEX: {
        return (void*)new_with_exception<single>(2 * length, initializeValues);
    } break;
    case NLS_DCOMPLEX: {
        return (void*)new_with_exception<double>(2 * length, initializeValues);
    } break;
    case NLS_CHAR: {
        return (void*)new_with_exception<charType>(length, initializeValues);
    } break;
    }
    return nullptr;
}
//=============================================================================
void
ArrayOf::deleteArrayOf(void* dp, Class dataclass)
{
    switch (dataclass) {
    case NLS_HANDLE: {
        nelson_handle* rp = (nelson_handle*)dp;
        delete[] rp;
    } break;
    case NLS_STRING_ARRAY: {
        ArrayOf* rp = (ArrayOf*)dp;
        delete[] rp;
    } break;
    case NLS_CELL_ARRAY: {
        ArrayOf* rp = (ArrayOf*)dp;
        delete[] rp;
    } break;
    case NLS_STRUCT_ARRAY: {
        ArrayOf* rp = (ArrayOf*)dp;
        delete[] rp;
    } break;
    case NLS_LOGICAL: {
        logical* rp = (logical*)dp;
        delete[] rp;
    } break;
    case NLS_UINT8: {
        uint8* rp = (uint8*)dp;
        delete[] rp;
    } break;
    case NLS_INT8: {
        int8* rp = (int8*)dp;
        delete[] rp;
    } break;
    case NLS_UINT16: {
        uint16* rp = (uint16*)dp;
        delete[] rp;
    } break;
    case NLS_INT16: {
        int16* rp = (int16*)dp;
        delete[] rp;
    } break;
    case NLS_UINT32: {
        uint32* rp = (uint32*)dp;
        delete[] rp;
    } break;
    case NLS_INT32: {
        int32* rp = (int32*)dp;
        delete[] rp;
    } break;
    case NLS_UINT64: {
        uint64* rp = (uint64*)dp;
        delete[] rp;
    } break;
    case NLS_INT64: {
        int64* rp = (int64*)dp;
        delete[] rp;
    } break;
    case NLS_SINGLE: {
        single* rp = (single*)dp;
        delete[] rp;
    } break;
    case NLS_DOUBLE: {
        double* rp = (double*)dp;
        delete[] rp;
    } break;
    case NLS_SCOMPLEX: {
        single* rp = (single*)dp;
        delete[] rp;
    } break;
    case NLS_DCOMPLEX: {
        double* rp = (double*)dp;
        delete[] rp;
    } break;
    case NLS_CHAR: {
        charType* rp = (charType*)dp;
        delete[] rp;
    } break;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
