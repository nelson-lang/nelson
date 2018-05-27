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
#include "Endian.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
isLittleEndianFormat()
{
    int littlendian = 1;
    char* endptr = (char*)&littlendian;
    return ((int)*endptr == 0) ? false : true;
}
//=============================================================================
bool
BITSWAP(void* ptrReadWrite, size_t count, Class destClass)
{
    switch (destClass) {
    case NLS_LOGICAL: {
        logical* buffer = (logical*)ptrReadWrite;
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<logical>(buffer[k]);
        }
    } break;
    case NLS_UINT8: {
        uint8* buffer = (uint8*)ptrReadWrite;
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<uint8>(buffer[k]);
        }
    } break;
    case NLS_INT8: {
        int8* buffer = (int8*)ptrReadWrite;
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<int8>(buffer[k]);
        }
    } break;
    case NLS_UINT16: {
        uint16* buffer = (uint16*)ptrReadWrite;
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<uint16>(buffer[k]);
        }
    } break;
    case NLS_INT16: {
        int16* buffer = (int16*)ptrReadWrite;
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<int16>(buffer[k]);
        }
    } break;
    case NLS_UINT32: {
        uint32* buffer = (uint32*)ptrReadWrite;
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<uint32>(buffer[k]);
        }
    } break;
    case NLS_INT32: {
        int32* buffer = (int32*)ptrReadWrite;
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<int32>(buffer[k]);
        }
    } break;
    case NLS_UINT64: {
        uint64* buffer = (uint64*)ptrReadWrite;
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<uint64>(buffer[k]);
        }
    } break;
    case NLS_INT64: {
        int64* buffer = (int64*)ptrReadWrite;
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<int64>(buffer[k]);
        }
    } break;
    case NLS_SINGLE: {
        single* buffer = (single*)ptrReadWrite;
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<single>(buffer[k]);
        }
    } break;
    case NLS_DOUBLE: {
        double* buffer = (double*)ptrReadWrite;
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<double>(buffer[k]);
        }
    } break;
    default: {
        return false;
    } break;
    }
    return true;
}
//=============================================================================
}
//=============================================================================
