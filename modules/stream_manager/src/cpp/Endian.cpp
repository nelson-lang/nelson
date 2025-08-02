//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
    char* endptr = reinterpret_cast<char*>(&littlendian);
    return (static_cast<int>(*endptr) == 0) ? false : true;
}
//=============================================================================
bool
BITSWAP(void* ptrReadWrite, size_t count, NelsonType destClass)
{
    switch (destClass) {
    case NLS_LOGICAL: {
        auto* buffer = static_cast<logical*>(ptrReadWrite);
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<logical>(buffer[k]);
        }
    } break;
    case NLS_UINT8: {
        auto* buffer = static_cast<uint8*>(ptrReadWrite);
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<uint8>(buffer[k]);
        }
    } break;
    case NLS_INT8: {
        int8* buffer = static_cast<int8*>(ptrReadWrite);
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<int8>(buffer[k]);
        }
    } break;
    case NLS_UINT16: {
        auto* buffer = static_cast<uint16*>(ptrReadWrite);
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<uint16>(buffer[k]);
        }
    } break;
    case NLS_INT16: {
        auto* buffer = static_cast<int16*>(ptrReadWrite);
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<int16>(buffer[k]);
        }
    } break;
    case NLS_UINT32: {
        auto* buffer = static_cast<uint32*>(ptrReadWrite);
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<uint32>(buffer[k]);
        }
    } break;
    case NLS_INT32: {
        auto* buffer = static_cast<int32*>(ptrReadWrite);
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<int32>(buffer[k]);
        }
    } break;
    case NLS_UINT64: {
        auto* buffer = static_cast<uint64*>(ptrReadWrite);
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<uint64>(buffer[k]);
        }
    } break;
    case NLS_INT64: {
        auto* buffer = static_cast<int64*>(ptrReadWrite);
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<int64>(buffer[k]);
        }
    } break;
    case NLS_SINGLE: {
        auto* buffer = static_cast<single*>(ptrReadWrite);
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<single>(buffer[k]);
        }
    } break;
    case NLS_DOUBLE: {
        auto* buffer = static_cast<double*>(ptrReadWrite);
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
} // namespace Nelson
//=============================================================================
