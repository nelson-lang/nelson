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
    // Every supported class does the same byte-swap loop, differing only in the
    // element type; a generic lambda factors the eleven identical cases.
    auto swapBuffer = [ptrReadWrite, count](auto tag) {
        using T = decltype(tag);
        auto* buffer = static_cast<T*>(ptrReadWrite);
        for (size_t k = 0; k < count; k++) {
            buffer[k] = bswap<T>(buffer[k]);
        }
    };
    switch (destClass) {
    case NLS_LOGICAL:
        swapBuffer(logical {});
        break;
    case NLS_UINT8:
        swapBuffer(uint8 {});
        break;
    case NLS_INT8:
        swapBuffer(int8 {});
        break;
    case NLS_UINT16:
        swapBuffer(uint16 {});
        break;
    case NLS_INT16:
        swapBuffer(int16 {});
        break;
    case NLS_UINT32:
        swapBuffer(uint32 {});
        break;
    case NLS_INT32:
        swapBuffer(int32 {});
        break;
    case NLS_UINT64:
        swapBuffer(uint64 {});
        break;
    case NLS_INT64:
        swapBuffer(int64 {});
        break;
    case NLS_SINGLE:
        swapBuffer(single {});
        break;
    case NLS_DOUBLE:
        swapBuffer(double {});
        break;
    default:
        return false;
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
