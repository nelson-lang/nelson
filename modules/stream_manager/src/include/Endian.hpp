//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "Types.hpp"
#include "nlsStream_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename T>
T
bswap(T val)
{
    T retVal;
    char* pVal = (char*)&val;
    char* pRetVal = (char*)&retVal;
    int size = sizeof(T);
    for (int i = 0; i < size; i++) {
        pRetVal[size - 1 - i] = pVal[i];
    }
    return retVal;
}
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP bool
BITSWAP(void* ptrReadWrite, size_t count, NelsonType destClass);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP bool
isLittleEndianFormat();
//=============================================================================
}; // namespace Nelson
//=============================================================================
