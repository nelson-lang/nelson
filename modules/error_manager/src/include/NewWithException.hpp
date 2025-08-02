//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// Copyright (c) 2002, 2003 Samit Basu
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <cstring>
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
T*
new_with_exception(size_t len, bool initializeToZero = true)
{
    T* ptr = nullptr;
    if (len != 0) {
        try {
            ptr = new T[len];
            if (initializeToZero) {
                memset(ptr, 0, sizeof(T) * len);
            }
        } catch (const std::bad_alloc&) {
            ptr = nullptr;
            Error(ERROR_MEMORY_ALLOCATION);
        }
    }
    return ptr;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
