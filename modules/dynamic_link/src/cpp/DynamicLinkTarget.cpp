//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#endif
#include <cstdlib>
#include <cstring>
#include "DynamicLinkTarget.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
IsWasm32DynamicLinkTarget()
{
#ifdef _MSC_VER
    char target[8] = {};
    constexpr DWORD targetBufferSize = static_cast<DWORD>(sizeof(target));
    const DWORD targetLength
        = ::GetEnvironmentVariableA("NELSON_DL_TARGET", target, targetBufferSize);
    return targetLength > 0 && targetLength < targetBufferSize
        && std::strcmp(target, "wasm32") == 0;
#else
    const char* target = std::getenv("NELSON_DL_TARGET");
    return target != nullptr && std::strcmp(target, "wasm32") == 0;
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
