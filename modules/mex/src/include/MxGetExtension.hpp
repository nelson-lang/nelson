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
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * Get current platform MEX file extension
 */
static std::wstring
getMexExtension()
{
#if defined(_MSC_VER) // ----- Windows -----
#if defined(_M_ARM64)
    return L"nexwoa64";
#elif defined(_WIN64)
    return L"nexw64";
#elif defined(_WIN32)
    return L"nexw32";
#else
    return L"nexw"; // Fallback improbable
#endif

#elif defined(__APPLE__) || defined(__MACH__) // ----- macOS -----
#if defined(__x86_64__)
    return L"nexmaci64";
#elif defined(__arm64__) || defined(__aarch64__)
    return L"nexmacm1";
#else
    return L"nexmaci";
#endif

#else // ----- Linux / Unix -----
#if defined(__x86_64__)
    return L"nexa64";
#else
    return L"nexglx";
#endif
#endif
}
//=============================================================================
}
//=============================================================================
