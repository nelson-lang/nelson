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
    std::wstring mexext;
#ifdef _MSC_VER
#ifdef _M_ARM64
    mexext = L"nexwoa64";
#else
#ifdef _WIN64
    mexext = L"nexw64";
#else
#ifdef _WIN32
    mexext = L"nexw32";
#endif
#endif
#endif
#else
#if defined(__APPLE__) || defined(__MACH__)
#ifdef __x86_64__
    mexext = L"nexmaci64";
#else
#if defined(__arm64__) || defined(__aarch64__)
    mexext = L"nexmacm1";
#else
    mexext = L"nexmaci";
#endif
#endif
#else
#ifdef __x86_64__
    mexext = L"nexa64";
#else
    mexext = L"nexglx";
#endif
#endif
#endif
    return mexext;
}
//=============================================================================
}
//=============================================================================
