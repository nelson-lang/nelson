//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "VersionCompilerFlags.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
VersionCompilerFlags()
{
    wstringVector options;
#if defined(__INTEL_LLVM_COMPILER) && defined(__INTEL_COMPILER)
    options.push_back(L"icx");
#else
#if defined(__INTEL_COMPILER)
    options.push_back(L"icc");
#endif
#ifdef _MSC_VER
    options.push_back(L"msvc");
#else
#ifdef __clang__
    options.push_back(L"clang");
#else
#ifdef __GNUC__
    options.push_back(L"gcc");
#else
#ifdef __PGI
    options.push_back(L"pgi");
#else
#if defined(__SUNPRO_C) || defined(__SUNPRO_CC)
    options.push_back(L"sun");
#else
    options.push_back(L"unknown");
#endif
#endif
#endif
#endif
#endif
#endif
#if defined(_NDEBUG) || defined(NDEBUG)
    options.push_back(L"release");
#else
    options.push_back(L"debug");
#endif
#ifdef _MSC_VER
#ifdef _WIN64
    options.push_back(L"64");
#else
    options.push_back(L"32");
#endif
#else
#ifdef _LP64
    options.push_back(L"64");
#else
    options.push_back(L"32");
#endif
#endif
    return options;
}
//=============================================================================

} // namespace Nelson
//=============================================================================
