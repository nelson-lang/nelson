//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PlatformInfo.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
IsPc()
{
    return ArrayOf::logicalScalar(IsPcPlatform());
}
//=============================================================================
ArrayOf
IsMac()
{
    return ArrayOf::logicalScalar(IsMacPlatform());
}
//=============================================================================
ArrayOf
IsUnix()
{
    return ArrayOf::logicalScalar(IsUnixPlatform());
}
//=============================================================================
ArrayOf
IsWasm()
{
    return ArrayOf::logicalScalar(IsWasmPlatform());
}
//=============================================================================
bool
IsPcPlatform()
{
#ifdef _MSC_VER
    return true;
#else
    return false;
#endif
}
//=============================================================================
bool
IsMacPlatform()
{
    if (IsUnixPlatform()) {
#if defined(__APPLE__) || defined(__MACH__)
        return true;
#endif
    }
    return false;
}
//=============================================================================
bool
IsUnixPlatform()
{
#ifdef _MSC_VER
    return false;
#else
    return true;
#endif
}
//=============================================================================
bool
IsWasmPlatform()
{
#if defined(__EMSCRIPTEN__) || defined(__wasm__)
    return true;
#else
    return false;
#endif
}
//=============================================================================
std::wstring
GetArchitecture()
{
    if (IsPcPlatform()) {
#ifdef _M_ARM64
        return std::wstring(L"woa64");
#else
#ifdef _WIN64
        return std::wstring(L"win64");
#else
        return std::wstring(L"win32");
#endif
#endif
    }
    if (IsMacPlatform()) {
#if defined(__arm64__) || defined(__aarch64__)
        return std::wstring(L"maca64");
#elif defined(__x86_64__)
        return std::wstring(L"maci64");
#else
        return std::wstring(L"maci32");
#endif
    }
    if (IsWasmPlatform()) {
        return std::wstring(L"wasm32");
    }
#if defined(__x86_64__) || defined(__aarch64__)
    return std::wstring(L"glnxa64");
#else
    return std::wstring(L"glnxa32");
#endif

    return std::wstring(L"?");
}
//=============================================================================
std::wstring
GetArchitectureType()
{
    if (IsPcPlatform()) {
#ifdef _M_ARM64
        return std::wstring(L"PCWOA64");
#else
#ifdef _WIN64
        return std::wstring(L"PCWIN64");
#else
        return std::wstring(L"PCWIN");
#endif
#endif
    }
    if (IsMacPlatform()) {
#if defined(__arm64__) || defined(__aarch64__)
        return std::wstring(L"MACA64");
#elif defined(__x86_64__)
        return std::wstring(L"MACI64");
#else
        return std::wstring(L"MACI32");
#endif
    }
    if (IsWasmPlatform()) {
        return std::wstring(L"WASM32");
    }
#ifdef __x86_64__
    return std::wstring(L"GLNXA64");
#else
    return std::wstring(L"GLNXA32");
#endif

    return std::wstring(L"?");
}
//=============================================================================
double
GetMaxArrayOfSizeSupported()
{
    return static_cast<double>(SIZE_TYPE_MAX);
}
//=============================================================================
NLSOS_FUNCTIONS_IMPEXP bool
IsBigEndian()
{
    union
    {
        uint32_t i;
        char c[4];
    } bint = { 0x01020304 };
    return (bint.c[0] == 1);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
