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
    return ArrayOf::logicalConstructor(IsPcPlatform());
}
//=============================================================================
ArrayOf
IsMac()
{
    return ArrayOf::logicalConstructor(IsMacPlatform());
}
//=============================================================================
ArrayOf
IsUnix()
{
    return ArrayOf::logicalConstructor(IsUnixPlatform());
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
#ifdef __x86_64__
        return std::wstring(L"maci64");
#else
        return std::wstring(L"maci32");
#endif
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
#ifdef __x86_64__
        return std::wstring(L"MACI64");
#else
        return std::wstring(L"MACI32");
#endif
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
