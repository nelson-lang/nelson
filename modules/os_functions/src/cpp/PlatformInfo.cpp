//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
#ifdef _WIN64
        return std::wstring(L"win64");
#else
        return std::wstring(L"win32");
#endif
    } else if (IsMacPlatform()) {
#ifdef __x86_64__
        return std::wstring(L"maci64");
#else
        return std::wstring(L"maci32");
#endif
    } else {
#ifdef __x86_64__
        return std::wstring(L"glnxa64");
#else
        return std::wstring(L"glnxa32");
#endif
    }
    return std::wstring(L"?");
}
//=============================================================================
std::wstring
GetArchitectureType()
{
    if (IsPcPlatform()) {
#ifdef _WIN64
        return std::wstring(L"PCWIN64");
#else
        return std::wstring(L"PCWIN");
#endif
    } else if (IsMacPlatform()) {
#ifdef __x86_64__
        return std::wstring(L"MACI64");
#else
        return std::wstring(L"MACI32");
#endif
    } else {
#ifdef __x86_64__
        return std::wstring(L"GLNXA64");
#else
        return std::wstring(L"GLNXA32");
#endif
    }
    return std::wstring(L"?");
}
//=============================================================================
double
GetMaxArrayOfSizeSupported()
{
    return (double)SIZE_TYPE_MAX;
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
    return (bool)(bint.c[0] == 1);
}
//=============================================================================
}
//=============================================================================
