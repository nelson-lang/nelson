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
#include "VersionCompilerFlags.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
VersionCompilerFlags()
{
    wstringVector options;
#ifdef __INTEL_COMPILER
    options.push_back(L"icc");
#else
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

}
//=============================================================================
