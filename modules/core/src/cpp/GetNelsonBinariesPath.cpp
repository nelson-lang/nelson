//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#include <Windows.h>
#endif
#include "GetNelsonBinariesPath.hpp"
#include "GetNelsonPath.hpp"
#include "GetVariableEnvironment.hpp"
#include "i18n.hpp"
#include <boost/filesystem.hpp>
#include <cstdio>
//=============================================================================
using namespace boost::filesystem;
//=============================================================================
namespace Nelson {

//=============================================================================
std::wstring
GetNelsonBinariesPath()
{
#define NELSON_BINARIES_PATH_ENV L"NELSON_BINARIES_PATH"
    std::wstring penv = GetVariableEnvironment(NELSON_BINARIES_PATH_ENV, L"");
    if (penv != L"") {
        boost::filesystem::path path(penv);
        if (boost::filesystem::is_directory(path)) {
            return path.generic_wstring();
        }
    }
    std::wstring nelsonPath = GetNelsonPath();
    boost::filesystem::path binpath(nelsonPath);
#ifdef _MSC_VER
#ifdef _WIN64
    binpath += L"/bin/x64";
#else
    binpath += L"/bin/win32";
#endif
#else
#if defined(__APPLE__) || defined(__MACH__)
#ifdef __x86_64__
    binpath += L"/bin/macosx64";
#else
    binpath += L"/bin/macosx32";
#endif
#else
#if defined(__x86_64__) || defined(__aarch64__)
    binpath += L"/bin/linux64";
#else
    binpath += L"/bin/linux32";
#endif
#endif
#endif
    if (boost::filesystem::is_directory(binpath)) {
        return binpath.generic_wstring();
    }
    fprintf(stderr, "%s\n", _("Error: we cannot find Nelson binaries path.").c_str());
    return std::wstring();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
