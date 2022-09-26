//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
    binpath += L"/bin/macOS";
#else
    binpath += L"/bin/linux";
#endif
#endif
    if (boost::filesystem::is_directory(binpath)) {
        return binpath.generic_wstring();
    }
    fprintf(stderr, "%s\n", _("Error: we cannot find Nelson binaries path.").c_str());
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
