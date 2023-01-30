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
#include <cstdio>
#include "FileSystemWrapper.hpp"
#include "ComputeNelsonBinariesPath.hpp"
#include "GetVariableEnvironment.hpp"
#include "NelsonConfiguration.hpp"
#include "i18n.hpp"
#include "DynamicLibrary.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ComputeNelsonBinariesPath()
{
#define CORE_MODULE_NAME L"libnlsCore"
#define NELSON_BINARIES_PATH_ENV L"NELSON_BINARIES_PATH"
    std::wstring penv = GetVariableEnvironment(NELSON_BINARIES_PATH_ENV, L"");
    if (penv != L"") {
        FileSystemWrapper::Path path(penv);
        if (FileSystemWrapper::Path::is_directory(path)) {
            NelsonConfiguration::getInstance()->setNelsonBinaryDirectory(path.generic_wstring());
            return true;
        }
    }
    std::wstring nelsonPath = NelsonConfiguration::getInstance()->getNelsonRootDirectory();
    FileSystemWrapper::Path binpath(nelsonPath);
    std::wstring currentLibraryName = CORE_MODULE_NAME + get_dynamic_library_extensionW();
    binpath = get_dynamic_library_pathW(currentLibraryName);
    if (FileSystemWrapper::Path::is_directory(binpath)) {
        NelsonConfiguration::getInstance()->setNelsonBinaryDirectory(binpath.generic_wstring());
        return true;
    }
    fprintf(stderr, "%s\n", _("Error: we cannot find Nelson binaries path.").c_str());
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
