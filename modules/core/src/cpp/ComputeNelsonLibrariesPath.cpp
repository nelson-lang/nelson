//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdio>
#include "FileSystemWrapper.hpp"
#include "ComputeNelsonBinariesPath.hpp"
#include "GetVariableEnvironment.hpp"
#include "SetVariableEnvironment.hpp"
#include "NelsonConfiguration.hpp"
#include "i18n.hpp"
#include "DynamicLibrary.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ComputeNelsonLibrariesPath(std::wstring& errorMessage)
{
#define CORE_MODULE_NAME L"libnlsCore"
#define NELSON_LIBRARIES_PATH_ENV L"NELSON_LIBRARIES_PATH"
    std::wstring penv = GetVariableEnvironment(NELSON_LIBRARIES_PATH_ENV, L"");
    if (penv != L"") {
        FileSystemWrapper::Path path(penv);
        if (FileSystemWrapper::Path::is_directory(path)) {
            NelsonConfiguration::getInstance()->setNelsonLibraryDirectory(path.generic_wstring());
            return true;
        }
    }
    FileSystemWrapper::Path libraryPath;
    std::wstring currentLibraryName = CORE_MODULE_NAME + get_dynamic_library_extensionW();
    libraryPath = get_dynamic_library_pathW(currentLibraryName);
    if (FileSystemWrapper::Path::is_directory(libraryPath)) {
        NelsonConfiguration::getInstance()->setNelsonLibraryDirectory(
            libraryPath.getFinalPathname().generic_wstring());
        SetVariableEnvironmentW(
            NELSON_LIBRARIES_PATH_ENV, libraryPath.getFinalPathname().generic_wstring());
        return true;
    }
    errorMessage = _W("Cannot find Nelson libraries path.");
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
