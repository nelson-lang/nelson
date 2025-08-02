//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "ComputeNelsonBinariesPath.hpp"
#include "GetVariableEnvironment.hpp"
#include "SetVariableEnvironment.hpp"
#include "NelsonConfiguration.hpp"
#include "i18n.hpp"
#include "DynamicLibrary.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ComputeNelsonBinariesPath(std::wstring& errorMessage)
{
#define NELSON_BINARIES_PATH_ENV L"NELSON_BINARIES_PATH"
    std::wstring penv = GetVariableEnvironment(NELSON_BINARIES_PATH_ENV, L"");
    if (penv != L"") {
        FileSystemWrapper::Path path(penv);
        if (path.is_directory()) {
            NelsonConfiguration::getInstance()->setNelsonBinaryDirectory(path.generic_wstring());
            return true;
        }
    }
    FileSystemWrapper::Path libraryPath
        = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
    // ../prefix/lib/nelson or ../nelson/bin/arch  --> nelson libraries path
    std::wstring prefixBinaryPath = L"NOT_MANAGED";
#ifdef _MSC_VER
#ifdef _WIN64
    prefixBinaryPath = L"/bin/x64";
#else
    prefixBinaryPath = L"/bin/win32";
#endif
#else
#if defined(__APPLE__) || defined(__MACH__)
    prefixBinaryPath = L"/bin/macOS";
#else
    prefixBinaryPath = L"/bin/linux";
#endif
#endif
    FileSystemWrapper::Path binaryPath;
    if (StringHelpers::ends_with(libraryPath.generic_wstring(), prefixBinaryPath)) {
        binaryPath = libraryPath;
    } else {
        binaryPath = libraryPath.parent_path().parent_path().generic_wstring() + L"/bin";
    }

    if (binaryPath.is_directory()) {
        NelsonConfiguration::getInstance()->setNelsonBinaryDirectory(
            binaryPath.getFinalPathname().generic_wstring());
        SetVariableEnvironmentW(
            NELSON_BINARIES_PATH_ENV, binaryPath.getFinalPathname().generic_wstring());
        return true;
    }
    errorMessage = _W("Cannot find Nelson binary path.");
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
