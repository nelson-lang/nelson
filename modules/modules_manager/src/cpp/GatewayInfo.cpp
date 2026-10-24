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
#include "GatewayInfo.hpp"
#include "i18n.hpp"
#include "FindDynamicLibraryName.hpp"
#include "characters_encoding.hpp"
#include "DynamicLibrary.hpp"
#include "NelsonGateway.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define GATEWAY_DESCRIPTOR_ENTRY "GetGatewayDescriptor"
//=============================================================================
class CurrentDirectoryGuard
{
public:
    explicit CurrentDirectoryGuard(const FileSystemWrapper::Path& newDirectory)
    {
        previousDirectory = FileSystemWrapper::Path::current_path();
        FileSystemWrapper::Path::current_path(newDirectory);
    }

    ~CurrentDirectoryGuard() { FileSystemWrapper::Path::current_path(previousDirectory); }

private:
    FileSystemWrapper::Path previousDirectory;
};
//=============================================================================
using PROC_GetGatewayDescriptor = const Nelson::nlsGatewayDescriptor* (*)();
//=============================================================================
bool
GatewayInfo(const std::wstring& dynlibname, std::wstring& moduleName, stringVector& functionsList,
    std::wstring& errorMessage)
{
    errorMessage.clear();
    moduleName.clear();
    functionsList.clear();

    FileSystemWrapper::Path p(dynlibname);
    p = p.generic_wstring();
    FileSystemWrapper::Path dir = p.parent_path();
    if (dir.generic_wstring().empty()) {
        dir = FileSystemWrapper::Path::current_path();
    }
    std::wstring filename
        = FindDynamicLibraryName(dir.generic_wstring(), p.filename().generic_wstring(), false);
    if (filename.empty()) {
        errorMessage = formatI18nMessage(
            resolveMessageTemplate(L"Nelson:modules_manager:fileFound", L"File not found: %s"),
            p.generic_wstring());
        return false;
    }

    library_handle handle = nullptr;
    {
        CurrentDirectoryGuard guard(dir);
#ifdef _MSC_VER
        handle = load_dynamic_libraryW(filename);
#else
        handle = load_dynamic_library(wstring_to_utf8(filename));
#endif
    }
    if (handle == nullptr) {
        errorMessage = formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:modules_manager:moduleLoadedLibraryLoaded",
                L"Module not loaded: library not loaded.\n%s\n%s"),
            filename, utf8_to_wstring(get_dynamic_library_error()));
        return false;
    }

    auto descriptorPtr = reinterpret_cast<PROC_GetGatewayDescriptor>(
        get_function(handle, GATEWAY_DESCRIPTOR_ENTRY));
    if (descriptorPtr == nullptr) {
        close_dynamic_library(handle);
        errorMessage
            = resolveMessageTemplate(L"Nelson:modules_manager:moduleLoadedGatewayDescriptor2",
                L"Module not loaded: gateway descriptor not found.");
        return false;
    }
    const nlsGatewayDescriptor* descriptor = descriptorPtr();
    if (descriptor == nullptr || descriptor->gatewayName == nullptr
        || descriptor->gateway == nullptr || descriptor->nbBuiltins == 0) {
        close_dynamic_library(handle);
        errorMessage
            = resolveMessageTemplate(L"Nelson:modules_manager:moduleLoadedGatewayDescriptor",
                L"Module not loaded: gateway descriptor is invalid.");
        return false;
    }

    moduleName = descriptor->gatewayName;
    functionsList.reserve(descriptor->nbBuiltins);
    try {
        for (size_t k = 0; k < descriptor->nbBuiltins; k++) {
            functionsList.push_back(descriptor->gateway[k].functionName);
        }
    } catch (...) {
        close_dynamic_library(handle);
        errorMessage
            = resolveMessageTemplate(L"Nelson:modules_manager:moduleLoadedGatewayDescriptor",
                L"Module not loaded: incompatible gateway library (rebuild required).");
        return false;
    }
    close_dynamic_library(handle);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
