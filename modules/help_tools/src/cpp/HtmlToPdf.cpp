//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "HtmlToPdf.hpp"
#include "DynamicLibrary.hpp"
#include <cstdio>
#include <string>
#include <mutex>
#include <stdexcept>
#include "Error.hpp"
#include "i18n.hpp"
#include "NelsonConfiguration.hpp"
//===================================================================================
namespace Nelson {
//===================================================================================
namespace {
    library_handle nlsGuiHandleDynamicLibrary = nullptr;
    std::once_flag guiLibraryInitFlag;

    void
    logErrorAndThrow(const std::string& message)
    {
        const std::string msg = _(message.c_str());
        const std::string error_msg = get_dynamic_library_error();
        if (!error_msg.empty()) {
            Error(msg + "\n" + error_msg);
        } else {
            Error(msg);
        }
    }

    template <typename FuncPtr>
    FuncPtr
    getFunctionPointer(const char* functionName)
    {
        auto funcPtr
            = reinterpret_cast<FuncPtr>(get_function(nlsGuiHandleDynamicLibrary, functionName));
        if (funcPtr == nullptr) {
            logErrorAndThrow("Gui Function not loaded.");
        }
        return funcPtr;
    }
}
//===================================================================================
static void
initGuiDynamicLibrary()
{
    std::call_once(guiLibraryInitFlag, []() {
        const std::wstring fullpathGuiSharedLibrary
            = L"libnlsGui" + get_dynamic_library_extensionW();
        const std::wstring nelsonLibrariesDirectory
            = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
        const std::wstring libraryPath = nelsonLibrariesDirectory + L"/" + fullpathGuiSharedLibrary;

        nlsGuiHandleDynamicLibrary = load_dynamic_libraryW(libraryPath);
        if (nlsGuiHandleDynamicLibrary == nullptr) {
            logErrorAndThrow("Gui module not loaded.");
        }
    });
}
//===================================================================================
bool
HtmlFileToPdfFile(const std::wstring& htmlsrcfilename, const std::wstring& pdfdestfilename)
{
    using PROC_HtmlFileToPdfFile = bool (*)(std::wstring, std::wstring);
    static PROC_HtmlFileToPdfFile HtmlFileToPdfFilePtr = nullptr;

    initGuiDynamicLibrary();
    if (HtmlFileToPdfFilePtr == nullptr) {
        HtmlFileToPdfFilePtr = getFunctionPointer<PROC_HtmlFileToPdfFile>("HtmlFileToPdfFile");
    }
    return HtmlFileToPdfFilePtr(htmlsrcfilename, pdfdestfilename);
}
//===================================================================================
bool
HtmlStreamToPdfFile(const std::wstring& htmlstream, const std::wstring& pdfdestfilename)
{
    using PROC_HtmlStreamToPdfFile = bool (*)(std::wstring, std::wstring);
    static PROC_HtmlStreamToPdfFile HtmlStreamToPdfFilePtr = nullptr;

    initGuiDynamicLibrary();
    if (HtmlStreamToPdfFilePtr == nullptr) {
        HtmlStreamToPdfFilePtr
            = getFunctionPointer<PROC_HtmlStreamToPdfFile>("HtmlStreamToPdfFile");
    }
    return HtmlStreamToPdfFilePtr(htmlstream, pdfdestfilename);
}
//===================================================================================
} // namespace Nelson
//===================================================================================
