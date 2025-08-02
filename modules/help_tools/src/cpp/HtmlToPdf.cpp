//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/function.hpp>
#include "HtmlToPdf.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "DynamicLibrary.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
static library_handle nlsGuiHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
//=============================================================================
static void
initGuiDynamicLibrary()
{
    if (bFirstDynamicLibraryCall) {
        std::wstring fullpathGuiSharedLibrary = L"libnlsGui" + get_dynamic_library_extensionW();
        std::wstring nelsonLibrariesDirectory
            = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
        fullpathGuiSharedLibrary
            = nelsonLibrariesDirectory + std::wstring(L"/") + fullpathGuiSharedLibrary;
        nlsGuiHandleDynamicLibrary = load_dynamic_libraryW(fullpathGuiSharedLibrary);
        if (nlsGuiHandleDynamicLibrary) {
            bFirstDynamicLibraryCall = false;
        }
    }
}
//=============================================================================
bool
HtmlFileToPdfFile(const std::wstring& htmlsrcfilename, const std::wstring& pdfdestfilename)
{
    using PROC_HtmlFileToPdfFile = bool (*)(std::wstring, std::wstring);
    static PROC_HtmlFileToPdfFile HtmlFileToPdfFilePtr = nullptr;
    initGuiDynamicLibrary();
    if (!HtmlFileToPdfFilePtr) {
        HtmlFileToPdfFilePtr = reinterpret_cast<PROC_HtmlFileToPdfFile>(
            get_function(nlsGuiHandleDynamicLibrary, "HtmlFileToPdfFile"));
        if (!HtmlFileToPdfFilePtr) {
            Error(_W("HtmlFileToPdfFile not loaded."));
        }
    }
    return HtmlFileToPdfFilePtr(htmlsrcfilename, pdfdestfilename);
}
//=============================================================================
bool
HtmlStreamToPdfFile(const std::wstring& htmlstream, const std::wstring& pdfdestfilename)
{
    using PROC_HtmlStreamToPdfFile = bool (*)(std::wstring, std::wstring);
    static PROC_HtmlStreamToPdfFile HtmlStreamToPdfFilePtr = nullptr;
    initGuiDynamicLibrary();
    if (!HtmlStreamToPdfFilePtr) {
        HtmlStreamToPdfFilePtr = reinterpret_cast<PROC_HtmlStreamToPdfFile>(
            get_function(nlsGuiHandleDynamicLibrary, "HtmlStreamToPdfFile"));
        if (!HtmlStreamToPdfFilePtr) {
            Error(_W("HtmlStreamToPdfFile not loaded."));
        }
    }
    return HtmlStreamToPdfFilePtr(htmlstream, pdfdestfilename);
}
//=============================================================================
}
//=============================================================================
