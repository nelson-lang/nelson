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
#include "HtmlToPdf.hpp"
#include "Error.hpp"
#include "dynamic_library.hpp"
#include <boost/function.hpp>
//=============================================================================
namespace Nelson {
static library_handle nlsGuiHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
//=============================================================================
static void
initGuiDynamicLibrary(void)
{
    if (bFirstDynamicLibraryCall) {
        std::string fullpathGuiSharedLibrary = "libnlsGui" + get_dynamic_library_extension();
#ifdef _MSC_VER
        char* buf;
        try {
            buf = new char[MAX_PATH];
        } catch (const std::bad_alloc&) {
            buf = nullptr;
        }
        if (buf) {
            DWORD dwRet = ::GetEnvironmentVariableA("NELSON_BINARY_PATH", buf, MAX_PATH);
            if (dwRet) {
                fullpathGuiSharedLibrary
                    = std::string(buf) + std::string("/") + fullpathGuiSharedLibrary;
            }
            delete[] buf;
        }
#else
        char const* tmp = std::getenv("NELSON_BINARY_PATH");
        if (tmp != nullptr) {
            fullpathGuiSharedLibrary
                = std::string(tmp) + std::string("/") + fullpathGuiSharedLibrary;
        }
#endif
        nlsGuiHandleDynamicLibrary = load_dynamic_library(fullpathGuiSharedLibrary);
        if (nlsGuiHandleDynamicLibrary) {
            bFirstDynamicLibraryCall = false;
        }
    }
}
//=============================================================================
bool
HtmlFileToPdfFile(std::wstring htmlsrcfilename, std::wstring pdfdestfilename)
{
    typedef bool (*PROC_HtmlFileToPdfFile)(
        std::wstring htmlsrcfilename, std::wstring pdfdestfilename);
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
HtmlStreamToPdfFile(std::wstring htmlstream, std::wstring pdfdestfilename)
{
    typedef bool (*PROC_HtmlStreamToPdfFile)(std::wstring htmlstream, std::wstring pdfdestfilename);
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
