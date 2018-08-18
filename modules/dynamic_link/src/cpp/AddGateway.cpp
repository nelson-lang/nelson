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
#include "AddGateway.hpp"
#include "Error.hpp"
#include "FindDynamicLibraryName.hpp"
#include "characters_encoding.hpp"
#include "dynamic_library.hpp"
#include <boost/filesystem.hpp>
//=============================================================================
namespace Nelson {
void
AddGateway(Evaluator* eval, std::wstring dynlibname)
{
    /* to simplify some dependencies resolution, we move in the directory and restore it after */
    boost::filesystem::path p = dynlibname;
    p = p.generic_wstring();
    std::wstring filename;
    boost::filesystem::path dir = p.parent_path();
    if (dir.generic_wstring().compare(L"") == 0) {
        dir = boost::filesystem::current_path();
    }
    const std::wstring dirname = dir.generic_wstring();
    filename = p.filename().generic_wstring();
    filename = FindDynamicLibraryName(dirname, filename, false);
    if (filename.empty()) {
        Error(_W("File not found."));
    } else {
        boost::filesystem::path currentdirbackup = boost::filesystem::current_path();
        boost::filesystem::current_path(dir);
        library_handle nlsModuleHandleDynamicLibrary = nullptr;
#ifdef _MSC_VER
        nlsModuleHandleDynamicLibrary = load_dynamic_libraryW(filename);
#else
        nlsModuleHandleDynamicLibrary = load_dynamic_library(wstring_to_utf8(filename));
#endif
        if (nlsModuleHandleDynamicLibrary) {
            typedef bool (*PROC_AddGateway)(const void* eval, const wchar_t* moduleFilename);
            PROC_AddGateway AddGatewayPtr = reinterpret_cast<PROC_AddGateway>(
                get_function(nlsModuleHandleDynamicLibrary, GATEWAY_ENTRY));
            boost::filesystem::current_path(currentdirbackup);
            if (!AddGatewayPtr) {
                Error(_W("Module not loaded: symbol not found."));
            }
            AddGatewayPtr((void*)eval, dynlibname.c_str());
        } else {
            std::string error_msg = get_dynamic_library_error();
            boost::filesystem::current_path(currentdirbackup);
            Error(_W("Module not loaded: library not loaded.\n") + dynlibname + L"\n"
                + utf8_to_wstring(error_msg) + L"\n");
        }
    }
}
//=============================================================================
}
//=============================================================================
