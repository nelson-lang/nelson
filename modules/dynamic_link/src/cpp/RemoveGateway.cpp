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
#include "RemoveGateway.hpp"
#include "Error.hpp"
#include "FindDynamicLibraryName.hpp"
#include "characters_encoding.hpp"
#include "dynamic_library.hpp"
#include <boost/filesystem.hpp>
//=============================================================================
namespace Nelson {
void
RemoveGateway(Evaluator* eval, std::wstring dynlibname)
{
    /* to simplify some dependencies resolution, we move in the directory and restore it after */
    boost::filesystem::path p;
    p = dynlibname;
    p = p.generic_wstring();
    std::wstring dirname;
    std::wstring filename;
    boost::filesystem::path dir = p.parent_path();
    if (dir.generic_wstring().compare(L"") == 0) {
        dir = boost::filesystem::current_path();
    }
    dirname = dir.generic_wstring();
    filename = p.filename().generic_wstring();
    filename = FindDynamicLibraryName(dirname, filename, false);
    if (filename.empty()) {
        Error(_W("File not found."));
    }
    boost::filesystem::path currentdirbackup = boost::filesystem::current_path();
    boost::filesystem::current_path(dir);
    library_handle nlsModuleHandleDynamicLibrary = nullptr;
#ifdef _MSC_VER
    nlsModuleHandleDynamicLibrary = load_dynamic_libraryW(filename);
#else
    nlsModuleHandleDynamicLibrary = load_dynamic_library(wstring_to_utf8(filename));
#endif
    if (nlsModuleHandleDynamicLibrary) {
        typedef bool (*PROC_RemoveGateway)(void* eval, const wchar_t* moduleFilename);
        PROC_RemoveGateway RemoveGatewayPtr = reinterpret_cast<PROC_RemoveGateway>(
            get_function(nlsModuleHandleDynamicLibrary, REMOVEGATEWAY_ENTRY));
        boost::filesystem::current_path(currentdirbackup);
        if (!RemoveGatewayPtr) {
            Error(_W("Module not loaded: symbol not found."));
        }
        RemoveGatewayPtr((void*)eval, dynlibname.c_str());
    } else {
        boost::filesystem::current_path(currentdirbackup);
        Error(_W("Module not loaded: library not loaded."));
    }
}
//=============================================================================
}
//=============================================================================
