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
#include "DynamicLinkLibraryObject.hpp"
#include "ClassName.hpp"
#include "characters_encoding.hpp"
#include "dynamic_library.hpp"
#include "Exception.hpp"
#include <boost/filesystem.hpp>
#include <boost/dll/library_info.hpp>
#include "dynamic_library.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    DynamicLinkLibraryObject::DynamicLinkLibraryObject(std::wstring libraryPath) : HandleGenericObject(std::wstring(DLLIB_CATEGORY_STR), this, false)
    {
        boost::system::error_code errorCode;
        boost::dll::shared_library lib(libraryPath, errorCode);
        if (errorCode)
        {
            throw Exception(_("Cannot load library: ") + errorCode.message());
        }
        boost::filesystem::path full_path = lib.location();
        _libraryPath = full_path.wstring();
        _shared_library = lib;
    }
    //=============================================================================
    DynamicLinkLibraryObject::~DynamicLinkLibraryObject()
    {
        _shared_library.unload();
        _libraryPath = L"";
    }
    //=============================================================================
    bool DynamicLinkLibraryObject::disp(Evaluator *eval)
    {
        if (eval != nullptr)
        {
            Interface *io = eval->getInterface();
            if (io)
            {
                std::wstring valueToDisp = L"";
                io->outputMessage(L"\n");
                io->outputMessage(L"\tPath: \t'" + _libraryPath + L"'\n");
                io->outputMessage(L"\n");
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    stringVector DynamicLinkLibraryObject::getAvailableSymbols()
    {
        stringVector symbols;
        boost::dll::library_info libinfo(_libraryPath, false);
        std::vector<std::string> stdSymbols = libinfo.symbols();
        symbols.reserve(stdSymbols.size());
        for (std::string s : stdSymbols)
        {
            symbols.push_back(s);
        }
        return symbols;
    }
    //=============================================================================
    void *DynamicLinkLibraryObject::getFunctionPointer(std::string symbolName)
    {
        return get_function(_shared_library.native(), symbolName);
    }
    //=============================================================================
}
//=============================================================================
