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
#include "GetVariableEnvironment.hpp"
#undef GetCurrentDirectory
#include "GetCurrentDirectory.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    DynamicLinkLibraryObject::DynamicLinkLibraryObject(std::wstring libraryPath) : HandleGenericObject(std::wstring(DLLIB_CATEGORY_STR), this, false)
    {
        _propertiesNames = { L"Path"
                           };

		std::wstring fullLibraryPath = L"";
		if (searchLibrary(libraryPath, fullLibraryPath))
		{
			boost::system::error_code errorCode;
			boost::dll::shared_library lib(fullLibraryPath, errorCode);
			if (errorCode)
			{
				throw Exception(_("Cannot load library: ") + errorCode.message());
			}
			boost::filesystem::path full_path = lib.location();
			_libraryPath = full_path.generic_wstring();
			_shared_library = lib;
		}
		else
		{
			throw Exception(_W("Cannot load library: ") + libraryPath);
		}
    }
    //=============================================================================
    DynamicLinkLibraryObject::~DynamicLinkLibraryObject()
    {
        _propertiesNames.clear();
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
    bool DynamicLinkLibraryObject::get(std::wstring propertyName, ArrayOf &res)
    {
        if (propertyName == L"Path")
        {
            res = ArrayOf::stringConstructor(_libraryPath);
            return true;
        }
        return false;
    }
    //=========================================================================
    bool DynamicLinkLibraryObject::isWriteableProperty(std::wstring propertyName)
    {
        return false;
    }
    //=============================================================================
    wstringVector DynamicLinkLibraryObject::fieldnames()
    {
        return _propertiesNames;
    }
    //=============================================================================
    bool DynamicLinkLibraryObject::isproperty(std::wstring propertyName)
    {
        auto it = std::find(_propertiesNames.begin(), _propertiesNames.end(), propertyName);
        return (it != _propertiesNames.end());
    }
    //=============================================================================
	bool DynamicLinkLibraryObject::searchLibrary(std::wstring libraryPath, std::wstring &fullLibraryPath)
	{
		fullLibraryPath = L"";
		boost::filesystem::path pathToSplit = libraryPath;
		std::wstring parentPath = L"";
		if (pathToSplit.has_parent_path())
		{
			parentPath = pathToSplit.parent_path().generic_wstring();
		}
		
		boost::system::error_code errorCode;
		if (parentPath != L"")
		{
			boost::dll::shared_library lib(libraryPath, errorCode);
			if (errorCode)
			{
				return false;
			}
			fullLibraryPath = libraryPath;
			return true;
		}
		if (parentPath == L"")
		{
			std::wstring currentPath = Nelson::GetCurrentDirectory();
			boost::filesystem::path dir(currentPath);
			boost::filesystem::path file(libraryPath);
			boost::filesystem::path full_path = dir / file;
			boost::dll::shared_library lib(full_path, errorCode);
			if (errorCode)
			{
				wstringVector paths = getEnvironmentPaths(L"NLS_LIBRARY_PATH");
				if (!paths.empty())
				{
					for (std::wstring path : paths)
					{
						boost::filesystem::path dir(path);
						boost::filesystem::path file(libraryPath);
						boost::filesystem::path full_path = dir / file;
						boost::dll::shared_library lib(full_path, errorCode);
						if (!errorCode)
						{
							fullLibraryPath = full_path.generic_wstring();
							return true;
						}
					}
				}
#ifdef _MSC_VER
				paths = getEnvironmentPaths(L"PATH");
#else
#ifdef __APPLE__
				paths = getEnvironmentPaths(L"DYLD_LIBRARY_PATH");
#else
				paths = getEnvironmentPaths(L"LD_LIBRARY_PATH");
#endif
#endif
				if (!paths.empty())
				{
					for (std::wstring path : paths)
					{
						boost::filesystem::path dir(path);
						boost::filesystem::path file(libraryPath);
						boost::filesystem::path full_path = dir / file;
						boost::dll::shared_library lib(full_path.generic_wstring(), errorCode);
						if (errorCode)
						{
						}
						else
						{
							fullLibraryPath = full_path.generic_wstring();
							return true;
						}
					}
				}
#ifndef _MSC_VER
				paths.clear();
				paths.push_back(L"/usr/lib");
				paths.push_back(L"/usr/local/lib");
				if (!paths.empty())
				{
					for (std::wstring path : paths)
					{
						boost::filesystem::path dir(path);
						boost::filesystem::path file(libraryPath);
						boost::filesystem::path full_path = dir / file;
						boost::dll::shared_library lib(full_path.generic_wstring(), errorCode);
						if (errorCode)
						{
						}
						else
						{
							fullLibraryPath = full_path.generic_wstring();
							return true;
						}
					}
				}
#endif

			}
			else
			{
				fullLibraryPath = full_path.generic_wstring();
				return true;
			}
		}
		return false;
	}
	//=============================================================================
	wstringVector DynamicLinkLibraryObject::getEnvironmentPaths(const std::wstring &environPath)
	{
		wstringVector  result;
		std::wstring Path = GetVariableEnvironment(environPath);

#if _MSC_VER
		const wchar_t delimiter = L';';
#else
		const wchar_t delimiter = L':';
#endif
		if (Path.empty())
		{
			return result;
		}
		size_t previous = 0;
		size_t index = Path.find(delimiter);
		while (index != std::wstring::npos)
		{
			result.push_back(Path.substr(previous, index - previous));
			previous = index + 1;
			index = Path.find(delimiter, previous);
		}
		std::wstring p = Path.substr(previous);
		if (!p.empty())
		{
			result.push_back(p);
		}
		return result;
	}
}
//=============================================================================
