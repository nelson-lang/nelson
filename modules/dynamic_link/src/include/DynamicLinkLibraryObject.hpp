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
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "HandleGenericObject.hpp"
#include "nlsDynamic_link_exports.h"
#include <boost/dll/shared_library.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
#define DLLIB_CATEGORY_STR L"dllib"
//=============================================================================
class NLSDYNAMIC_LINK_IMPEXP DynamicLinkLibraryObject : public HandleGenericObject
{
public:
    DynamicLinkLibraryObject(std::wstring libraryPath);
    ~DynamicLinkLibraryObject();

    bool
    disp(Evaluator* eval);
    stringVector
    getAvailableSymbols();
    void*
    getFunctionPointer(std::string symbolName);
    bool
    get(std::wstring propertyName, ArrayOf& res);
    bool
    isWriteableProperty(std::wstring propertyName);
    wstringVector
    fieldnames();
    bool
    isProperty(std::wstring propertyName);
    bool
    isMethod(std::wstring methodName);

private:
    wstringVector _propertiesNames;
    boost::dll::shared_library _shared_library;
    std::wstring _libraryPath;
    bool
    searchLibrary(std::wstring libraryPath, std::wstring& fullLibraryPath);
    wstringVector
    getEnvironmentPaths(const std::wstring& environPath);
    bool
    findLibrary(
        wstringVector paths, const std::wstring& libraryName, std::wstring& fullLibraryPath);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
