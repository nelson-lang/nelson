//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <boost/dll/shared_library.hpp>
#include "ArrayOf.hpp"
#include "Interface.hpp"
#include "HandleGenericObject.hpp"
#include "nlsDynamic_link_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
#define DLLIB_CATEGORY_STR L"dllib"
//=============================================================================
class NLSDYNAMIC_LINK_IMPEXP DynamicLinkLibraryObject : public HandleGenericObject
{
public:
    DynamicLinkLibraryObject(const std::wstring& libraryPath);
    ~DynamicLinkLibraryObject() override;

    bool
    disp(Interface* io);
    stringVector
    getAvailableSymbols(std::string& errorMessage);
    void*
    getFunctionPointer(const std::string& symbolName);
    bool
    get(const std::wstring& propertyName, ArrayOf& res);
    bool
    isWriteableProperty(const std::wstring& propertyName);
    wstringVector
    fieldnames();
    bool
    isProperty(const std::wstring& propertyName) override;
    bool
    isMethod(const std::wstring& methodName) override;
    std::wstring
    getLibraryPath();

private:
    wstringVector _propertiesNames;
    boost::dll::shared_library _shared_library;
    std::wstring _libraryPath;
    bool
    searchLibrary(const std::wstring& libraryPath, std::wstring& fullLibraryPath);
    wstringVector
    getEnvironmentPaths(const std::wstring& environPath);
    bool
    findLibrary(
        const wstringVector& paths, const std::wstring& libraryName, std::wstring& fullLibraryPath);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
