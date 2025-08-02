//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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

    wstringVector
    getProperties() override;
    wstringVector
    getMethods() override;

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
