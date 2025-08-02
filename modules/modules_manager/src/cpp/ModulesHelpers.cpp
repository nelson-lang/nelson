//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cwctype>
#include "ModulesHelpers.hpp"
#include "StringHelpers.hpp"
#include "DynamicLibrary.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
ConstructDynamicLibraryName(const std::wstring& moduleshortname)
{
    std::wstring name = moduleshortname;
    name[0] = towupper(name[0]);
    std::wstring libname = std::wstring(L"libnls") + name + std::wstring(L"_builtin");
    return libname + get_dynamic_library_extensionW();
}
//=============================================================================
std::wstring
ConstructDynamicLibraryFullname(
    const std::wstring& moduleRootPath, const std::wstring& moduleshortname, bool isInternalModule)
{
    if (isInternalModule) {
        return moduleRootPath + L"/" + ConstructDynamicLibraryName(moduleshortname);
    }
    return moduleRootPath + L"/builtin/" + ConstructDynamicLibraryName(moduleshortname);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
