//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RegisterStaticModule.hpp"
#include "FileSystemWrapper.hpp"
#include "ModulesManager.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
RegisterStaticModule(const wchar_t* moduleName)
{
    if (moduleName == nullptr) {
        return false;
    }
    const std::wstring name(moduleName);
    if (IsExistingModuleName(name)) {
        return true;
    }
    FileSystemWrapper::Path path = NelsonConfiguration::getInstance()->getNelsonModulesDirectory();
    path /= name;
#if !defined(__EMSCRIPTEN__) && !defined(NELSON_STATIC_GATEWAYS)
    if (!path.is_directory()) {
        return false;
    }
#endif
    return RegisterModule(name, path.generic_wstring(), true);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
