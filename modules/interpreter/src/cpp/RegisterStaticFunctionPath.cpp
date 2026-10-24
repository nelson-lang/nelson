//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RegisterStaticFunctionPath.hpp"
#include "FileSystemWrapper.hpp"
#include "NelsonConfiguration.hpp"
#include "PathFunctionIndexerManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
RegisterStaticFunctionPath(const wchar_t* moduleName, const wchar_t* relativePath)
{
    if (moduleName == nullptr || relativePath == nullptr) {
        return false;
    }
    FileSystemWrapper::Path path = NelsonConfiguration::getInstance()->getNelsonModulesDirectory();
    path /= moduleName;
    path /= relativePath;
    if (!path.is_directory()) {
        return false;
    }
    return PathFunctionIndexerManager::getInstance()->addPath(path.generic_wstring(), false, true);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
