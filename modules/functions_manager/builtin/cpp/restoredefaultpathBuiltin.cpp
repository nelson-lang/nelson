//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "restoredefaultpathBuiltin.hpp"
#include "Error.hpp"
#include "ModulesManager.hpp"
#include "PathFuncManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::restoredefaultpathBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 0, 0);
    PathFuncManager::getInstance()->clearUserPath();
    PathFuncManager::getInstance()->resetUserPath();
    PathFuncManager::getInstance()->clear();
    wstringVector paths = ModulesManager::Instance().getModulesPathList(false);
    for (const auto& path : paths) {
        std::wstring _path = path + L"/functions";
        Nelson::FileSystemWrapper::Path data_dir(_path);
        bool bRes = false;
        try {
            bRes = Nelson::FileSystemWrapper::Path::is_directory(data_dir);
        } catch (const boost::filesystem::filesystem_error& e) {
            if (e.code() == boost::system::errc::permission_denied) {
                // ONLY FOR DEBUG
            }
            bRes = false;
        }
        if (bRes) {
            PathFuncManager::getInstance()->addPath(_path, true, false);
        }
    }
    return retval;
}
//=============================================================================
