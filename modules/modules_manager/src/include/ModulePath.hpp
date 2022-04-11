//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "nlsModules_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum MODULEPATH_OPTION
{
    GET_BINARY_PATH,
    GET_ROOT_PATH,
    GET_ETC_PATH,
    GET_DYNLIB_FULLPATH,
    GET_SCRIPT_PATH
};
//=============================================================================
NLSMODULES_MANAGER_IMPEXP std::wstring
ModulePath(const std::wstring& moduleshortname);
NLSMODULES_MANAGER_IMPEXP std::wstring
ModulePath(const std::wstring& modulerootpath, const std::wstring& moduleshortname,
    MODULEPATH_OPTION option);
} // namespace Nelson
//=============================================================================
