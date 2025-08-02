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
#include <string>
#include "nlsFiles_folders_functions_exports.h"
//=============================================================================
#undef RemoveDirectory
//=============================================================================
namespace Nelson {
NLSFILES_FOLDERS_FUNCTIONS_IMPEXP bool
RemoveDirectory(const std::wstring& folderName, bool bSubfolder, std::wstring& message);
}
//=============================================================================
