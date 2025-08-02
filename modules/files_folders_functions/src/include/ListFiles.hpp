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
#include "FileInfo.hpp"
#include "nlsFiles_folders_functions_exports.h"
#include <vector>
#include <string>
//=============================================================================
namespace Nelson {
NLSFILES_FOLDERS_FUNCTIONS_IMPEXP std::vector<FileInfo>
ListFiles(const std::wstring& directory, bool bSubdirectories = false);
}
//=============================================================================
