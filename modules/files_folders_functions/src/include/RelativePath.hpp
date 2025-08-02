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
#include "nlsFiles_folders_functions_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
NLSFILES_FOLDERS_FUNCTIONS_IMPEXP std::wstring
RelativePath(const std::wstring& path1, const std::wstring& path2, bool& bSuccess);
}
//=============================================================================
