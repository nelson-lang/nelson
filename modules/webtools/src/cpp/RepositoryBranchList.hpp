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
#include "Types.hpp"
#include "nlsWebtools_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSWEBTOOLS_IMPEXP wstringVector
RepositoryBranchList(const std::wstring& localPath, std::wstring& errorMessage);
//=============================================================================
NLSWEBTOOLS_IMPEXP std::wstring
RepositoryGetCurrentBranchName(const std::wstring& localPath, std::wstring& errorMessage);
//=============================================================================

};
//=============================================================================
