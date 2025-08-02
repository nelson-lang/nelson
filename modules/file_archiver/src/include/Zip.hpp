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
#include "nlsFile_archiver_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSFILE_ARCHIVER_IMPEXP
void
Zip(const std::wstring& zipFilename, const wstringVector& names, const std::wstring& rootpath,
    wstringVector& filenames);
//=============================================================================
} // namespace Nelson
//=============================================================================
