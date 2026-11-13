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
#include "ExtractionGuard.hpp"
#include "nlsFile_archiver_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSFILE_ARCHIVER_IMPEXP
void
UnZip(const std::wstring& zipFilename, const std::wstring& rootpath, wstringVector& entriesnames);
// Same, refusing archives that pass the limits before anything is written.
NLSFILE_ARCHIVER_IMPEXP
void
UnZip(const std::wstring& zipFilename, const std::wstring& rootpath, wstringVector& entriesnames,
    const ExtractionLimits& limits);
//=============================================================================
} // namespace Nelson
//=============================================================================
