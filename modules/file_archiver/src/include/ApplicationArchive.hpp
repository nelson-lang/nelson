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
#include "nlsFile_archiver_exports.h"
//=============================================================================
namespace Nelson {
NLSFILE_ARCHIVER_IMPEXP void
createApplicationDirectory(const std::wstring& directory);
NLSFILE_ARCHIVER_IMPEXP void
verifyApplicationArchive(const std::wstring& executable);
NLSFILE_ARCHIVER_IMPEXP void
extractApplicationArchive(const std::wstring& executable, const std::wstring& directory);
//=============================================================================
} // namespace Nelson
//=============================================================================
