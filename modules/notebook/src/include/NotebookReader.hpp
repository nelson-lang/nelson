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
#include "nlsNotebook_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// True when the filename has the Notebook native extension (.nlx).
NLSNOTEBOOK_IMPEXP bool
isNotebookFile(const std::wstring& filename);
//=============================================================================
// Read a Notebook document and return the code of each code cell, in
// document order. Supports the native .nlx container (a zip holding
// document.json) and the plain-text .m Notebook format (its `%% [code]`
// sections, or the whole file when it is an ordinary script). Throws on a
// read/parse failure.
NLSNOTEBOOK_IMPEXP wstringVector
ReadNotebookCode(const std::wstring& filename);
//=============================================================================
} // namespace Nelson
//=============================================================================
