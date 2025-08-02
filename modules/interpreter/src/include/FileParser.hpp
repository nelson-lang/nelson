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
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
NLSINTERPRETER_IMPEXP void
setParserFilename(const std::string& filename);
NLSINTERPRETER_IMPEXP void
setParserFilename(const std::wstring& filename);
NLSINTERPRETER_IMPEXP std::string
getParserFilenameU();
NLSINTERPRETER_IMPEXP std::wstring
getParserFilenameW();
} // namespace Nelson
//=============================================================================
