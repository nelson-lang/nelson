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
#include "Evaluator.hpp"
#include "nlsCore_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
NLSCORE_IMPEXP std::wstring
GetCurrentNFilenameW(Evaluator* eval);
NLSCORE_IMPEXP std::string
GetCurrentNFilenameU(Evaluator* eval);
} // namespace Nelson
//=============================================================================
