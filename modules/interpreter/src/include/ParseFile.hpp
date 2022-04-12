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
#include <string>
#include "Evaluator.hpp"
#include "ParserState.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
NLSINTERPRETER_IMPEXP ParserState
ParseFile(Evaluator* eval, const std::wstring& filename, bool bIgnoreException = true);
}
//=============================================================================
