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
#include "nlsSpreadsheet_builtin_exports.h"
#include "Evaluator.hpp"
#include "ArrayOfVector.hpp"
//=============================================================================
namespace Nelson::SpreadsheetGateway {
//=============================================================================
NLSSPREADSHEET_BUILTIN_IMPEXP ArrayOfVector
writetimetableBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson::SpreadsheetGateway
//=============================================================================
