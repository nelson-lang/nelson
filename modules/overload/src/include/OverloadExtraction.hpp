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
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "nlsOverload_exports.h"
//=============================================================================
namespace Nelson {
NLSOVERLOAD_IMPEXP ArrayOfVector
OverloadExtraction(Evaluator* eval, const std::string& ClassName, ArrayOfVector args);
}
//=============================================================================
