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
#include "ArrayOf.hpp"
#include "nlsElementary_functions_exports.h"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSELEMENTARY_FUNCTIONS_IMPEXP bool
IsEqual(Evaluator* eval, const ArrayOfVector& args, NelsonType commonType, bool isSparse,
    bool isComplex, bool& needToOverload);
//=============================================================================
} // namespace Nelson
//=============================================================================
