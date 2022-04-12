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
#include "ArrayOf.hpp"
#include "nlsOperators_exports.h"
//=============================================================================
namespace Nelson {
NLSOPERATORS_IMPEXP ArrayOf
binaryOperatorEmptyMatrixEmptryMatrix(
    const ArrayOf& A, const ArrayOf& B, NelsonType commonClass, const std::string& operatorName);
//=============================================================================
} // namespace Nelson
//=============================================================================
