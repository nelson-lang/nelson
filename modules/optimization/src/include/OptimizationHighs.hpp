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
#include "nlsOptimization_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson::OptimizationGateway {
//=============================================================================
NLSOPTIMIZATION_IMPEXP ArrayOfVector
intlinprogBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
NLSOPTIMIZATION_IMPEXP ArrayOfVector
linprogBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson::OptimizationGateway
//=============================================================================
