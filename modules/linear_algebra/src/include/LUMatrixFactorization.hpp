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
#include "nlsLinear_algebra_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP ArrayOfVector
LUMatrixFactorizationDoubleReal(const ArrayOf& A, int nLhs);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP ArrayOfVector
LUMatrixFactorizationDoubleComplex(const ArrayOf& A, int nLhs);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP ArrayOfVector
LUMatrixFactorizationSingleReal(const ArrayOf& A, int nLhs);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP ArrayOfVector
LUMatrixFactorizationSingleComplex(const ArrayOf& A, int nLhs);
//=============================================================================
}
//=============================================================================
