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
#include "nlsLinear_algebra_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP bool
IsHermitianWithSkew(const ArrayOf& A, bool& needToOverload);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP bool
IsHermitianWithoutSkew(const ArrayOf& A, bool& needToOverload);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP bool
IsHermitian(const ArrayOf& A, double tol, bool& needToOverload);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP bool
IsHermitian(const ArrayOf& A, bool skew, const std::string& functionName);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP bool
IsHermitian(const ArrayOf& A, double tol, const std::string& functionName);
//=============================================================================
} // namespace Nelson
//=============================================================================
