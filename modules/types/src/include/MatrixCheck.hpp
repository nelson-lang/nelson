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
#include "nlsTypes_exports.h"
//=============================================================================
namespace Nelson {
/**
 * Validate operands for a matrix product.
 *
 * Scalars are not handled here and make the function return false, so the
 * caller can use the vector-operation path. Non-scalar operands must be
 * numeric, share a compatible type, be two-dimensional, and have matching
 * inner dimensions.
 */
NLSTYPES_IMPEXP bool
MatrixCheck(const ArrayOf& A, const ArrayOf& B, const std::string& opname);
/**
 * Return the result class required for a binary numeric operation.
 *
 * Mixed single and double inputs promote to double. Integer inputs promote
 * to signed 64-bit integer unless the operation requires double arithmetic.
 * Mixed real and complex inputs promote to the matching complex class.
 * Character inputs are treated as 32-bit integer values.
 */
NLSTYPES_IMPEXP
NelsonType
FindCommonType(const ArrayOf& A, const ArrayOf& B);
/**
 * Require both operands to be numeric.
 */
NLSTYPES_IMPEXP void
CheckNumeric(const ArrayOf& A, const ArrayOf& B, const std::string& opname);
/**
 * Return whether two dimension objects describe the same vector length.
 */
NLSTYPES_IMPEXP bool
SameSizeCheck(Dimensions& Adim, Dimensions& Bdim);
/**
 * Validate and promote operands for element-wise numeric operations.
 *
 * Operands must be numeric. Their shapes must match unless either side is a
 * scalar.
 */
NLSTYPES_IMPEXP void
VectorCheck(ArrayOf& A, ArrayOf& B, const std::string& opname);
/**
 * Validate operands for element-wise operations on reference arrays.
 *
 * Operands must have the same reference class. Their shapes must match unless
 * either side is a scalar.
 */
NLSTYPES_IMPEXP void
VectorCheckReference(ArrayOf& A, ArrayOf& B, const std::string& opname);
/**
 * Convert operands to logical values and validate their element-wise shape.
 *
 * The shapes must match unless either side is a scalar.
 */
NLSTYPES_IMPEXP void
PromoteToLogicalVectorCheck(ArrayOf& A, ArrayOf& B, const std::string& opname);
//=============================================================================
} // namespace Nelson
//=============================================================================
