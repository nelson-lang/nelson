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
 * We want to perform a matrix-matrix operation between two data objects.
 * The following checks are required:
 *  1. If A or B is a scalar, then return false - this is really a
 *     vector operation, and the arguments should be passed to a
 *     vector checker (like VectorCheck).
 *  2. Both A & B must be numeric
 *  3. Both A & B must be the same type.
 *  4. Both A & B must be 2-Dimensional.
 *  5. A & B must be conformant, i.e. the number of columns in A must
 *     match the number of rows in B.
 */
NLSTYPES_IMPEXP bool
MatrixCheck(const ArrayOf& A, const ArrayOf& B, const std::string& opname);

/**
 * Check that both of the argument objects are of the
 * same type.  If not, computes conversion.  Here are the
 * rules:
 *   Float operations produce float results.
 *   Double operations produce double results.
 *   Mixtures of float and double operations produce double results.
 *   Integer operations (of any kind) produce int results -
 *     the question is what type do these objects get promoted too?  The
 *     answer is a signed int (64 bits).
 *   Mixtures of real and complex operations produce complex results.
 *   Integer constants are 32 bit by default
 *   Float constants are 64 bit by default
 *   Division operations lead to promotion
 *   Character types are automatically (demoted) to 32 bit integers.
 *
 *   The way to accomplish this is as follows: compute the larger
 *   of the types of A and B.  Call this type t_max.  If t_max
 *   is not an integer type, promote both types to this type.
 *   If t_max is an integer type, promote both objects to an NLS_INT64.
 *   If this is a division operation or a matrix operation, promote both
 *    objects to an NLS_DOUBLE!
 *
 */
NLSTYPES_IMPEXP
NelsonType
FindCommonType(const ArrayOf& A, const ArrayOf& B);

/**
 * Check that both of the argument objects are numeric.
 */
NLSTYPES_IMPEXP void
CheckNumeric(const ArrayOf& A, const ArrayOf& B, const std::string& opname);

/*
 * Check to see if two dimensions (when treated as vectors) are equivalent in size.
 */
NLSTYPES_IMPEXP bool
SameSizeCheck(Dimensions& Adim, Dimensions& Bdim);

/**
 * We want to perform a vector operation between two data objects.
 * The following checks are required:
 *  1. Both A & B must be numeric
 *  2. Either A & B are the same size or
 *      A is a scalar or B is a scalar.
 */
NLSTYPES_IMPEXP void
VectorCheck(ArrayOf& A, ArrayOf& B, const std::string& opname);

/**
 * We want to perform a vector operation between two data objects.
 * The following checks are required:
 *  1. Both A & B must be same reference type
 *  2. Either A & B are the same size or
 *      A is a scalar or B is a scalar.
 */
NLSTYPES_IMPEXP void
VectorCheckReference(ArrayOf& A, ArrayOf& B, const std::string& opname);

/**
 * We want to perform a vector operator between two logical data objects.
 * The following operations are performed:
 *  1. Both A & B are converted to logical types.
 *  2. Either A & B must be the same size, or A is a
 *     scalar or B is a scalar.
 */
NLSTYPES_IMPEXP void
PromoteToLogicalVectorCheck(ArrayOf& A, ArrayOf& B, const std::string& opname);
} // namespace Nelson
//=============================================================================
