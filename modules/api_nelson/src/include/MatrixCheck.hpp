//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "nlsapi_nelson_exports.h"
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
NLSAPI_NELSON_IMPEXP bool
MatrixCheck(ArrayOf A, ArrayOf B, const std::string& opname);

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
NLSAPI_NELSON_IMPEXP Class
FindCommonType(const ArrayOf& A, const ArrayOf& B, bool isDivOrMatrix);

/**
 * Check that both of the argument objects are numeric.
 */
NLSAPI_NELSON_IMPEXP void
CheckNumeric(ArrayOf A, ArrayOf B, const std::string& opname);

/*
 * Check to see if two dimensions (when treated as vectors) are equivalent in size.
 */
NLSAPI_NELSON_IMPEXP bool
SameSizeCheck(Dimensions& Adim, Dimensions& Bdim);

/**
 * We want to perform a vector operation between two data objects.
 * The following checks are required:
 *  1. Both A & B must be numeric
 *  2. Either A & B are the same size or
 *      A is a scalar or B is a scalar.
 */
NLSAPI_NELSON_IMPEXP void
VectorCheck(ArrayOf& A, ArrayOf& B, const std::string& opname);

/**
 * We want to perform a vector operation between two data objects.
 * The following checks are required:
 *  1. Both A & B must be same reference type
 *  2. Either A & B are the same size or
 *      A is a scalar or B is a scalar.
 */
NLSAPI_NELSON_IMPEXP void
VectorCheckReference(ArrayOf& A, ArrayOf& B, const std::string& opname);

/**
 * We want to perform a vector operator between two logical data objects.
 * The following operations are performed:
 *  1. Both A & B are converted to logical types.
 *  2. Either A & B must be the same size, or A is a
 *     scalar or B is a scalar.
 */
NLSAPI_NELSON_IMPEXP void
BoolVectorCheck(ArrayOf& A, ArrayOf& B, const std::string& opname);
} // namespace Nelson
//=============================================================================
