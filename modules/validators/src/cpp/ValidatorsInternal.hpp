//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsValidators_exports.h"
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSVALIDATORS_IMPEXP void
setEvaluator(Evaluator* eval);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeLogical(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeLogicalScalar(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeFinite(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNonempty(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeScalarOrEmpty(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeValidVariableName(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeTextScalar(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeText(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeFolder(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeFile(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeVector(const ArrayOf& arg, bool allowsAllEmpties, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeFloat(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNumeric(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeA(
    const ArrayOf& arg, const wstringVector& classNames, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBePositive(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNonpositive(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNonnegative(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNegative(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNonNan(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNonZero(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNonSparse(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeReal(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeInteger(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNonmissing(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeGreaterThan(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeLessThan(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeGreaterThanOrEqual(
    const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeLessThanOrEqual(
    const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNumericOrLogical(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
}
//=============================================================================
