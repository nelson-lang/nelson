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
#include "nlsValidators_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
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
mustBeMatrix(const ArrayOf& arg, int argPosition, bool asCaller = false);
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
mustBeLessThanOrEqual(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNumericOrLogical(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNonzeroLengthText(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeMember(const ArrayOf& arg, const ArrayOf& S, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeInRange(const ArrayOf& value, const ArrayOf& lower, const ArrayOf& upper,
    const std::wstring& boundflag1, const std::wstring& boundflag2, int argPosition,
    bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeRow(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeColumn(const ArrayOf& arg, int argPosition, bool asCaller = false);
//=============================================================================
}
//=============================================================================
