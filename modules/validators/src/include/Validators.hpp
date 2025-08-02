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
#include "nlsValidators_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeLogical(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeLogicalScalar(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeFinite(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNonempty(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeScalarOrEmpty(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeValidVariableName(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeTextScalar(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeText(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeFolder(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeFile(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeVector(const ArrayOfVector& args, bool allowsAllEmpties, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeFloat(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNumeric(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeA(const ArrayOfVector& args, const wstringVector& classNames, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBePositive(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNonpositive(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNonnegative(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNegative(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNonNan(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNonZero(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNonSparse(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeReal(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeInteger(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNonmissing(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeGreaterThan(const ArrayOfVector& args, const ArrayOf& c, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeLessThan(const ArrayOfVector& args, const ArrayOf& c, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeGreaterThanOrEqual(const ArrayOfVector& args, const ArrayOf& c, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeLessThanOrEqual(const ArrayOfVector& args, const ArrayOf& c, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNumericOrLogical(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeNonzeroLengthText(const ArrayOfVector& args, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeMember(const ArrayOfVector& args, const ArrayOf& S, int argPosition);
//=============================================================================
NLSVALIDATORS_IMPEXP void
mustBeInRange(const ArrayOfVector& args, const ArrayOf& upper, const ArrayOf& lower,
    const std::wstring& boundflag1, const std::wstring& boundflag2, int argPosition);
//=============================================================================

}
//=============================================================================
