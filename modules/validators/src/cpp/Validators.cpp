//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Validators.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ValidatorsInternal.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
checkArgumentPosition(const ArrayOfVector& args, int argPosition)
{
    if (argPosition < 0 || argPosition >= args.size()) {
        std::wstring msg = _W("Invalid input argument position.");
        std::wstring id = L"Nelson:validators:invalidInputPosition";
        Error(msg, id);
    }
}
//=============================================================================
void
mustBeLogical(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeLogical(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeLogicalScalar(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeLogicalScalar(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeFinite(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeFinite(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeNonempty(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeNonempty(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeScalarOrEmpty(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeScalarOrEmpty(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeValidVariableName(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeValidVariableName(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeTextScalar(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeTextScalar(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeText(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeText(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeVector(const ArrayOfVector& args, bool allowsAllEmpties, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeVector(args[argPosition], allowsAllEmpties, argPosition + 1);
}
//=============================================================================
void
mustBeFloat(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeFloat(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeNumeric(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeNumeric(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeFolder(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeFolder(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeFile(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeFile(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeA(const ArrayOfVector& args, const wstringVector& classNames, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeA(args[argPosition], classNames, argPosition + 1);
}
//=============================================================================
void
mustBePositive(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBePositive(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeNonpositive(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeNonpositive(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeNonnegative(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeNonnegative(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeNegative(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeNegative(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeNonNan(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeNonNan(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeNonZero(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeNonZero(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeNonSparse(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeNonSparse(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeReal(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeReal(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeInteger(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeInteger(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeNonmissing(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeNonmissing(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeGreaterThan(const ArrayOfVector& args, const ArrayOf& c, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeGreaterThan(args[argPosition], c, argPosition + 1);
}
//=============================================================================
void
mustBeLessThan(const ArrayOfVector& args, const ArrayOf& c, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeLessThan(args[argPosition], c, argPosition + 1);
}
//=============================================================================
void
mustBeGreaterThanOrEqual(const ArrayOfVector& args, const ArrayOf& c, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeGreaterThanOrEqual(args[argPosition], c, argPosition + 1);
}
//=============================================================================
void
mustBeLessThanOrEqual(const ArrayOfVector& args, const ArrayOf& c, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeLessThanOrEqual(args[argPosition], c, argPosition + 1);
}
//=============================================================================
void
mustBeNumericOrLogical(const ArrayOfVector& args, const ArrayOf& c, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeNumericOrLogical(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeNonzeroLengthText(const ArrayOfVector& args, const ArrayOf& c, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeNonzeroLengthText(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeMember(const ArrayOfVector& args, const ArrayOf& S, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeMember(args[argPosition], S, argPosition + 1);
}
//=============================================================================
void
mustBeInRange(const ArrayOfVector& args, const ArrayOf& upper, const ArrayOf& lower,
    const std::wstring& boundflag1, const std::wstring& boundflag2, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeInRange(args[argPosition], upper, lower, boundflag1, boundflag2, argPosition + 1);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
