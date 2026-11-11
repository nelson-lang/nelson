//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <array>
#include <cmath>
#include <cwctype>
#include "isapproxBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "IsApprox.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
isTextScalar(const ArrayOf& value)
{
    return (value.isRowVectorCharacterArray() || value.isScalarStringArray()) && !value.isEmpty();
}
//=============================================================================
// true when text is a non-empty case-insensitive prefix of name
static bool
isPrefixOf(const std::wstring& text, const wchar_t* name)
{
    std::wstring full(name);
    if (text.empty() || text.size() > full.size()) {
        return false;
    }
    for (size_t k = 0; k < text.size(); ++k) {
        if (std::towlower(text[k]) != std::towlower(full[k])) {
            return false;
        }
    }
    return true;
}
//=============================================================================
// unique case-insensitive prefix of a level name
static bool
matchLevel(const ArrayOf& value, IsApproxLevel& level)
{
    if (!isTextScalar(value)) {
        return false;
    }
    std::wstring text = value.asWideString();
    if (text.empty()) {
        return false;
    }
    const std::array<std::pair<const wchar_t*, IsApproxLevel>, 4> levels = { {
        { L"verytight", IsApproxLevel::VeryTight },
        { L"tight", IsApproxLevel::Tight },
        { L"loose", IsApproxLevel::Loose },
        { L"veryloose", IsApproxLevel::VeryLoose },
    } };
    int found = 0;
    for (const auto& candidate : levels) {
        if (isPrefixOf(text, candidate.first)) {
            level = candidate.second;
            found++;
        }
    }
    return found == 1;
}
//=============================================================================
static ArrayOf
checkedTolerance(const ArrayOf& value, bool relative)
{
    bool valid
        = (value.dataClass() == NLS_DOUBLE || value.dataClass() == NLS_SINGLE) && !value.isEmpty();
    if (valid) {
        ArrayOf asDouble(value);
        if (asDouble.isSparse()) {
            asDouble.makeDense();
        }
        asDouble.promoteType(NLS_DOUBLE);
        const auto* p = static_cast<const double*>(asDouble.dataPointer());
        for (indexType k = 0; k < asDouble.elementCount() && valid; ++k) {
            valid = std::isfinite(p[k]) && p[k] >= 0 && (!relative || p[k] < 1);
        }
        if (valid) {
            return asDouble;
        }
    }
    if (relative) {
        Error(L"Nelson:isapprox:invalidRelativeTolerance",
            L"RelativeTolerance must be a nonnegative finite real double or single value smaller "
            L"than 1.");
    }
    Error(L"Nelson:isapprox:invalidAbsoluteTolerance",
        L"AbsoluteTolerance must be a nonnegative finite real double or single value.");
    return {};
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::isapproxBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 1);
    for (size_t k = 0; k < 2; ++k) {
        if (argIn[k].isCell() || argIn[k].isHandle() || argIn[k].isStruct()
            || argIn[k].isClassType()) {
            OverloadRequired("isapprox");
        }
    }
    const ArrayOf& A = argIn[0];
    const ArrayOf& B = argIn[1];
    if (!A.isNumeric() || !B.isNumeric()) {
        Error(L"Nelson:isapprox:numericInputsExpected",
            L"The first two inputs must be numeric arrays.");
    }
    IsApproxCheckIntegerRange(A, B);
    bool singlePrecision = A.dataClass() == NLS_SINGLE || A.dataClass() == NLS_SCOMPLEX
        || B.dataClass() == NLS_SINGLE || B.dataClass() == NLS_SCOMPLEX;
    IsApproxLevel level = IsApproxLevel::VeryTight;
    size_t nOptions = argIn.size() - 2;
    if (nOptions == 1) {
        if (!matchLevel(argIn[2], level)) {
            Error(L"Nelson:isapprox:invalidToleranceLevel",
                L"Tolerance level expected: 'verytight', 'tight', 'loose' or 'veryloose'.");
        }
    }
    if (nOptions % 2 == 1 && nOptions > 1) {
        if (matchLevel(argIn[2], level)) {
            Error(L"Nelson:isapprox:levelWithTolerances",
                L"A tolerance level cannot be combined with AbsoluteTolerance or "
                L"RelativeTolerance.");
        }
        Error(L"Nelson:isapprox:missingParameterValue",
            L"Each parameter name must be followed by its value.");
    }
    double defaultTolerance = IsApproxLevelTolerance(level, singlePrecision);
    ArrayOf absoluteTolerance = ArrayOf::doubleScalar(defaultTolerance);
    ArrayOf relativeTolerance = ArrayOf::doubleScalar(defaultTolerance);
    if (nOptions > 1) {
        // a tolerance not given by name is zero
        absoluteTolerance = ArrayOf::doubleScalar(0.);
        relativeTolerance = ArrayOf::doubleScalar(0.);
        for (size_t k = 2; k < argIn.size(); k += 2) {
            std::wstring name = isTextScalar(argIn[k]) ? argIn[k].asWideString() : L"";
            if (isPrefixOf(name, L"AbsoluteTolerance")) {
                absoluteTolerance = checkedTolerance(argIn[k + 1], false);
            } else if (isPrefixOf(name, L"RelativeTolerance")) {
                relativeTolerance = checkedTolerance(argIn[k + 1], true);
            } else {
                Error(L"Nelson:isapprox:invalidParameterName",
                    L"Parameter name expected: 'AbsoluteTolerance' or 'RelativeTolerance'.");
            }
        }
    }
    ArrayOfVector retval;
    retval << IsApprox(A, B, absoluteTolerance, relativeTolerance);
    return retval;
}
//=============================================================================
