//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "sortBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "OverloadRequired.hpp"
#include "Sort.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
isValidDirection(const std::wstring& direction, bool& isAscend)
{
    if (direction == L"a" || direction == L"ascend") {
        isAscend = true;
        return true;
    }
    if (direction == L"d" || direction == L"descend") {
        isAscend = false;
        return true;
    }
    return false;
}
//=============================================================================
static bool
isValidMissingPlacement(
    const std::wstring& name, const std::wstring& value, MISSING_PLACEMENT& placement)
{
    bool res = false;
    if (name == L"MissingPlacement") {
        if (value == L"auto") {
            placement = MISSING_PLACEMENT::AUTO_PLACEMENT;
            res = true;
        } else if (value == L"first") {
            placement = MISSING_PLACEMENT::FIRST_PLACEMENT;
            res = true;
        } else if (value == L"last") {
            placement = MISSING_PLACEMENT::LAST_PLACEMENT;
            res = true;
        } else {
            Error(L"Nelson:data_analysis:autoFirstLastMissingPlacementParameter",
                L"'auto', 'first', or 'last' for 'MissingPlacement' parameter.");
        }
    }
    return res;
}
//=============================================================================
static bool
isValidComparisonMethod(
    const std::wstring& name, const std::wstring& value, COMPARISON_METHOD& comparisonMethod)
{
    bool res = false;
    if (name == L"ComparisonMethod") {
        if (value == L"auto") {
            comparisonMethod = COMPARISON_METHOD::AUTO_METHOD;
            res = true;
        } else if (value == L"real") {
            comparisonMethod = COMPARISON_METHOD::REAL_METHOD;
            res = true;
        } else if (value == L"abs") {
            comparisonMethod = COMPARISON_METHOD::ABS_METHOD;
            res = true;
        } else {
            Error(L"Nelson:data_analysis:autoRealAbsComparisonMethodParameter",
                L"'auto', 'real', or 'abs' for 'ComparisonMethod' parameter.");
        }
    }
    return res;
}
//=============================================================================
struct SortOptions
{
    indexType dim = 0;
    bool ascend = true;
    MISSING_PLACEMENT placement = MISSING_PLACEMENT::AUTO_PLACEMENT;
    COMPARISON_METHOD comparisonMethod = COMPARISON_METHOD::AUTO_METHOD;
};
//=============================================================================
static indexType
parseSortDimension(const ArrayOf& value)
{
    indexType dim = value.asScalarIndex();
    if (dim < 1) {
        Error(L"Nelson:data_analysis:dimensionArgumentPositiveIntegerScalar",
            L"%s must be a positive integer scalar.", L"Dimension argument");
    }
    return dim;
}
//=============================================================================
static bool
parseSortDirection(const ArrayOf& value)
{
    bool ascend = true;
    std::wstring direction = value.asWideString();
    if (!isValidDirection(direction, ascend)) {
        Error(L"Nelson:data_analysis:invalidSortDirection",
            L"Sort direction must be either the string 'ascend' or 'descend'");
    }
    return ascend;
}
//=============================================================================
static void
parseSortDimensionOrDirection(const ArrayOf& value, SortOptions& options)
{
    if (value.isNumeric()) {
        options.dim = parseSortDimension(value);
    } else {
        options.ascend = parseSortDirection(value);
    }
}
//=============================================================================
static void
parseSortOption(const ArrayOfVector& argIn, size_t nameIndex, SortOptions& options)
{
    std::wstring name = argIn[nameIndex].asWideString();
    std::wstring value = argIn[nameIndex + 1].asWideString();
    bool res = isValidComparisonMethod(name, value, options.comparisonMethod);
    if (!res) {
        res = isValidMissingPlacement(name, value, options.placement);
    }
    if (!res) {
        Error(L"Nelson:data_analysis:comparisonMethodMissingPlacementExpected", L"%s expected.",
            L"'ComparisonMethod' or 'MissingPlacement'");
    }
}
//=============================================================================
static SortOptions
parseSortArguments(const ArrayOfVector& argIn)
{
    SortOptions options;
    switch (argIn.size()) {
    case 1:
        break;
    case 2:
        parseSortDimensionOrDirection(argIn[1], options);
        break;
    case 3:
        if (argIn[1].isNumeric()) {
            options.dim = parseSortDimension(argIn[1]);
            options.ascend = parseSortDirection(argIn[2]);
        } else {
            parseSortOption(argIn, 1, options);
        }
        break;
    case 4:
        parseSortDimensionOrDirection(argIn[1], options);
        parseSortOption(argIn, 2, options);
        break;
    case 5:
        if (argIn[1].isNumeric()) {
            options.dim = parseSortDimension(argIn[1]);
            options.ascend = parseSortDirection(argIn[2]);
        } else {
            parseSortOption(argIn, 1, options);
        }
        parseSortOption(argIn, 3, options);
        break;
    case 6:
        parseSortDimensionOrDirection(argIn[1], options);
        parseSortOption(argIn, 2, options);
        parseSortOption(argIn, 4, options);
        break;
    case 7:
        options.dim = parseSortDimension(argIn[1]);
        options.ascend = parseSortDirection(argIn[2]);
        parseSortOption(argIn, 3, options);
        parseSortOption(argIn, 5, options);
        break;
    default:
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
        break;
    }
    return options;
}
//=============================================================================
ArrayOfVector
Nelson::DataAnalysisGateway::sortBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 1);
    SortOptions options = parseSortArguments(argIn);
    bool needToOverload;
    // a sparse input is sorted densely; the sorted values stay sparse, the
    // index output is a full array
    ArrayOf input(argIn[0]);
    bool wasSparse = input.isSparse();
    if (wasSparse) {
        input.makeDense();
    }
    ArrayOfVector retval = Sort(input, argIn.size(), nLhs == 2, options.dim, options.ascend,
        options.placement, options.comparisonMethod, needToOverload);
    if (needToOverload) {
        OverloadRequired("sort");
    }
    if (wasSparse && !retval.empty()) {
        retval[0].makeSparse();
    }
    return retval;
}
//=============================================================================
