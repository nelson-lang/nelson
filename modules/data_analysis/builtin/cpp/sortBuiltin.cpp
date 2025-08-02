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
            Error(_W("'auto', 'first', or 'last' for 'MissingPlacement' parameter."));
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
            Error(_W("'auto', 'real', or 'abs' for 'ComparisonMethod' parameter."));
        }
    }
    return res;
}
//=============================================================================
ArrayOfVector
Nelson::DataAnalysisGateway::sortBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 1);
    MISSING_PLACEMENT placement = MISSING_PLACEMENT::AUTO_PLACEMENT;
    COMPARISON_METHOD comparisonMethod = COMPARISON_METHOD::AUTO_METHOD;

    ArrayOf A;
    indexType dim = 0;
    bool ascend = true;
    std::wstring name;
    std::wstring value;
    switch (argIn.size()) {
    case 0: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    case 1: {
        // sort(A)
        A = argIn[0];
    } break;
    case 2: {
        // sort(A, 'ascend')
        // sort(A, 'descend')
        // sort(A, dim)
        // sort(A, dim)
        A = argIn[0];
        ArrayOf param2 = argIn[1];
        if (param2.isNumeric()) {
            dim = param2.getContentAsScalarIndex();
            if (dim < 1) {
                Error(_W("Dimension argument to sort should be positive"));
            }
        } else {
            std::wstring direction = param2.getContentAsWideString();
            if (!isValidDirection(direction, ascend)) {
                Error(_W("Sort direction must be either the string 'ascend' or 'descend'"));
            }
        }
    } break;
    case 3: {
        // sort(A, 'MissingPlacement', value)
        // sort(A, 'ComparisonMethod', value)
        // sort(A, dim, 'ascend')
        A = argIn[0];
        ArrayOf param2 = argIn[1];
        if (param2.isNumeric()) {
            dim = param2.getContentAsScalarIndex();
            if (dim < 1) {
                Error(_W("Dimension argument to sort should be positive"));
            }
            ArrayOf param3 = argIn[2];
            std::wstring direction = param3.getContentAsWideString();
            if (!isValidDirection(direction, ascend)) {
                Error(_W("Sort direction must be either the string 'ascend' or 'descend'"));
            }
        } else {
            name = argIn[1].getContentAsWideString();
            value = argIn[2].getContentAsWideString();
            bool res = isValidComparisonMethod(name, value, comparisonMethod);
            if (!res) {
                res = isValidMissingPlacement(name, value, placement);
            }
            if (!res) {
                Error(_W("'ComparisonMethod' or 'MissingPlacement' expected."));
            }
        }
    } break;
    case 4: {
        // sort(A, 'ascend', 'MissingPlacement', value)
        // sort(A, 'ascend', 'ComparisonMethod', value)
        // sort(A, dim, 'MissingPlacement', value)
        // sort(A, dim, 'ComparisonMethod', value)
        A = argIn[0];
        ArrayOf param2 = argIn[1];
        if (param2.isNumeric()) {
            dim = param2.getContentAsScalarIndex();
            if (dim < 1) {
                Error(_W("Dimension argument to sort should be positive"));
            }
        } else {
            std::wstring direction = param2.getContentAsWideString();
            if (!isValidDirection(direction, ascend)) {
                Error(_W("Sort direction must be either the string 'ascend' or 'descend'"));
            }
        }
        name = argIn[2].getContentAsWideString();
        value = argIn[3].getContentAsWideString();
        bool res = isValidComparisonMethod(name, value, comparisonMethod);
        if (!res) {
            res = isValidMissingPlacement(name, value, placement);
        }
        if (!res) {
            Error(_W("'ComparisonMethod' or 'MissingPlacement' expected."));
        }
    } break;
    case 5: {
        // sort(A, 'MissingPlacement', value, 'ComparisonMethod', value)
        // sort(A, 'ComparisonMethod', value, 'MissingPlacement', value)
        // sort(A, dim, ascend, 'ComparisonMethod', value);
        // sort(A, dim, ascend, 'MissingPlacement', value);
        A = argIn[0];
        ArrayOf param2 = argIn[1];
        if (param2.isNumeric()) {
            dim = param2.getContentAsScalarIndex();
            if (dim < 1) {
                Error(_W("Dimension argument to sort should be positive"));
            }
            std::wstring direction = argIn[2].getContentAsWideString();
            if (!isValidDirection(direction, ascend)) {
                Error(_W("Sort direction must be either the string 'ascend' or 'descend'"));
            }

            name = argIn[2].getContentAsWideString();
            value = argIn[3].getContentAsWideString();
            bool res = isValidComparisonMethod(name, value, comparisonMethod);
            if (!res) {
                res = isValidMissingPlacement(name, value, placement);
            }
            if (!res) {
                Error(_W("'ComparisonMethod' or 'MissingPlacement' expected."));
            }
        } else {
            name = argIn[1].getContentAsWideString();
            value = argIn[2].getContentAsWideString();
            bool res = isValidComparisonMethod(name, value, comparisonMethod);
            if (!res) {
                res = isValidMissingPlacement(name, value, placement);
            }
            if (!res) {
                Error(_W("'ComparisonMethod' or 'MissingPlacement' expected."));
            }

            name = argIn[3].getContentAsWideString();
            value = argIn[4].getContentAsWideString();
            res = isValidComparisonMethod(name, value, comparisonMethod);
            if (!res) {
                res = isValidMissingPlacement(name, value, placement);
            }
            if (!res) {
                Error(_W("'ComparisonMethod' or 'MissingPlacement' expected."));
            }
        }
    } break;
    case 6: {
        // sort(A, 'ascend', 'MissingPlacement', value, 'ComparisonMethod', value)
        // sort(A, 'ascend', 'ComparisonMethod', value, 'MissingPlacement', value)
        // sort(A, dim, 'MissingPlacement', value, 'ComparisonMethod', value)
        // sort(A, dim, 'ComparisonMethod', value, 'MissingPlacement', value)
        A = argIn[0];
        ArrayOf param2 = argIn[1];
        if (param2.isNumeric()) {
            dim = param2.getContentAsScalarIndex();
            if (dim < 1) {
                Error(_W("Dimension argument to sort should be positive"));
            }
        } else {
            std::wstring direction = param2.getContentAsWideString();
            if (!isValidDirection(direction, ascend)) {
                Error(_W("Sort direction must be either the string 'ascend' or 'descend'"));
            }
        }
        name = argIn[2].getContentAsWideString();
        value = argIn[3].getContentAsWideString();
        bool res = isValidComparisonMethod(name, value, comparisonMethod);
        if (!res) {
            res = isValidMissingPlacement(name, value, placement);
        }
        if (!res) {
            Error(_W("'ComparisonMethod' or 'MissingPlacement' expected."));
        }
        name = argIn[4].getContentAsWideString();
        value = argIn[5].getContentAsWideString();
        res = isValidComparisonMethod(name, value, comparisonMethod);
        if (!res) {
            res = isValidMissingPlacement(name, value, placement);
        }
        if (!res) {
            Error(_W("'ComparisonMethod' or 'MissingPlacement' expected."));
        }
    } break;
    case 7: {
        // sort(A, dim, 'ascend', 'MissingPlacement', value, 'ComparisonMethod', value)
        // sort(A, dim, 'ascend', 'ComparisonMethod', value, 'MissingPlacement', value)
        A = argIn[0];
        ArrayOf param2 = argIn[1];
        dim = param2.getContentAsScalarIndex();
        if (dim < 1) {
            Error(_W("Dimension argument to sort should be positive"));
        }
        std::wstring direction = argIn[2].getContentAsWideString();
        if (!isValidDirection(direction, ascend)) {
            Error(_W("Sort direction must be either the string 'ascend' or 'descend'"));
        }
        name = argIn[3].getContentAsWideString();
        value = argIn[4].getContentAsWideString();
        bool res = isValidComparisonMethod(name, value, comparisonMethod);
        if (!res) {
            res = isValidMissingPlacement(name, value, placement);
        }
        if (!res) {
            Error(_W("'ComparisonMethod' or 'MissingPlacement' expected."));
        }

        name = argIn[5].getContentAsWideString();
        value = argIn[6].getContentAsWideString();
        res = isValidComparisonMethod(name, value, comparisonMethod);
        if (!res) {
            res = isValidMissingPlacement(name, value, placement);
        }
        if (!res) {
            Error(_W("'ComparisonMethod' or 'MissingPlacement' expected."));
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }

    bool needToOverload;
    retval = Sort(
        A, argIn.size(), nLhs == 2, dim, ascend, placement, comparisonMethod, needToOverload);
    if (needToOverload) {
        OverloadRequired("sort");
    }
    return retval;
}
//=============================================================================
