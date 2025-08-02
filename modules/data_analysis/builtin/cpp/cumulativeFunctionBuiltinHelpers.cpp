//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "cumulativeFunctionBuiltinHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::DataAnalysisGateway {
//=============================================================================
static inline void
getCumulativeArgument(const ArrayOfVector& argIn, int pos, bool& withNaN, bool& reverse)
{
    std::wstring wstr = argIn[pos].getContentAsWideString();
    if (wstr == L"includenan") {
        withNaN = true;
    } else if (wstr == L"omitnan") {
        withNaN = false;
    } else if (wstr == L"reverse") {
        reverse = true;
    } else if (wstr == L"forward") {
        reverse = false;
    } else {
        std::string msg = fmt::sprintf(_("Wrong value for #%d argument."), pos);
        Error(msg);
    }
}
//=============================================================================
ArrayOfVector
cumulativeFunctionBuiltin(int nLhs, const ArrayOfVector& argIn, std::string cumulativeFunctionName,
    callback_cumulative_function pCumulativeFunction)
{
    ArrayOfVector retval;
    size_t nRhs = argIn.size();
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isSparse() || argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct()
        || argIn[0].isClassType()) {
        OverloadRequired(cumulativeFunctionName);
    }
    bool withNaN = true;
    bool needOverload = true;
    bool reverse = false;
    indexType n = 0;

    switch (nRhs) {
    case 1: {
        // NOTHING TO DO
    } break;
    case 2: {
        if (argIn[1].isRowVectorCharacterArray()) {
            getCumulativeArgument(argIn, 1, withNaN, reverse);
        } else {
            n = argIn[1].getContentAsScalarIndex(false);
        }
    } break;
    case 3: {
        if (argIn[1].isRowVectorCharacterArray()) {
            std::wstring wstr1 = argIn[1].getContentAsWideString();
            std::wstring wstr2 = argIn[2].getContentAsWideString();
            if (wstr1 == wstr2) {
                Error(_W("Wrong value for #3 argument."));
            }
            getCumulativeArgument(argIn, 1, withNaN, reverse);
            getCumulativeArgument(argIn, 2, withNaN, reverse);
        } else {
            n = argIn[1].getContentAsScalarIndex(false);
            getCumulativeArgument(argIn, 2, withNaN, reverse);
        }
    } break;
    case 4: {
        n = argIn[1].getContentAsScalarIndex(false);
        std::wstring wstr1 = argIn[2].getContentAsWideString();
        std::wstring wstr2 = argIn[3].getContentAsWideString();
        if (wstr1 == wstr2) {
            Error(_W("Wrong value for #3 argument."));
        }
        getCumulativeArgument(argIn, 2, withNaN, reverse);
        getCumulativeArgument(argIn, 3, withNaN, reverse);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }

    ArrayOf res = pCumulativeFunction(argIn[0], n, withNaN, reverse, needOverload);
    if (needOverload) {
        OverloadRequired(cumulativeFunctionName);
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
}
//=============================================================================
