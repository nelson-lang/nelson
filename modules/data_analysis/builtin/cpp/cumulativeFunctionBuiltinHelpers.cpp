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
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "cumulativeFunctionBuiltinHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::DataAnalysisGateway {
//=============================================================================
static inline void
getCumulativeArgument(const ArrayOfVector& argIn, int pos, bool& withNaN, bool& reverse)
{
    std::wstring wstr = argIn[pos].asWideString();
    if (wstr == L"includenan" || wstr == L"includemissing") {
        withNaN = true;
    } else if (wstr == L"omitnan" || wstr == L"omitmissing") {
        withNaN = false;
    } else if (wstr == L"reverse") {
        reverse = true;
    } else if (wstr == L"forward") {
        reverse = false;
    } else {
        std::string msg = formatI18nMessage(
            Nelson::resolveMessageTemplate(
                "Nelson:data_analysis:wrongValueForArgument", "Wrong value for #%d argument."),
            pos);
        Error("Nelson:cumulative:invalidOptionValue", msg);
    }
}
//=============================================================================
ArrayOfVector
cumulativeFunctionBuiltin(int nLhs, const ArrayOfVector& argIn, std::string cumulativeFunctionName,
    callback_cumulative_function pCumulativeFunction)
{
    ArrayOfVector retval;
    size_t nRhs = argIn.size();
    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct() || argIn[0].isClassType()) {
        OverloadRequired(cumulativeFunctionName);
    }
    bool withNaN = true;
    bool needOverload = true;
    bool reverse = false;
    indexType n = 0;

    switch (nRhs) {
    case 1: {
    } break;
    case 2: {
        if (argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray()) {
            getCumulativeArgument(argIn, 1, withNaN, reverse);
        } else {
            n = argIn[1].asScalarIndex(false);
        }
    } break;
    case 3: {
        if (argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray()) {
            std::wstring wstr1 = argIn[1].asWideString();
            std::wstring wstr2 = argIn[2].asWideString();
            if (wstr1 == wstr2) {
                Error(L"Nelson:data_analysis:wrongValueForArgument",
                    L"Wrong value for #%d argument.", 3);
            }
            getCumulativeArgument(argIn, 1, withNaN, reverse);
            getCumulativeArgument(argIn, 2, withNaN, reverse);
        } else {
            n = argIn[1].asScalarIndex(false);
            getCumulativeArgument(argIn, 2, withNaN, reverse);
        }
    } break;
    case 4: {
        n = argIn[1].asScalarIndex(false);
        std::wstring wstr1 = argIn[2].asWideString();
        std::wstring wstr2 = argIn[3].asWideString();
        if (wstr1 == wstr2) {
            Error(
                L"Nelson:data_analysis:wrongValueForArgument", L"Wrong value for #%d argument.", 3);
        }
        getCumulativeArgument(argIn, 2, withNaN, reverse);
        getCumulativeArgument(argIn, 3, withNaN, reverse);
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
} // namespace Nelson::DataAnalysisGateway
//=============================================================================
