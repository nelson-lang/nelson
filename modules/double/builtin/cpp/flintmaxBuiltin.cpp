//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cfloat>
#include "flintmaxBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DoubleGateway::flintmaxBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 2);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 1) {
        ArrayOf param1 = argIn[0];
        std::wstring paramStr = param1.getContentAsWideString();
        if ((paramStr == L"double" || paramStr == L"single")) {
            if (paramStr == L"double") {
                double intmax = (1ULL << DBL_MANT_DIG);
                retval << ArrayOf::doubleConstructor(intmax);
            } else {
                single intmax = (1UL << FLT_MANT_DIG);
                retval << ArrayOf::singleConstructor(intmax);
            }
        } else {
            raiseError(L"Nelson:double:ERROR_DOUBLE_OR_SINGLE_TYPE_EXPECTED",
                ERROR_DOUBLE_OR_SINGLE_TYPE_EXPECTED);
        }
    } else if (argIn.size() == 2) {
        if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
            std::wstring param = argIn[0].getContentAsWideString();
            if (param == L"like") {
                if (argIn[1].isDoubleType()) {
                    double intmax = (1ULL << DBL_MANT_DIG);
                    retval << ArrayOf::doubleConstructor(intmax);
                } else if (argIn[1].isSingleType()) {
                    single intmax = (1UL << FLT_MANT_DIG);
                    retval << ArrayOf::singleConstructor(intmax);
                } else {
                    raiseError(L"Nelson:double:ERROR_SECOND_ARGUMENT_MUST_BE_DOUBLE_OR_SINGLE",
                        ERROR_DOUBLE_OR_SINGLE_TYPE_EXPECTED);
                }
            } else {
                raiseError(L"Nelson:double:ERROR_FIRST_ARGUMENT_MUST_BE_LIKE",
                    ERROR_LIKE_EXPECTED_AT_N_MINUS_2_ARG);
            }
        } else {
            raiseError(L"Nelson:double:ERROR_FIRST_ARGUMENT_MUST_BE_LIKE",
                ERROR_LIKE_EXPECTED_AT_N_MINUS_2_ARG);
        }
    } else {
        double intmax = (1ULL << DBL_MANT_DIG);
        retval << ArrayOf::doubleConstructor(intmax);
    }
    return retval;
}
//=============================================================================
