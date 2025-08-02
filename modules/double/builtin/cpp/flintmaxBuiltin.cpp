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
            Error(_W("#1 'double' or 'single' expected."));
        }
    } else if (argIn.size() == 2) {
        if (argIn[0].isRowVectorCharacterArray()) {
            std::wstring param = argIn[0].getContentAsWideString();
            if (param == L"like") {
                if (argIn[1].isDoubleType()) {
                    double intmax = (1ULL << DBL_MANT_DIG);
                    retval << ArrayOf::doubleConstructor(intmax);
                } else if (argIn[1].isSingleType()) {
                    single intmax = (1UL << FLT_MANT_DIG);
                    retval << ArrayOf::singleConstructor(intmax);
                } else {
                    Error(_("Second argument must a double or single variable."));
                }
            } else {
                Error(_("First argument must be 'like'."));
            }
        } else {
            Error(_("First argument must be 'like'."));
        }
    } else {
        double intmax = (1ULL << DBL_MANT_DIG);
        retval << ArrayOf::doubleConstructor(intmax);
    }
    return retval;
}
//=============================================================================
