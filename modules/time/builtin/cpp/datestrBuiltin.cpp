//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "datestrBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "DateString.hpp"
#include "DateToFormattedString.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::datestrBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;

    bool isValid = argIn[0].isDoubleType(true) && !argIn[0].isSparse() && argIn[0].is2D()
        && argIn[0].isPositive();

    if (!isValid) {
        Error(L"Nelson:time:numericInputDataReal", L"Numeric input data must be real.");
    }

    bool isLocalized = false;
    if (argIn.size() > 1) {
        const ArrayOf& lastArg = argIn.back();
        if (lastArg.isRowVectorCharacterArray() || lastArg.isScalarStringArray()) {
            if (lastArg.asWideString() == L"local") {
                isLocalized = true;
            }
        }
    }

    int predefinedFormatOutValue = -1;
    bool withUserFormatOut = false;
    std::wstring userFormatOut;
    if (argIn.size() > 1) {
        ArrayOf secondArg = argIn[1];
        if (secondArg.isDoubleType(true) && secondArg.isScalar()) {
            predefinedFormatOutValue = (int)secondArg.asDoubleScalar();
            if (predefinedFormatOutValue < -1 || predefinedFormatOutValue > 31) {
                Error(
                    L"Nelson:time:unknownDateFormatNumber", L"Unknown %s.", L"date format number");
            }
        } else if (secondArg.isRowVectorCharacterArray() || secondArg.isScalarStringArray()) {
            userFormatOut = secondArg.asWideString();
            if (!(argIn.size() == 2 && userFormatOut == L"local")) {
                withUserFormatOut = true;
            }
        } else {
            Error(L"Nelson:time:e2ArgumentScalarCharacterVector",
                L"#%d argument must be a scalar or a character vector.", 2);
        }
    }

    double* ptrValues = (double*)argIn[0].dataPointer();
    if (argIn[0].isEmpty()) {
        size_t nbCols = 11;
        if (withUserFormatOut) {
            nbCols = userFormatOut.size();
        }
        Dimensions dims(0, nbCols);
        ArrayOf emptyChar = ArrayOf::empty(dims);
        emptyChar.promoteType(NLS_CHAR);
        retval << emptyChar;

    } else {
        std::vector<double> doubleValues(ptrValues, ptrValues + argIn[0].elementCount());
        wstringVector results;
        if (withUserFormatOut) {
            results = dateToUserFormatString(
                doubleValues, argIn[0].dimensions(), userFormatOut, isLocalized);
        } else {
            results = DateString(
                doubleValues, argIn[0].dimensions(), isLocalized, predefinedFormatOutValue);
        }
        retval << ArrayOf::characterArrayFromRows(results);
    }
    return retval;
}
//=============================================================================
