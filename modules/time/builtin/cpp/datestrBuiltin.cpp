//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "datestrBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "DateString.hpp"
#include "DateToFormattedString.hpp"
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
        Error(_W("Numeric input data must be real."));
    }

    bool isLocalized = false;
    if (argIn.size() > 1) {
        ArrayOf lastArg = argIn[argIn.size() - 1];
        if (lastArg.isRowVectorCharacterArray() || lastArg.isScalarStringArray()) {
            if (lastArg.getContentAsWideString() == L"local") {
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
            predefinedFormatOutValue = (int)secondArg.getContentAsDoubleScalar();
            if (predefinedFormatOutValue < -1 || predefinedFormatOutValue > 31) {
                Error(_W("Unknown date format number."));
            }
        } else if (secondArg.isRowVectorCharacterArray() || secondArg.isScalarStringArray()) {
            userFormatOut = secondArg.getContentAsWideString();
            if (argIn.size() == 2 && userFormatOut == L"local") {

            } else {
                withUserFormatOut = true;
            }
        } else {
            Error(_W("#2 argument must be a scalar or a character vector."));
        }
    }

    double* ptrValues = (double*)argIn[0].getDataPointer();
    if (argIn[0].isEmpty()) {
        size_t nbCols = 11;
        if (withUserFormatOut) {
            nbCols = userFormatOut.size();
        }
        Dimensions dims(0, nbCols);
        ArrayOf emptyChar = ArrayOf::emptyConstructor(dims);
        emptyChar.promoteType(NLS_CHAR);
        retval << emptyChar;

    } else {
        std::vector<double> doubleValues(ptrValues, ptrValues + argIn[0].getElementCount());
        wstringVector results;
        if (withUserFormatOut) {
            results = dateToUserFormatString(
                doubleValues, argIn[0].getDimensions(), userFormatOut, isLocalized);
        } else {
            results = DateString(
                doubleValues, argIn[0].getDimensions(), isLocalized, predefinedFormatOutValue);
        }
        retval << ArrayOf::characterVectorToCharacterArray(results);
    }
    return retval;
}
//=============================================================================
