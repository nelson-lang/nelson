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
        raiseError(L"Nelson:time:ERROR_NUMERIC_INPUT_DATA_MUST_BE_REAL",
            ERROR_NUMERIC_INPUT_DATA_MUST_BE_REAL);
    }

    bool isLocalized = false;
    if (argIn.size() > 1) {
        const ArrayOf& lastArg = argIn.back();
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
                raiseError(L"Nelson:time:ERROR_UNKNOWN_DATE_FORMAT_NUMBER",
                    ERROR_UNKNOWN_DATE_FORMAT_NUMBER);
            }
        } else if (secondArg.isRowVectorCharacterArray() || secondArg.isScalarStringArray()) {
            userFormatOut = secondArg.getContentAsWideString();
            if (!(argIn.size() == 2 && userFormatOut == L"local")) {
                withUserFormatOut = true;
            }
        } else {
            raiseError(L"Nelson:time:ERROR_SECOND_ARG_MUST_BE_SCALAR_OR_CHARACTER_VECTOR",
                ERROR_SECOND_ARG_MUST_BE_SCALAR_OR_CHARACTER_VECTOR);
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
