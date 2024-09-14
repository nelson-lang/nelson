//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "datenumBuiltin.hpp"
#include "DateNumber.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Warning.hpp"
#include "Now.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static Dimensions
findCommonDimensions(const Dimensions& dims1, const Dimensions& dims2, bool& isValid);
static Dimensions
findCommonDimensions(
    const Dimensions& dims1, const Dimensions& dims2, const Dimensions& dims3, bool& isValid);
static Dimensions
findCommonDimensions(const Dimensions& dims1, const Dimensions& dims2, const Dimensions& dims3,
    const Dimensions& dims4, const Dimensions& dims5, const Dimensions& dims6, bool& isValid);
//=============================================================================
static ArrayOfVector
datenumOneRhsBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
static ArrayOfVector
datenumOneNumericRhsBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
static ArrayOfVector
datenumOneStringRhsBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
static ArrayOfVector
datenumTwoRhsBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
static ArrayOfVector
datenumThreeRhsBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
static ArrayOfVector
datenumSixRhsBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::datenumBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // DateNumber = datenum(t) (by overload later)

    nargincheck(argIn, 1, 6);
    nargoutcheck(nLhs, 0, 1);
    switch (argIn.size()) {
    case 1: {
        // DateNumber = datenum(DateString)
        // DateNumber = datenum(DateVector)
        return datenumOneRhsBuiltin(nLhs, argIn);
    } break;
    case 2: {
        // DateNumber = datenum(DateString, formatIn)
        // DateNumber = datenum(DateString, pivotYear)
        return datenumTwoRhsBuiltin(nLhs, argIn);
    } break;
    case 3: {
        // DateNumber = datenum(DateString, formatIn, pivotYear)
        // DateNumber = datenum(Y, M, D)
        return datenumThreeRhsBuiltin(nLhs, argIn);
    } break;
    case 6: {
        // DateNumber = datenum(Y, M, D, H, MN, S)
        return datenumSixRhsBuiltin(nLhs, argIn);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    } break;
    }
    return {};
}
//=============================================================================
ArrayOfVector
datenumOneRhsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn[0].isNumeric()) {
        return datenumOneNumericRhsBuiltin(nLhs, argIn);
    }
    return datenumOneStringRhsBuiltin(nLhs, argIn);
}
//=============================================================================
ArrayOfVector
datenumOneNumericRhsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn[0].getElementCount() < 2) {
        retval << argIn[0];
    } else if (argIn[0].getElementCount() == 3 && argIn[0].isRowVector()) {
        ArrayOf asDoubleArrayOf(argIn[0]);
        asDoubleArrayOf.promoteType(NLS_DOUBLE);
        double* v = (double*)asDoubleArrayOf.getDataPointer();
        double year = v[0];
        double month = v[1];
        double day = v[2];
        double hour = 0.;
        double min = 0.;
        double sec = 0.;
        retval << ArrayOf::doubleConstructor(DateNumber(year, month, day, hour, min, sec));
    } else if (argIn[0].getElementCount() == 6 && argIn[0].isRowVector()) {
        ArrayOf asDoubleArrayOf(argIn[0]);
        asDoubleArrayOf.promoteType(NLS_DOUBLE);
        double* v = (double*)asDoubleArrayOf.getDataPointer();
        double year = v[0];
        double month = v[1];
        double day = v[2];
        double hour = v[3];
        double min = v[4];
        double sec = v[5];
        retval << ArrayOf::doubleConstructor(DateNumber(year, month, day, hour, min, sec));
    } else if (argIn[0].is2D() && argIn[0].getColumns() == 3) {
        ArrayOf asDoubleArrayOf(argIn[0]);
        asDoubleArrayOf.promoteType(NLS_DOUBLE);
        double* v = (double*)asDoubleArrayOf.getDataPointer();
        indexType lenghResultVector = argIn[0].getElementCount() / 3;
        Dimensions dimsRes(1, lenghResultVector);
        double* pRes = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, lenghResultVector);
        ArrayOf res = ArrayOf(NLS_DOUBLE, dimsRes, pRes);

#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)lenghResultVector; ++k) {
            double year = v[lenghResultVector * k];
            double month = v[(lenghResultVector * k) + 1];
            double day = v[(lenghResultVector * k) + 2];
            double hour = 0;
            double min = 0;
            double sec = 0;
            pRes[k] = DateNumber(year, month, day, hour, min, sec);
        }
        retval << res;

    } else if (argIn[0].is2D() && argIn[0].getColumns() == 6) {
        ArrayOf asDoubleArrayOf(argIn[0]);
        asDoubleArrayOf.promoteType(NLS_DOUBLE);
        double* v = (double*)asDoubleArrayOf.getDataPointer();
        indexType lenghResultVector = argIn[0].getElementCount() / 6;
        Dimensions dimsRes(1, lenghResultVector);
        double* pRes = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, lenghResultVector);
        ArrayOf res = ArrayOf(NLS_DOUBLE, dimsRes, pRes);

#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)lenghResultVector; ++k) {
            double year = v[lenghResultVector * k];
            double month = v[(lenghResultVector * k) + 1];
            double day = v[(lenghResultVector * k) + 2];
            double hour = v[(lenghResultVector * k) + 3];
            double min = v[(lenghResultVector * k) + 4];
            double sec = v[(lenghResultVector * k) + 5];
            pRes[k] = DateNumber(year, month, day, hour, min, sec);
        }
        retval << res;

    } else {
        retval << argIn[0];
    }
    return retval;
}
//=============================================================================
ArrayOfVector
datenumOneStringRhsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    int pivotYear = 50;
    if (argIn[0].isRowVectorCharacterArray()) {
        std::wstring strdate = argIn[0].getContentAsWideString();
        bool bParsed;
        double res = DateNumber(strdate, false, pivotYear, bParsed);
        if (!bParsed) {
            Error(L"None of the standard formats match the DATE string.");
        }
        retval << ArrayOf::doubleConstructor(res);
    } else if (argIn[0].isStringArray()) {
        ArrayOf* pArrayStr = (ArrayOf*)argIn[0].getDataPointer();
        Dimensions dimsRes = argIn[0].getDimensions();
        double* pRes = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount());
        ArrayOf res = ArrayOf(NLS_DOUBLE, dimsRes, pRes);
        for (indexType k = 0; k < dimsRes.getElementCount(); ++k) {
            if (pArrayStr[k].isRowVectorCharacterArray()) {
                std::wstring strdate = pArrayStr[k].getContentAsWideString();
                bool bParsed;
                pRes[k] = DateNumber(strdate, false, pivotYear, bParsed);
                if (!bParsed) {
                    Error(L"None of the standard formats match the DATE string.");
                }
            } else {
                Error(_W("Failed to convert text to date number."));
            }
        }
        retval << res;
    } else if (argIn[0].isCellArrayOfCharacterVectors()) {
        ArrayOf* pArrayStr = (ArrayOf*)argIn[0].getDataPointer();
        Dimensions dimsRes = argIn[0].getDimensions();
        double* pRes = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount());
        ArrayOf res = ArrayOf(NLS_DOUBLE, dimsRes, pRes);
        for (indexType k = 0; k < dimsRes.getElementCount(); ++k) {
            std::wstring strdate = pArrayStr[k].getContentAsWideString();
            bool bParsed;
            pRes[k] = DateNumber(strdate, false, pivotYear, bParsed);
            if (!bParsed) {
                Error(L"None of the standard formats match the DATE string.");
            }
        }
        retval << res;
    } else {
        Error(_W("vector double, character vector or string array expected."));
    }
    return retval;
}
//=============================================================================
ArrayOfVector
datenumTwoRhsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // DateNumber = datenum(DateString, formatIn)
    bool isCompatibleType = argIn[0].isCellArrayOfCharacterVectors()
        || argIn[0].isRowVectorCharacterArray() || argIn[0].isStringArray();

    if (!isCompatibleType) {
        Error(_W("First input argument must be a date character vector or a string."));
    }
    int pivotYear = 50;
    bool withPivotYear = false;
    bool withFormatIn = false;
    std::wstring formatIn;
    if (argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray()) {
        formatIn = argIn[1].getContentAsWideString();
        withFormatIn = true;
    } else if (argIn[1].isNumeric() && argIn[1].isScalar()) {
        pivotYear = (int)argIn[1].getContentAsDoubleScalar();
        withPivotYear = true;
    } else {
        Error(_W("Second argument must be a character vector, a string or scalar numerica value."));
    }

    wstringVector dateAsString = argIn[0].getContentAsWideStringVector(false);
    Dimensions dimsRes(1, dateAsString.size());
    double* pRes = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dateAsString.size());
    ArrayOf res = ArrayOf(NLS_DOUBLE, dimsRes, pRes);

    ArrayOfVector retval;
    if (withFormatIn) {
        for (size_t k = 0; k < dateAsString.size(); ++k) {
            bool bParsed;
            pRes[k] = DateNumber(dateAsString[k], formatIn, withPivotYear, pivotYear, bParsed);
            if (!bParsed) {
                Error(_W("Failed to convert text to date number."));
            }
        }
    } else {
        for (size_t k = 0; k < dateAsString.size(); ++k) {
            bool bParsed;
            pRes[k] = DateNumber(dateAsString[k], withPivotYear, pivotYear, bParsed);
            if (!bParsed) {
                Error(L"None of the standard formats match the DATE string.");
            }
        }
    }
    retval << res;
    return retval;
}
//=============================================================================
ArrayOfVector
datenumThreeRhsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // DateNumber = datenum(DateString, formatIn, pivotYear)
    // DateNumber = datenum(Y, M, D)
    ArrayOfVector retval;
    bool withPivotYear = false;
    bool isDateString = argIn[0].isRowVectorCharacterArray()
        || argIn[0].isCellArrayOfCharacterVectors() || argIn[0].isStringArray();
    if (isDateString) {
        wstringVector dateString = argIn[0].getContentAsWideStringVector(false);
        if (argIn[1].isScalarStringArray() || argIn[1].isRowVectorCharacterArray()) {
            std::wstring formatIn = argIn[1].getContentAsWideString();
            int pivotYear = (int)argIn[2].getContentAsDoubleScalar();
            withPivotYear = true;
            Dimensions dimsRes(1, dateString.size());
            double* pRes = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount());
            ArrayOf res = ArrayOf(NLS_DOUBLE, dimsRes, pRes);
            for (size_t k = 0; k < dimsRes.getElementCount(); ++k) {
                bool bParsed = false;
                pRes[k] = DateNumber(dateString[k], withPivotYear, pivotYear, bParsed);
                if (!bParsed) {
                    Error(_W("Failed to convert text to date number."));
                }
            }
            retval << res;
        }
    } else {
        bool isAllNumeric = argIn[0].isNumeric() && argIn[1].isNumeric() && argIn[2].isNumeric();
        if (!isAllNumeric) {
            Error(_W("vector double, character vector or string array expected."));
        }
        ArrayOf param1(argIn[0]);
        ArrayOf param2(argIn[1]);
        ArrayOf param3(argIn[2]);
        bool isValid;
        Dimensions dimsRes = findCommonDimensions(
            param1.getDimensions(), param2.getDimensions(), param3.getDimensions(), isValid);
        if (!isValid) {
            Error(_W("Invalid vector size must be compatible"));
        }
        ArrayOf param1AsDouble(param1);
        param1AsDouble.promoteType(NLS_DOUBLE);
        ArrayOf param2AsDouble(param2);
        param2AsDouble.promoteType(NLS_DOUBLE);
        ArrayOf param3AsDouble(param3);
        param3AsDouble.promoteType(NLS_DOUBLE);

        double* ptrParam1 = (double*)param1AsDouble.getDataPointer();
        double* ptrParam2 = (double*)param2AsDouble.getDataPointer();
        double* ptrParam3 = (double*)param3AsDouble.getDataPointer();
        double hour = 0.;
        double min = 0.;
        double sec = 0.;

        bool param1IsScalar = param1AsDouble.isScalar();
        bool param2IsScalar = param2AsDouble.isScalar();
        bool param3IsScalar = param3AsDouble.isScalar();

        double* pRes = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount());
        ArrayOf res = ArrayOf(NLS_DOUBLE, dimsRes, pRes);

#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)dimsRes.getElementCount(); ++k) {
            double year = param1IsScalar ? ptrParam1[0] : ptrParam1[k];
            double month = param2IsScalar ? ptrParam2[0] : ptrParam2[k];
            double day = param3IsScalar ? ptrParam3[0] : ptrParam3[k];
            pRes[k] = DateNumber(year, month, day, hour, min, sec);
        }
        retval << res;
    }
    return retval;
}
//=============================================================================
ArrayOfVector
datenumSixRhsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // DateNumber = datenum(Y, M, D, H, MN, S)
    ArrayOfVector retval;
    ArrayOf param1(argIn[0]);
    ArrayOf param2(argIn[1]);
    ArrayOf param3(argIn[2]);
    ArrayOf param4(argIn[3]);
    ArrayOf param5(argIn[4]);
    ArrayOf param6(argIn[5]);

    bool isValid;
    Dimensions dimsRes = findCommonDimensions(param1.getDimensions(), param2.getDimensions(),
        param3.getDimensions(), param4.getDimensions(), param5.getDimensions(),
        param6.getDimensions(), isValid);
    if (!isValid) {
        Error(_W("Invalid vector size must be compatible"));
    }

    ArrayOf param1AsDouble(param1);
    param1AsDouble.promoteType(NLS_DOUBLE);
    ArrayOf param2AsDouble(param2);
    param2AsDouble.promoteType(NLS_DOUBLE);
    ArrayOf param3AsDouble(param3);
    param3AsDouble.promoteType(NLS_DOUBLE);
    ArrayOf param4AsDouble(param4);
    param4AsDouble.promoteType(NLS_DOUBLE);
    ArrayOf param5AsDouble(param5);
    param5AsDouble.promoteType(NLS_DOUBLE);
    ArrayOf param6AsDouble(param6);
    param6AsDouble.promoteType(NLS_DOUBLE);

    double* ptrParam1 = (double*)param1AsDouble.getDataPointer();
    double* ptrParam2 = (double*)param2AsDouble.getDataPointer();
    double* ptrParam3 = (double*)param3AsDouble.getDataPointer();
    double* ptrParam4 = (double*)param4AsDouble.getDataPointer();
    double* ptrParam5 = (double*)param5AsDouble.getDataPointer();
    double* ptrParam6 = (double*)param6AsDouble.getDataPointer();

    bool param1IsScalar = param1AsDouble.isScalar();
    bool param2IsScalar = param2AsDouble.isScalar();
    bool param3IsScalar = param3AsDouble.isScalar();
    bool param4IsScalar = param4AsDouble.isScalar();
    bool param5IsScalar = param5AsDouble.isScalar();
    bool param6IsScalar = param6AsDouble.isScalar();

    double* pRes = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount());
    ArrayOf res = ArrayOf(NLS_DOUBLE, dimsRes, pRes);

#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)dimsRes.getElementCount(); ++k) {
        double year = param1IsScalar ? ptrParam1[0] : ptrParam1[k];
        double month = param2IsScalar ? ptrParam2[0] : ptrParam2[k];
        double day = param3IsScalar ? ptrParam3[0] : ptrParam3[k];
        double hour = param4IsScalar ? ptrParam4[0] : ptrParam4[k];
        double min = param5IsScalar ? ptrParam5[0] : ptrParam5[k];
        double sec = param6IsScalar ? ptrParam6[0] : ptrParam6[k];
        pRes[k] = DateNumber(year, month, day, hour, min, sec);
    }
    retval << res;
    return retval;
}
//=============================================================================
Dimensions
findCommonDimensions(const Dimensions& dims1, const Dimensions& dims2, bool& isValid)
{
    Dimensions dimsRes;
    isValid = false;
    if (dims1.isScalar()) {
        dimsRes = dims2;
        isValid = true;
    } else if (dims2.isScalar()) {
        dimsRes = dims1;
        isValid = true;
    } else if (dims1.equals(dims2)) {
        dimsRes = dims1;
        isValid = true;
    } else {
        isValid = false;
    }
    return dimsRes;
}
//=============================================================================
Dimensions
findCommonDimensions(
    const Dimensions& dims1, const Dimensions& dims2, const Dimensions& dims3, bool& isValid)
{
    Dimensions dimsRes = findCommonDimensions(dims1, dims2, isValid);
    if (!isValid) {
        return dimsRes;
    }
    return findCommonDimensions(dimsRes, dims3, isValid);
}
//=============================================================================
Dimensions
findCommonDimensions(const Dimensions& dims1, const Dimensions& dims2, const Dimensions& dims3,
    const Dimensions& dims4, const Dimensions& dims5, const Dimensions& dims6, bool& isValid)
{
    Dimensions dimsRes = findCommonDimensions(dims1, dims2, isValid);
    if (!isValid) {
        return dimsRes;
    }
    dimsRes = findCommonDimensions(dimsRes, dims3, isValid);
    if (!isValid) {
        return dimsRes;
    }
    dimsRes = findCommonDimensions(dimsRes, dims4, isValid);
    if (!isValid) {
        return dimsRes;
    }
    dimsRes = findCommonDimensions(dimsRes, dims5, isValid);
    if (!isValid) {
        return dimsRes;
    }
    return findCommonDimensions(dimsRes, dims6, isValid);
}
//=============================================================================
