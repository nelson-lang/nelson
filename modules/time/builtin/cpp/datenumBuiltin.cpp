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
static ArrayOfVector
datanumBuiltinNoRhs(int nLhs);
//=============================================================================
static ArrayOfVector
datanumBuiltinOneRhs(int nLhs, const ArrayOf& param1);
//=============================================================================
static ArrayOfVector
datanumBuiltinTwoRhs(int nLhs, const ArrayOf& param1, const ArrayOf& param2);
//=============================================================================
static ArrayOfVector
datanumBuiltinThreeRhs(
    int nLhs, const ArrayOf& param1, const ArrayOf& param2, const ArrayOf& param3);
//=============================================================================
static ArrayOfVector
datanumBuiltinSixRhs(int nLhs, const ArrayOf& param1, const ArrayOf& param2, const ArrayOf& param3,
    const ArrayOf& param4, const ArrayOf& param5, const ArrayOf& param6);
//=============================================================================
static Dimensions
findCommonDimensions(const Dimensions& dims1, const Dimensions& dims2, bool& isValid);
//=============================================================================
static Dimensions
findCommonDimensions(
    const Dimensions& dims1, const Dimensions& dims2, const Dimensions& dims3, bool& isValid);
//=============================================================================
static Dimensions
findCommonDimensions(const Dimensions& dims1, const Dimensions& dims2, const Dimensions& dims3,
    const Dimensions& dims4, const Dimensions& dims5, const Dimensions& dims6, bool& isValid);
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::datenumBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // DateNumber = datenum()
    // DateNumber = datenum(t)
    // DateNumber = datenum(DateString)
    // DateNumber = datenum(DateVector)
    // DateNumber = datenum(DateString, formatIn)
    // DateNumber = datenum(Y, M, D)
    // DateNumber = datenum(Y, M, D, H, MN, S)
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    switch (argIn.size()) {
    case 0: {
        retval = datanumBuiltinNoRhs(nLhs);
    } break;
    case 1: {
        retval = datanumBuiltinOneRhs(nLhs, argIn[0]);
    } break;
    case 2: {
        retval = datanumBuiltinTwoRhs(nLhs, argIn[0], argIn[1]);
    } break;
    case 3: {
        retval = datanumBuiltinThreeRhs(nLhs, argIn[0], argIn[1], argIn[2]);
    } break;
    case 6: {
        retval = datanumBuiltinSixRhs(
            nLhs, argIn[0], argIn[1], argIn[2], argIn[3], argIn[4], argIn[5]);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
ArrayOfVector
datanumBuiltinNoRhs(int nLhs)
{
    ArrayOfVector retval;
    retval << ArrayOf::doubleConstructor(Now());
    return retval;
}
//=============================================================================
static ArrayOfVector
datanumBuiltinOneRhs(int nLhs, const ArrayOf& param1)
{
    ArrayOfVector retval;
    if (param1.isNumeric()) {
        if (param1.getElementCount() < 2) {
            retval << param1;
        } else if (param1.getElementCount() == 3 && param1.isRowVector()) {
            ArrayOf asDoubleArrayOf(param1);
            asDoubleArrayOf.promoteType(NLS_DOUBLE);
            double* v = (double*)asDoubleArrayOf.getDataPointer();
            double year = v[0];
            double month = v[1];
            double day = v[2];
            double hour = 0.;
            double min = 0.;
            double sec = 0.;
            retval << ArrayOf::doubleConstructor(DateNumber(year, month, day, hour, min, sec));
        } else if (param1.getElementCount() == 6 && param1.isRowVector()) {
            ArrayOf asDoubleArrayOf(param1);
            asDoubleArrayOf.promoteType(NLS_DOUBLE);
            double* v = (double*)asDoubleArrayOf.getDataPointer();
            double year = v[0];
            double month = v[1];
            double day = v[2];
            double hour = v[3];
            double min = v[4];
            double sec = v[5];
            retval << ArrayOf::doubleConstructor(DateNumber(year, month, day, hour, min, sec));
        } else if (param1.is2D() && param1.getColumns() == 3) {
            ArrayOf asDoubleArrayOf(param1);
            asDoubleArrayOf.promoteType(NLS_DOUBLE);
            double* v = (double*)asDoubleArrayOf.getDataPointer();
            indexType lenghResultVector = param1.getElementCount() / 3;
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

        } else if (param1.is2D() && param1.getColumns() == 6) {
            ArrayOf asDoubleArrayOf(param1);
            asDoubleArrayOf.promoteType(NLS_DOUBLE);
            double* v = (double*)asDoubleArrayOf.getDataPointer();
            indexType lenghResultVector = param1.getElementCount() / 6;
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
            retval << param1;
        }
    } else {
        if (param1.isRowVectorCharacterArray()) {
            std::wstring strdate = param1.getContentAsWideString();
            bool bParsed;
            double res = DateNumber(strdate, bParsed);
            if (!bParsed) {
                Error(L"None of the standard formats match the DATE string.");
            }
            retval << ArrayOf::doubleConstructor(res);
        } else if (param1.isStringArray()) {
            ArrayOf* pArrayStr = (ArrayOf*)param1.getDataPointer();
            Dimensions dimsRes = param1.getDimensions();
            double* pRes = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount());
            ArrayOf res = ArrayOf(NLS_DOUBLE, dimsRes, pRes);
            for (indexType k = 0; k < dimsRes.getElementCount(); ++k) {
                if (pArrayStr[k].isRowVectorCharacterArray()) {
                    std::wstring strdate = pArrayStr[k].getContentAsWideString();
                    bool bParsed;
                    pRes[k] = DateNumber(strdate, bParsed);
                    if (!bParsed) {
                        Error(L"None of the standard formats match the DATE string.");
                    }
                } else {
                    Error(_W("Failed to convert text to date number."));
                }
            }
            retval << res;
        } else if (param1.isCellArrayOfCharacterVectors()) {
            ArrayOf* pArrayStr = (ArrayOf*)param1.getDataPointer();
            Dimensions dimsRes = param1.getDimensions();
            double* pRes = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount());
            ArrayOf res = ArrayOf(NLS_DOUBLE, dimsRes, pRes);
            for (indexType k = 0; k < dimsRes.getElementCount(); ++k) {
                std::wstring strdate = pArrayStr[k].getContentAsWideString();
                bool bParsed;
                pRes[k] = DateNumber(strdate, bParsed);
                if (!bParsed) {
                    Error(L"None of the standard formats match the DATE string.");
                }
            }
            retval << res;
        } else {
            Error(_W("vector double, character vector or string array expected."));
        }
    }
    return retval;
}
//=============================================================================
ArrayOfVector
datanumBuiltinTwoRhs(int nLhs, const ArrayOf& param1, const ArrayOf& param2)
{
    ArrayOfVector retval;
    if (param1.isNumeric()) {
        Warning(_W("a date vector or string expected, all subsequent arguments are ignored."));
        retval << param1;
    } else if (param1.isStringArray()) {
        std::wstring dateformat = param2.getContentAsWideString();
        ArrayOf* pArrayStr = (ArrayOf*)param1.getDataPointer();
        Dimensions dimsRes = param1.getDimensions();
        double* pRes = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount());
        ArrayOf res = ArrayOf(NLS_DOUBLE, dimsRes, pRes);
        for (indexType k = 0; k < dimsRes.getElementCount(); ++k) {
            if (pArrayStr[k].isRowVectorCharacterArray()) {
                std::wstring strdate = pArrayStr[k].getContentAsWideString();
                bool bParsed;
                pRes[k] = DateNumber(strdate, dateformat, bParsed);
                if (!bParsed) {
                    Error(L"None of the standard formats match the DATE string.");
                }
            } else {
                Error(_W("Failed to convert text to date number."));
            }
        }
        retval << res;
    } else if (param1.isCellArrayOfCharacterVectors()) {
        std::wstring dateformat = param2.getContentAsWideString();
        ArrayOf* pArrayStr = (ArrayOf*)param1.getDataPointer();
        Dimensions dimsRes = param1.getDimensions();
        double* pRes = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount());
        ArrayOf res = ArrayOf(NLS_DOUBLE, dimsRes, pRes);
        for (indexType k = 0; k < dimsRes.getElementCount(); ++k) {
            std::wstring strdate = pArrayStr[k].getContentAsWideString();
            bool bParsed;
            pRes[k] = DateNumber(strdate, dateformat, bParsed);
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
datanumBuiltinThreeRhs(
    int nLhs, const ArrayOf& param1, const ArrayOf& param2, const ArrayOf& param3)
{
    ArrayOfVector retval;
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
    return retval;
}
//=============================================================================
ArrayOfVector
datanumBuiltinSixRhs(int nLhs, const ArrayOf& param1, const ArrayOf& param2, const ArrayOf& param3,
    const ArrayOf& param4, const ArrayOf& param5, const ArrayOf& param6)
{
    ArrayOfVector retval;
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
