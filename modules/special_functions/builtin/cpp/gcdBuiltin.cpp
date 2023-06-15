//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gcdBuiltin.hpp"
#include "Error.hpp"
#include "GCD.hpp"
#include "ClassName.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ClassToString.hpp"
#include "ClassName.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static NelsonType
getCommonType(NelsonType typeA, NelsonType typeB)
{
    if (typeA == typeB) {
        return typeA;
    }
    if (typeA == NLS_DOUBLE || typeA == NLS_DCOMPLEX) {
        return typeB;
    }
    if (typeB == NLS_DOUBLE || typeA == NLS_DCOMPLEX) {
        return typeA;
    }
    return NLS_UNKNOWN;
}
//=============================================================================
static std::string
precedenceType(const ArrayOf& A, const ArrayOf& B, NelsonType& destinationType)
{
    NelsonType classA = A.getDataClass();
    NelsonType classB = B.getDataClass();
    bool isObjectA = A.isClassStruct() || (classA == NLS_HANDLE);
    bool isObjectB = B.isClassStruct() || (classB == NLS_HANDLE);
    if (isObjectA) {
        if (classA == NLS_HANDLE) {
            destinationType = NLS_HANDLE;
            return A.getHandleCategory();
        }
        destinationType = NLS_STRUCT_ARRAY;
        return A.getStructType();
    }
    if (isObjectB) {
        if (classB == NLS_HANDLE) {
            destinationType = NLS_HANDLE;
            return B.getHandleCategory();
        }
        destinationType = NLS_STRUCT_ARRAY;
        return B.getStructType();
    }
    if (classA == classB) {
        destinationType = classA;
    } else if (A.isIntegerType()) {
        bool isCompatible = (classB == NLS_DOUBLE) && B.isScalar();
        if (!isCompatible) {
            Error(_W("Integers can only be combined with integers of the same class, or scalar "
                     "doubles."));
        }
        destinationType = classA;
    } else if (B.isIntegerType()) {
        bool isCompatible = (classA == NLS_DOUBLE) && A.isScalar();
        if (!isCompatible) {
            Error(_W("Integers can only be combined with integers of the same class, or scalar "
                     "doubles."));
        }
        destinationType = classB;
    } else {
        bool asComplex = A.isComplex() || B.isComplex();
        bool asSingle = A.isSingleClass() || B.isSingleClass();
        if (asComplex) {
            if (asSingle) {
                destinationType = NLS_SCOMPLEX;
            } else {
                destinationType = NLS_DCOMPLEX;
            }
        } else {
            if (asSingle) {
                destinationType = NLS_SINGLE;
            } else {
                destinationType = NLS_DOUBLE;
            }
        }
    }
    return ClassToString(destinationType);
}
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::gcdBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);

    NelsonType destinationType = argIn[0].getDataClass();

    ArrayOf A(argIn[0]);
    ArrayOf B(argIn[1]);
    NelsonType complexTypeOrNot = destinationType;
    if ((A.isComplex() || B.isComplex())
        && (destinationType == NLS_DOUBLE || destinationType == NLS_SINGLE)) {
        if (destinationType == NLS_DOUBLE) {
            complexTypeOrNot = NLS_DCOMPLEX;
        } else {
            complexTypeOrNot = NLS_SCOMPLEX;
        }
    } else {
        complexTypeOrNot = destinationType;
    }
    A.promoteType(complexTypeOrNot);
    B.promoteType(complexTypeOrNot);
    retval << GCD(A, B);
    return retval;
}
//=============================================================================
