//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "gcdBuiltin.hpp"
#include "Error.hpp"
#include "GCD.hpp"
#include "OverloadFunction.hpp"
#include "ClassName.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ClassToString.hpp"
#include "ClassName.hpp"
#include "FunctionsInMemory.hpp"
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
Nelson::SpecialFunctionsGateway::gcdBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);

    NelsonType destinationType = argIn[0].getDataClass();
    std::string destinationTypeName = precedenceType(argIn[0], argIn[1], destinationType);

    FunctionDef* funcDef = nullptr;
    std::string overloadTypeName = overloadFunctionName(destinationTypeName, "gcd");
    if (!FunctionsInMemory::getInstance()->find(overloadTypeName, funcDef,
            eval->isOverloadAllowed() ? FunctionsInMemory::ALL : FunctionsInMemory::BUILTIN)) {
        Context* context = eval->getContext();
        context->lookupFunction(overloadTypeName, funcDef, !eval->isOverloadAllowed());
    }
    if (!funcDef) {
        std::wstring msgfmt = _W(
            "Check for incorrect argument data type or missing argument in call to function '%s'.");
        std::wstring msg = fmt::sprintf(msgfmt, L"gcd");
        Error(msg, L"Nelson:UndefinedFunction");
    }
    return funcDef->evaluateFunction(eval, argIn, nLhs);
}
//=============================================================================
static ArrayOfVector
generic_gcdBuiltin(int nLhs, const ArrayOfVector& argIn, NelsonType commonType)
{
    ArrayOfVector retval;
    ArrayOf A(argIn[0]);
    ArrayOf B(argIn[1]);
    NelsonType complexTypeOrNot = commonType;
    if ((A.isComplex() || B.isComplex())
        && (commonType == NLS_DOUBLE || commonType == NLS_SINGLE)) {
        if (commonType == NLS_DOUBLE) {
            complexTypeOrNot = NLS_DCOMPLEX;
        } else {
            complexTypeOrNot = NLS_SCOMPLEX;
        }
    } else {
        complexTypeOrNot = commonType;
    }
    A.promoteType(complexTypeOrNot);
    B.promoteType(complexTypeOrNot);
    retval << GCD(A, B);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::double_gcdBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_gcdBuiltin(nLhs, argIn, NLS_DOUBLE);
}
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::single_gcdBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_gcdBuiltin(nLhs, argIn, NLS_SINGLE);
}
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::logical_gcdBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_gcdBuiltin(nLhs, argIn, NLS_LOGICAL);
}
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::uint8_gcdBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_gcdBuiltin(nLhs, argIn, NLS_UINT8);
}
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::uint16_gcdBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_gcdBuiltin(nLhs, argIn, NLS_UINT16);
}
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::uint32_gcdBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_gcdBuiltin(nLhs, argIn, NLS_UINT32);
}
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::uint64_gcdBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_gcdBuiltin(nLhs, argIn, NLS_UINT64);
}
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::int8_gcdBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_gcdBuiltin(nLhs, argIn, NLS_INT8);
}
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::int16_gcdBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_gcdBuiltin(nLhs, argIn, NLS_INT16);
}
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::int32_gcdBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_gcdBuiltin(nLhs, argIn, NLS_INT32);
}
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::int64_gcdBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_gcdBuiltin(nLhs, argIn, NLS_INT64);
}
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::char_gcdBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_gcdBuiltin(nLhs, argIn, NLS_CHAR);
}
//=============================================================================
