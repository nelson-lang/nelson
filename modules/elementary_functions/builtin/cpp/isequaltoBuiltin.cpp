//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isequaltoBuiltin.hpp"
#include "FunctionsInMemory.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "IsEqual.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ClassToString.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static NelsonType
getCommonDataType(NelsonType typeA, NelsonType typeB)
{
    if (typeA == typeB) {
        return typeA;
    }
    if (typeA < typeB) {
        if (typeB == NLS_DCOMPLEX) {
            return NLS_DOUBLE;
        }
        if (typeB == NLS_SCOMPLEX) {
            return NLS_SINGLE;
        }
        return typeB;
    }
    if (typeA == NLS_DCOMPLEX) {
        return NLS_DOUBLE;
    }
    if (typeA == NLS_SCOMPLEX) {
        return NLS_SINGLE;
    }
    return typeA;
}
//=============================================================================
static bool
isObject(NelsonType nelsonType, const std::string& typeName)
{
    return nelsonType == NLS_HANDLE
        || (nelsonType == NLS_STRUCT_ARRAY && typeName != NLS_STRUCT_ARRAY_STR);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::isequaltoBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2);

    NelsonType destinationType = argIn[0].getDataClass();
    std::string destinationTypeName = ClassName(argIn[0]);
    for (size_t k = 1; k < argIn.size(); k++) {
        destinationType = getCommonDataType(destinationType, argIn[k].getDataClass());
        if (destinationType >= NLS_STRUCT_ARRAY) {
            if (argIn[k].getDataClass() == destinationType) {
                destinationTypeName = ClassName(argIn[k]);
            }
        } else {
            destinationTypeName = ClassToString(destinationType);
        }
    }
    FunctionDef* funcDef = nullptr;
    std::string overloadTypeName = overloadFunctionName(destinationTypeName, "isequalto");
    if (!FunctionsInMemory::getInstance()->find(overloadTypeName, funcDef,
            eval->isOverloadAllowed() ? FunctionsInMemory::ALL : FunctionsInMemory::BUILTIN)) {
        Context* context = eval->getContext();
        context->lookupFunction(overloadTypeName, funcDef, !eval->isOverloadAllowed());
    }
    if (!funcDef) {
        Error(_("function") + " " + overloadTypeName + " " + _("undefined."));
    }
    return funcDef->evaluateFunction(eval, argIn, 1);
}
//=============================================================================
static ArrayOfVector
generic_isequaltoBuiltin(const ArrayOfVector& args, NelsonType commonType)
{
    ArrayOfVector retval;
    for (size_t k = 1; k < args.size(); k++) {
        ArrayOf A(args[k - 1]);
        ArrayOf B(args[k]);
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
        bool res = IsEqual(A, B, true, complexTypeOrNot, true);
        if (!res) {
            retval << ArrayOf::logicalConstructor(res);
            return retval;
        }
    }
    retval << ArrayOf::logicalConstructor(true);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::struct_isequaltoBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (ClassName(argIn[0]) != NLS_STRUCT_ARRAY_STR) {
        retval << ArrayOf::logicalConstructor(false);
        return retval;
    }
    FunctionDef* funcDef = nullptr;
    std::string overloadTypeName = "builtin_struct_isequalto";
    if (!FunctionsInMemory::getInstance()->find(overloadTypeName, funcDef)) {
        Context* context = eval->getContext();
        context->lookupFunction(overloadTypeName, funcDef);
    }
    if (!funcDef) {
        Error(_("function") + " " + overloadTypeName + " " + _("undefined."));
    }
    for (size_t k = 1; k < argIn.size(); k++) {
        if (ClassName(argIn[k]) != NLS_STRUCT_ARRAY_STR) {
            retval << ArrayOf::logicalConstructor(false);
            return retval;
        }
        ArrayOfVector args;
        args << argIn[k - 1];
        args << argIn[k];
        ArrayOfVector r = funcDef->evaluateFunction(eval, args, 1);
        bool res = r[0].getContentAsLogicalScalar(false) == 0 ? false : true;
        if (!res) {
            retval << ArrayOf::logicalConstructor(res);
            return retval;
        }
    }
    retval << ArrayOf::logicalConstructor(true);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::cell_isequaltoBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (ClassName(argIn[0]) != NLS_CELL_ARRAY_STR) {
        retval << ArrayOf::logicalConstructor(false);
        return retval;
    }
    FunctionDef* funcDef = nullptr;
    std::string overloadTypeName = "builtin_cell_isequalto";
    if (!FunctionsInMemory::getInstance()->find(overloadTypeName, funcDef)) {
        Context* context = eval->getContext();
        context->lookupFunction(overloadTypeName, funcDef);
    }
    if (!funcDef) {
        Error(_("function") + " " + overloadTypeName + " " + _("undefined."));
    }
    for (size_t k = 1; k < argIn.size(); k++) {
        if (ClassName(argIn[k]) != NLS_CELL_ARRAY_STR) {
            retval << ArrayOf::logicalConstructor(false);
            return retval;
        }
        ArrayOfVector args;
        args << argIn[k - 1];
        args << argIn[k];
        ArrayOfVector r = funcDef->evaluateFunction(eval, args, 1);
        bool res = r[0].getContentAsLogicalScalar(false) == 0 ? false : true;
        if (!res) {
            retval << ArrayOf::logicalConstructor(res);
            return retval;
        }
    }
    retval << ArrayOf::logicalConstructor(true);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::char_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_isequaltoBuiltin(argIn, NLS_CHAR);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::logical_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_isequaltoBuiltin(argIn, NLS_LOGICAL);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::double_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_isequaltoBuiltin(argIn, NLS_DOUBLE);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::single_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_isequaltoBuiltin(argIn, NLS_SINGLE);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::int8_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_isequaltoBuiltin(argIn, NLS_INT8);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::int16_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_isequaltoBuiltin(argIn, NLS_INT16);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::int32_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_isequaltoBuiltin(argIn, NLS_INT32);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::int64_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_isequaltoBuiltin(argIn, NLS_INT64);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::uint8_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_isequaltoBuiltin(argIn, NLS_UINT8);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::uint16_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_isequaltoBuiltin(argIn, NLS_UINT16);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::uint32_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_isequaltoBuiltin(argIn, NLS_UINT32);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::uint64_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_isequaltoBuiltin(argIn, NLS_UINT64);
}
//=============================================================================s
ArrayOfVector
Nelson::ElementaryFunctionsGateway::string_isequaltoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_isequaltoBuiltin(argIn, NLS_STRING_ARRAY);
}
//=============================================================================s
