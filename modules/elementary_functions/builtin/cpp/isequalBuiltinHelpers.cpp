//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isequalBuiltinHelpers.hpp"
#include "FunctionsInMemory.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "IsEqual.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ClassToString.hpp"
#include "ClassName.hpp"
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
static ArrayOfVector
reference_isequalCommonBuiltin(Evaluator* eval, const ArrayOfVector& argIn,
    const std::string& functionName, const std::string& referenceTypeName)
{
    ArrayOfVector retval;
    if (ClassName(argIn[0]) != referenceTypeName) {
        retval << ArrayOf::logicalConstructor(false);
        return retval;
    }
    FunctionDef* funcDef = nullptr;
    std::string overloadTypeName = "__" + referenceTypeName + "_" + functionName;
    if (!FunctionsInMemory::getInstance()->find(overloadTypeName, funcDef)) {
        Context* context = eval->getContext();
        context->lookupFunction(overloadTypeName, funcDef);
    }
    if (!funcDef) {
        Error(_("function") + " " + overloadTypeName + " " + _("undefined."));
    }
    for (size_t k = 1; k < argIn.size(); k++) {
        if (ClassName(argIn[k]) != referenceTypeName) {
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
static ArrayOfVector
generic_isequalCommonBuiltin(const ArrayOfVector& args, const std::string& functionName,
    NelsonType commonType, bool mustBeSameType, bool nanIsSame)
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
        bool res = IsEqual(A, B, mustBeSameType, complexTypeOrNot, nanIsSame);
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
Nelson::ElementaryFunctionsGateway::isequalCommonBuiltin(Evaluator* eval, int nLhs,
    const ArrayOfVector& argIn, const std::string& functionName, bool mustBeSameType,
    bool nanIsSame)
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

    if (destinationType <= NLS_CHAR || (destinationType == NLS_STRING_ARRAY)) {
        return generic_isequalCommonBuiltin(
            argIn, functionName, destinationType, mustBeSameType, nanIsSame);
    }

    return reference_isequalCommonBuiltin(eval, argIn, functionName, destinationTypeName);
}
//=============================================================================
