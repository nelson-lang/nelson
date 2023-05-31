//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Colon.hpp"
#include "Evaluator.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadBinaryOperator.hpp"
#include "ClassToString.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
//!
//@Module COLON Index Generation Operator
//@@Section OPERATORS
//@@Usage
// There are two distinct syntaxes for the colon @|:| operator - the two argument form
//@[
//  y = a : c
//@]
// and the three argument form
//@[
//  y = a : b : c
//@]
// The two argument form is exactly equivalent to @|a:1:c|.  The output @|y| is the vector
//\[
//  y = [a,a+b,a+2b,\ldots,a+nb]
//\]
// where @|a+nb <= c|.  There is a third form of the colon operator, the
// no-argument form used in indexing (see @|indexing| for more details).
//@@Examples
// Some simple examples of index generation.
//@<
// y = 1:4
//@>
// Now by half-steps:
//@<
// y = 1:.5:4
//@>
// Now going backwards (negative steps)
//@<
// y = 4:-.5:1
//@>
// If the endpoints are the same, one point is generated, regardless of the step size (middle
// argument)
//@<
// y = 4:1:4
//@>
// If the endpoints define an empty interval, the output is an empty matrix:
//@<
// y = 5:4
//@>
//!
//=============================================================================
ArrayOf
Evaluator::colonOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    ArrayOf retval = this->colonOperator(
        expression(t->down->down), expression(t->down->down->right), expression(t->down->right));
    callstack.popID();
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::colonUnitOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    ArrayOf retval = this->colonUnitOperator(expression(t->down), expression(t->down->right));
    callstack.popID();
    return retval;
}
//=============================================================================
static NelsonType
getColonDataType(NelsonType typeA, NelsonType typeB)
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
static NelsonType
getColonDataType(const std::vector<NelsonType>& typeList)
{
    NelsonType colonType = getColonDataType(typeList[0], typeList[1]);
    if (colonType == NLS_UNKNOWN || typeList.size() == 2) {
        return colonType;
    }
    return getColonDataType(colonType, typeList[2]);
}
//=============================================================================
static std::string
precedenceTypeNameColon(const ArrayOfVector& argsIn, NelsonType& destinationType)
{
    NelsonType classA = argsIn[0].getDataClass();
    NelsonType classB = argsIn[1].getDataClass();
    NelsonType classC = argsIn.size() == 3 ? argsIn[2].getDataClass() : NLS_UNKNOWN;

    bool isObjectA = argsIn[0].isClassStruct() || (classA == NLS_HANDLE);
    bool isObjectB = argsIn[1].isClassStruct() || (classB == NLS_HANDLE);
    bool isObjectC
        = argsIn.size() == 3 ? argsIn[2].isClassStruct() || (classC == NLS_HANDLE) : false;

    if (isObjectA) {
        if (classA == NLS_HANDLE) {
            destinationType = NLS_HANDLE;
            return argsIn[0].getHandleCategory();
        }
        destinationType = NLS_STRUCT_ARRAY;
        return argsIn[0].getStructType();
    }
    if (isObjectB) {
        if (classB == NLS_HANDLE) {
            destinationType = NLS_HANDLE;
            return argsIn[1].getHandleCategory();
        }
        destinationType = NLS_STRUCT_ARRAY;
        return argsIn[1].getStructType();
    }
    if (argsIn.size() == 3 && isObjectC) {
        if (classC == NLS_HANDLE) {
            destinationType = NLS_HANDLE;
            return argsIn[2].getHandleCategory()

                ;
        }
        destinationType = NLS_STRUCT_ARRAY;
        return argsIn[2].getStructType();
    }
    std::vector<NelsonType> typeList;
    typeList.push_back(classA);
    typeList.push_back(classB);
    if (argsIn.size() == 3) {
        typeList.push_back(classC);
    }
    NelsonType commonColonType = getColonDataType(typeList);
    if (commonColonType == NLS_UNKNOWN) {
        Error(_W("Colon operands must be all the same type."),
            L"Nelson:colon:mixedNonDoubleOperands");
    }
    destinationType = commonColonType;
    return ClassToString(commonColonType);
}
//=============================================================================
static bool
isObject(NelsonType nelsonType, const std::string& typeName)
{
    return nelsonType == NLS_HANDLE
        || (nelsonType == NLS_STRUCT_ARRAY && typeName != NLS_STRUCT_ARRAY_STR);
}
//=============================================================================
ArrayOf
colonOperatorImpl(Evaluator* eval, const ArrayOfVector& argsIn)
{
    FunctionDef* funcDef = nullptr;
    NelsonType commonColonType;
    std::string typeName = precedenceTypeNameColon(argsIn, commonColonType);
    std::string overloadTypeName = typeName + "_" + "colon";

    if (!eval->isOverloadAllowed()) {
        Context* context = eval->getContext();
        if (context->lookupFunction(overloadTypeName, funcDef, !eval->isOverloadAllowed())) {
            FunctionsInMemory::getInstance()->add(commonColonType, COLON_OP, funcDef);
        }
    } else {
        if (isObject(commonColonType, typeName)) {
            if (!FunctionsInMemory::getInstance()->find(overloadTypeName, funcDef)) {
                Context* context = eval->getContext();
                context->lookupFunction(overloadTypeName, funcDef);
            }
        } else {
            if (!FunctionsInMemory::getInstance()->findUnaryOperator(
                    commonColonType, COLON_OP, funcDef)) {
                Context* context = eval->getContext();
                if (context->lookupFunction(overloadTypeName, funcDef)) {
                    FunctionsInMemory::getInstance()->add(commonColonType, COLON_OP, funcDef);
                }
            }
        }
    }

    if (!funcDef) {
        Error(_("function") + " " + overloadTypeName + " " + _("undefined."));
    }

    ArrayOfVector r = funcDef->evaluateFunction(eval, argsIn, 1);
    return r[0];
}
//=============================================================================
ArrayOf
Evaluator::colonUnitOperator(const ArrayOf& A, const ArrayOf& B)
{
    ArrayOfVector args;
    args.reserve(2);
    args << A;
    args << B;
    return colonOperatorImpl(this, args);
}
//=============================================================================
ArrayOf
Evaluator::colonOperator(const ArrayOf& A, const ArrayOf& B, const ArrayOf& C)
{
    ArrayOfVector args;
    args.reserve(3);
    args << A;
    args << B;
    args << C;
    return colonOperatorImpl(this, args);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
