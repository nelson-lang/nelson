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
getColonDataType(NelsonType typeA, NelsonType typeB, NelsonType typeC)
{
    NelsonType colonType = getColonDataType(typeA, typeB);
    if (colonType == NLS_UNKNOWN) {
        return colonType;
    }
    return getColonDataType(colonType, typeC);
}
//=============================================================================
static std::string
precedenceTypeNameColon(const ArrayOf& A, const ArrayOf& B, const ArrayOf& C)
{
    NelsonType classA = A.getDataClass();
    NelsonType classB = B.getDataClass();
    NelsonType classC = C.getDataClass();

    bool isObjectA = A.isClassType() || (classA == NLS_HANDLE);
    bool isObjectB = B.isClassType() || (classB == NLS_HANDLE);
    bool isObjectC = C.isClassType() || (classC == NLS_HANDLE);

    if (isObjectA) {
        if (classA == NLS_HANDLE) {
            return A.getHandleCategory();
        } else {
            return A.getClassType();
        }
    } else if (isObjectB) {
        if (classB == NLS_HANDLE) {
            return B.getHandleCategory();
        } else {
            return B.getClassType();
        }
    } else if (isObjectC) {
        if (classC == NLS_HANDLE) {
            return C.getHandleCategory();
        } else {
            return C.getClassType();
        }
    }
    NelsonType commonColonType = getColonDataType(classA, classB, classC);
    if (commonColonType == NLS_UNKNOWN) {
        Error(_W("Colon operands must be all the same type."),
            L"Nelson:colon:mixedNonDoubleOperands");
    }
    return ClassToString(commonColonType);
}
//=============================================================================
static std::string
precedenceTypeNameColon(const ArrayOf& A, const ArrayOf& B)
{
    NelsonType classA = A.getDataClass();
    NelsonType classB = B.getDataClass();

    bool isObjectA = A.isClassType() || (classA == NLS_HANDLE);
    bool isObjectB = B.isClassType() || (classB == NLS_HANDLE);
    if (isObjectA) {
        if (classA == NLS_HANDLE) {
            return A.getHandleCategory();
        } else {
            return A.getClassType();
        }
    } else if (isObjectB) {
        if (classB == NLS_HANDLE) {
            return B.getHandleCategory();
        } else {
            return B.getClassType();
        }
    }

    NelsonType commonColonType = getColonDataType(classA, classB);
    if (commonColonType == NLS_UNKNOWN) {
        Error(_W("Colon operands must be all the same type."),
            L"Nelson:colon:mixedNonDoubleOperands");
    }
    return ClassToString(commonColonType);
}
//=============================================================================
ArrayOf
Evaluator::colonUnitOperator(const ArrayOf& A, const ArrayOf& B)
{
    FunctionDef* funcDef = nullptr;
    std::string typeName = precedenceTypeNameColon(A, B);

    std::string overloadTypeName = typeName + "_" + "colon";
    if (!FunctionsInMemory::getInstance()->find(overloadTypeName, funcDef)) {
        Context* context = this->getContext();
        context->lookupFunction(overloadTypeName, funcDef);
    }
    if (!funcDef) {
        Error(_("function") + " " + overloadTypeName + " " + _("undefined."));
    }
    ArrayOfVector argsIn;
    argsIn << A;
    argsIn << B;
    ArrayOfVector r = funcDef->evaluateFunction(this, argsIn, 1);
    return r[0];
}
//=============================================================================
ArrayOf
Evaluator::colonOperator(const ArrayOf& A, const ArrayOf& B, const ArrayOf& C)
{
    FunctionDef* funcDef = nullptr;
    std::string typeName = precedenceTypeNameColon(A, B, C);

    std::string overloadTypeName = typeName + "_" + "colon";
    if (!FunctionsInMemory::getInstance()->find(overloadTypeName, funcDef)) {
        Context* context = this->getContext();
        context->lookupFunction(overloadTypeName, funcDef);
    }
    if (!funcDef) {
        Error(_W("colon overloading not defined."));
    }
    ArrayOfVector argsIn;
    argsIn << A;
    argsIn << B;
    argsIn << C;
    ArrayOfVector r = funcDef->evaluateFunction(this, argsIn, 1);
    return r[0];
}
//=============================================================================
} // namespace Nelson
//=============================================================================
