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
precedenceTypeNameColon(
    const ArrayOf& A, const ArrayOf& B, const ArrayOf& C, NelsonType& destinationType)
{
    NelsonType classA = A.getDataClass();
    NelsonType classB = B.getDataClass();
    NelsonType classC = C.getDataClass();

    bool isObjectA = A.isClassStruct() || (classA == NLS_HANDLE);
    bool isObjectB = B.isClassStruct() || (classB == NLS_HANDLE);
    bool isObjectC = C.isClassStruct() || (classC == NLS_HANDLE);

    if (isObjectA) {
        if (classA == NLS_HANDLE) {
            destinationType = NLS_HANDLE;
            return wstring_to_utf8(A.getHandleCategory());
        } else {
            destinationType = NLS_STRUCT_ARRAY;
            return A.getStructType();
        }
    } else if (isObjectB) {
        if (classB == NLS_HANDLE) {
            destinationType = NLS_HANDLE;
            return wstring_to_utf8(B.getHandleCategory());
        } else {
            destinationType = NLS_STRUCT_ARRAY;
            return B.getStructType();
        }
    } else if (isObjectC) {
        if (classC == NLS_HANDLE) {
            destinationType = NLS_HANDLE;
            return wstring_to_utf8(C.getHandleCategory());
        } else {
            destinationType = NLS_STRUCT_ARRAY;
            return C.getStructType();
        }
    }
    NelsonType commonColonType = getColonDataType(classA, classB, classC);
    if (commonColonType == NLS_UNKNOWN) {
        Error(_W("Colon operands must be all the same type."),
            L"Nelson:colon:mixedNonDoubleOperands");
    }
    destinationType = commonColonType;
    return ClassToString(commonColonType);
}
//=============================================================================
static std::string
precedenceTypeNameColon(const ArrayOf& A, const ArrayOf& B, NelsonType& destinationType)
{
    NelsonType classA = A.getDataClass();
    NelsonType classB = B.getDataClass();

    bool isObjectA = A.isClassStruct() || (classA == NLS_HANDLE);
    bool isObjectB = B.isClassStruct() || (classB == NLS_HANDLE);
    if (isObjectA) {
        if (classA == NLS_HANDLE) {
            destinationType = NLS_HANDLE;
            return wstring_to_utf8(A.getHandleCategory());
        } else {
            destinationType = NLS_STRUCT_ARRAY;
            return A.getStructType();
        }
    } else if (isObjectB) {
        if (classB == NLS_HANDLE) {
            destinationType = NLS_HANDLE;
            return wstring_to_utf8(B.getHandleCategory());
        } else {
            destinationType = NLS_STRUCT_ARRAY;
            return B.getStructType();
        }
    }

    NelsonType commonColonType = getColonDataType(classA, classB);
    if (commonColonType == NLS_UNKNOWN) {
        Error(_W("Colon operands must be all the same type."),
            L"Nelson:colon:mixedNonDoubleOperands");
    }
    destinationType = commonColonType;
    return ClassToString(commonColonType);
}
//=============================================================================
ArrayOf
Evaluator::colonUnitOperator(const ArrayOf& A, const ArrayOf& B)
{
    FunctionDef* funcDef = nullptr;
    NelsonType commonColonType;
    std::string typeName = precedenceTypeNameColon(A, B, commonColonType);

    std::string overloadTypeName = typeName + "_" + "colon";

    FunctionsInMemory::FIND_FUNCTION_TYPE functionType
        = isOverloadAllowed() ? FunctionsInMemory::ALL : FunctionsInMemory::BUILTIN;

    if (!FunctionsInMemory::getInstance()->find(overloadTypeName, funcDef, functionType)) {
        Context* context = this->getContext();
        context->lookupFunction(overloadTypeName, funcDef, !isOverloadAllowed());
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
    NelsonType commonColonType;
    std::string typeName = precedenceTypeNameColon(A, B, C, commonColonType);

    FunctionsInMemory::FIND_FUNCTION_TYPE functionType
        = isOverloadAllowed() ? FunctionsInMemory::ALL : FunctionsInMemory::BUILTIN;

    std::string overloadTypeName = typeName + "_" + "colon";
    if (!FunctionsInMemory::getInstance()->find(overloadTypeName, funcDef, functionType)) {
        Context* context = this->getContext();
        context->lookupFunction(overloadTypeName, funcDef, !isOverloadAllowed());
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
