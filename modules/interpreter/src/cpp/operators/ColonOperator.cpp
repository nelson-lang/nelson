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
#include "Colon.hpp"
#include "Evaluator.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ClassToString.hpp"
#include "Operators.hpp"
#include "NelsonConfiguration.hpp"
#include "OverloadHelpers.hpp"
#include "FunctionsInMemory.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
findColonCommonTypeName(
    const ArrayOfVector& argIn, NelsonType& commonType, bool& isSparse, std::string& typeName);
//=============================================================================
static NelsonType
getColonCommonBasicType(const ArrayOf& A, const ArrayOf& B);
static NelsonType
getColonCommonType(const ArrayOf& A, const ArrayOf& B, const ArrayOf& C);
static NelsonType
getColonCommonType(NelsonType typeA, NelsonType typeB);
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
ArrayOf
Evaluator::colonUnitOperator(const ArrayOf& A, const ArrayOf& B)
{
    std::string functionName = COLON_OPERATOR_STR;
    std::string commonTypeName;
    NelsonType commonType;
    bool isSparse;

    ArrayOfVector argIn;
    argIn.push_back(A);
    argIn.push_back(B);

    ArrayOf res;
    if (findColonCommonTypeName(argIn, commonType, isSparse, commonTypeName)) {
        bool overloadWasFound = false;
        res = callOverloadedFunction(this,
            NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), argIn,
            functionName, commonTypeName, commonType, overloadWasFound);
        if (overloadWasFound) {
            return res;
        }
    }
    if (commonType == NLS_UNKNOWN || commonType == NLS_STRUCT_ARRAY
        || commonType == NLS_CELL_ARRAY) {
        std::string msg
            = fmt::sprintf(_("Operator ':' is not supported for operands of type '%s'."),
                ClassToString(commonType));
        Error(msg, "Nelson:UndefinedFunction");
    }
    ArrayOf _A(A);
    ArrayOf _B(B);
    _A.promoteType(commonType);
    _B.promoteType(commonType);

    bool needToOverload;
    res = Colon(_A, _B, needToOverload);
    if (needToOverload) {
        OverloadRequired(functionName);
    }
    return res;
}
//=============================================================================
ArrayOf
Evaluator::colonOperator(const ArrayOf& A, const ArrayOf& B, const ArrayOf& C)
{
    ArrayOf res;
    std::string functionName = COLON_OPERATOR_STR;

    std::string commonTypeName;
    NelsonType commonType;
    bool isSparse;

    ArrayOfVector argIn;
    argIn.push_back(A);
    argIn.push_back(B);
    argIn.push_back(C);

    if (findColonCommonTypeName(argIn, commonType, isSparse, commonTypeName)) {
        bool overloadWasFound = false;
        res = callOverloadedFunction(this,
            NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), argIn,
            functionName, commonTypeName, commonType, overloadWasFound);
        if (overloadWasFound) {
            return res;
        }
    }
    if (commonType == NLS_UNKNOWN || commonType == NLS_STRUCT_ARRAY
        || commonType == NLS_CELL_ARRAY) {
        std::string msg
            = fmt::sprintf(_("Operator ':' is not supported for operands of type '%s'."),
                ClassToString(commonType));
        Error(msg, "Nelson:UndefinedFunction");
    }
    bool needToOverload;
    ArrayOf _A(argIn[0]);
    ArrayOf _B(argIn[1]);
    ArrayOf _C(argIn[2]);
    _A.promoteType(commonType);
    _B.promoteType(commonType);
    _C.promoteType(commonType);

    res = Colon(_A, _B, _C, needToOverload);
    if (needToOverload) {
        OverloadRequired(functionName);
    }
    return res;
}
//=============================================================================
bool
findColonCommonTypeName(
    const ArrayOfVector& argIn, NelsonType& commonType, bool& isSparse, std::string& typeName)
{
    isSparse = false;
    commonType = NLS_UNKNOWN;

    const size_t numArgs = argIn.size();
    if (numArgs == 0) {
        return false; // Nothing to process, return false
    }

    isSparse = argIn[0].isSparse();
    for (int i = 1; i < numArgs; ++i) {
        isSparse = isSparse || argIn[i].isSparse();
    }

    bool isMixed = false;
    commonType = argIn[0].getDataClass();
    for (int i = 1; i < numArgs; ++i) {
        NelsonType argType = argIn[i].getDataClass();
        if (commonType != argType) {
            isMixed = true;
            break;
        }
    }

    if (!isMixed) {
        typeName = ClassToString(commonType);
        return true;
    }

    bool hasReference = false;
    for (int i = 0; i < numArgs; ++i) {
        NelsonType argType = argIn[i].getDataClass();
        if (argType > NLS_CHAR) {
            hasReference = true;
            if (argType == NLS_STRING_ARRAY || argType == NLS_CELL_ARRAY) {
                commonType = argType;
                typeName
                    = (argType == NLS_STRING_ARRAY) ? NLS_STRING_ARRAY_STR : NLS_CELL_ARRAY_STR;
                return true;
            } else if (argType == NLS_CLASS_ARRAY) {
                commonType = NLS_CLASS_ARRAY;
                typeName = argIn[i].getClassType();
                return true;
            } else if (argType == NLS_STRUCT_ARRAY) {
                commonType = NLS_STRUCT_ARRAY;
                typeName = NLS_STRUCT_ARRAY_STR;
                return true;
            } else if (argType == NLS_HANDLE) {
                commonType = NLS_HANDLE;
                typeName = argIn[i].getHandleCategory();
                return true;
            } else if (argType == NLS_GO_HANDLE) {
                commonType = NLS_GO_HANDLE;
                typeName = NLS_GO_HANDLE_STR;
                return true;
            }
        }
    }

    if (!hasReference) {
        commonType = getColonCommonBasicType(argIn[0], argIn[1]);
        if (numArgs > 2) {
            NelsonType commonType2 = getColonCommonBasicType(argIn[1], argIn[2]);
            commonType = getColonCommonType(commonType, commonType2);
        }
    }

    typeName = ClassToString(commonType);
    if (isSparse) {
        typeName = NLS_SPARSE_STR + typeName;
    }

    return commonType != NLS_UNKNOWN;
}
//=============================================================================
static NelsonType
getColonCommonType(NelsonType typeA, NelsonType typeB)
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
getColonCommonBasicType(const ArrayOf& A, const ArrayOf& B)
{
    NelsonType commonType = NLS_UNKNOWN;
    NelsonType classA = A.getDataClass();
    NelsonType classB = B.getDataClass();

    if (classA == NLS_CHAR || classB == NLS_CHAR) {
        Error(_W("For colon operator with char operands, first and last operands must be char."),
            L"Nelson:colon:mixedCharOperands");
    }

    if (IS_INTEGER_TYPE(classA) || IS_INTEGER_TYPE(classB)) {
        bool isIntegerA = IS_INTEGER_TYPE(classA) || (A.isDoubleClass() && A.isIntegerValue());
        bool isIntegerB = IS_INTEGER_TYPE(classB) || (B.isDoubleClass() && B.isIntegerValue());
        bool isSupportedMixedInteger = false;
        if (IS_INTEGER_TYPE(classA) && IS_INTEGER_TYPE(classB)) {
            isSupportedMixedInteger = (classA == classB);
        } else {
            isSupportedMixedInteger = (isIntegerA && isIntegerB);
        }

        if (!isSupportedMixedInteger && ((IS_INTEGER_TYPE(classA) || IS_INTEGER_TYPE(classB)))) {
            Error(_W("Colon operands must be all the same type, or mixed with real double "
                     "scalar."),
                L"Nelson:colon:mixedNonDoubleOperands");
        }
    }
    return getColonCommonType(classA, classB);
}
//=============================================================================
NelsonType
getColonCommonType(const ArrayOf& A, const ArrayOf& B, const ArrayOf& C)
{
    NelsonType classA = A.getDataClass();
    NelsonType classB = B.getDataClass();
    NelsonType classC = C.getDataClass();

    if (classA == NLS_CHAR) {
        if ((classB != NLS_DOUBLE) && (classB != NLS_DCOMPLEX) && (classB != NLS_CHAR)) {
            Error(_W("For colon operator with char operands, second operand must be char or real "
                     "scalar double."),
                L"Nelson:colon:mixedCharOperand");
        }
        if (classC != NLS_CHAR) {
            Error(_W("For colon operator with char operands, first and last operands must be "
                     "char."),
                L"Nelson:colon:mixedCharOperand");
        }
    }

    bool isSupportedMixedInteger = false;
    if (IS_INTEGER_TYPE(classA) && IS_INTEGER_TYPE(classB) && IS_INTEGER_TYPE(classC)) {
        isSupportedMixedInteger = (classA == classB) && (classA == classC);
    } else if (IS_INTEGER_TYPE(classA) && B.isDoubleClass() && IS_INTEGER_TYPE(classC)) {
        isSupportedMixedInteger = (classA == classC);
    } else if (IS_INTEGER_TYPE(classA) && IS_INTEGER_TYPE(classB) && C.isDoubleClass()) {
        isSupportedMixedInteger = (classA == classB);
    } else if (IS_INTEGER_TYPE(classA) && B.isDoubleClass() && C.isDoubleClass()) {
        isSupportedMixedInteger = true;
    } else if (A.isDoubleClass() && IS_INTEGER_TYPE(classB) && C.isDoubleClass()) {
        isSupportedMixedInteger = true;
    } else if (A.isDoubleClass() && B.isDoubleClass() && C.isDoubleClass()) {
        isSupportedMixedInteger = true;
    } else if (A.isDoubleClass() && B.isDoubleClass() && IS_INTEGER_TYPE(classC)) {
        isSupportedMixedInteger = true;
    }

    if (!isSupportedMixedInteger
        && (IS_INTEGER_TYPE(classA) || IS_INTEGER_TYPE(classB) || IS_INTEGER_TYPE(classC))) {
        Error(_W("Colon operands must be all the same type, or mixed with real double scalar."),
            L"Nelson:colon:mixedNonDoubleOperands");
    }
    return getColonCommonType(getColonCommonType(classA, classB), classC);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
