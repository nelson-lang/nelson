//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
#include "OverloadRequired.hpp"
#include "FindCommonColonType.hpp"
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
ArrayOf
Evaluator::colonUnitOperator(const ArrayOf& A, const ArrayOf& B)
{
    std::string functionName = COLON_OPERATOR_STR;
    std::string commonTypeName = NLS_UNKNOWN_STR;
    NelsonType commonType = NLS_UNKNOWN;
    bool isSparse = false;

    ArrayOfVector argIn;
    argIn.push_back(A);
    argIn.push_back(B);

    ArrayOf res;
    if (findColonCommonType(argIn, commonType, isSparse, commonTypeName)) {
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
            = fmt::sprintf(_("Operator '%s' is not supported for operands of type '%s'."), ":",
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

    std::string commonTypeName = NLS_UNKNOWN_STR;
    NelsonType commonType = NLS_UNKNOWN;
    bool isSparse = false;

    ArrayOfVector argIn;
    argIn.push_back(A);
    argIn.push_back(B);
    argIn.push_back(C);

    if (findColonCommonType(argIn, commonType, isSparse, commonTypeName)) {
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
            = fmt::sprintf(_("Operator '%s' is not supported for operands of type '%s'."), ":",
                ClassToString(commonType));
        Error(msg, "Nelson:UndefinedFunction");
    }
    bool needToOverload;

    ArrayOf _A(argIn[0]);
    ArrayOf _B(argIn[1]);
    ArrayOf _C(argIn[2]);

    if (commonType == NLS_CHAR) {
        _A.promoteType(commonType);
        _B.promoteType(NLS_DOUBLE);
        _C.promoteType(commonType);
    } else {
        _A.promoteType(commonType);
        _B.promoteType(commonType);
        _C.promoteType(commonType);
    }

    res = Colon(_A, _B, _C, needToOverload);
    if (needToOverload) {
        OverloadRequired(functionName);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
