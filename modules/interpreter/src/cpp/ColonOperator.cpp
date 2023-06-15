//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Evaluator.hpp"
#include "OverloadHelpers.hpp"
#include "FunctionsInMemory.hpp"
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
    ArrayOfVector args;
    args << expression(t->down->down);
    args << expression(t->down->down->right);
    args << expression(t->down->right);
    ArrayOf retval = this->colonOperator(args);
    callstack.popID();
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::colonUnitOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    ArrayOfVector args;
    args << expression(t->down);
    args << expression(t->down->right);
    ArrayOf retval = this->colonOperator(args);
    callstack.popID();
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::colonOperator(const ArrayOfVector& argIn)
{
    FunctionDef* funcDef = nullptr;
    if (!FunctionsInMemory::getInstance()->findUnaryOperator(COLON_OP, funcDef)) {
        Context* context = this->getContext();
        context->lookupFunction(getOperatorName(COLON_OP), funcDef);
        FunctionsInMemory::getInstance()->add(COLON_OP, funcDef);
    }
    if (!funcDef) {
        OverloadRequired(getOperatorName(COLON_OP));
    }
    ArrayOfVector r = funcDef->evaluateFunction(this, argIn, 1);
    return r[0];
}
//=============================================================================
} // namespace Nelson
//=============================================================================
