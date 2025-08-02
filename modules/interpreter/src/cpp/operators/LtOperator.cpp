//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LessThan.hpp"
#include "Evaluator.hpp"
#include "Operators.hpp"
#include "RelationOperatorsHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::ltOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    ArrayOfVector args;
    args << expression(t->down);
    args << expression(t->down->right);
    ArrayOf retval = this->ltOperator(args);
    callstack.popID();
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::ltOperator(const ArrayOfVector& args)
{
    return relationalOperator(this, LT_OPERATOR_STR, "<", false, args, &LessThan);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
