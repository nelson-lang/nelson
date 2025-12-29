//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GreaterThan.hpp"
#include "Evaluator.hpp"
#include "Operators.hpp"
#include "RelationOperatorsHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::gtOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    ArrayOfVector args;
    args.reserve(2);
    args.push_back(expression(t->down));
    args.push_back(expression(t->down->right));
    ArrayOf retval = this->gtOperator(args);
    callstack.popID();
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::gtOperator(const ArrayOfVector& args)
{
    return relationalOperator(this, GT_OPERATOR_STR, ">", false, args, &GreaterThan);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
