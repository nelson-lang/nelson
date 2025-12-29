//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Evaluator.hpp"
#include "Operators.hpp"
#include "Equals.hpp"
#include "RelationOperatorsHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::eqOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    ArrayOfVector args;
    args.reserve(2);
    args.push_back(expression(t->down));
    args.push_back(expression(t->down->right));
    ArrayOf retval = this->eqOperator(args);
    callstack.popID();
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::eqOperator(const ArrayOfVector& args)
{
    return relationalOperator(this, EQ_OPERATOR_STR, "==", true, args, &Equals);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
