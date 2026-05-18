//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LessEquals.hpp"
#include "Evaluator.hpp"
#include "Operators.hpp"
#include "RelationOperatorsHelpers.hpp"
//=============================================================================
namespace Nelson {
ArrayOf
Evaluator::leOperator(const ArrayOfVector& args)
{
    return relationalOperator(this, LE_OPERATOR_STR, "<=", false, args, &LessEquals);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
