//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NotEquals.hpp"
#include "Evaluator.hpp"
#include "Operators.hpp"
#include "RelationOperatorsHelpers.hpp"
//=============================================================================
namespace Nelson {
ArrayOf
Evaluator::neOperator(const ArrayOfVector& args)
{
    return relationalOperator(this, NE_OPERATOR_STR, "~=", true, args, &NotEquals);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
