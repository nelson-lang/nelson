//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "regexprepBuiltin.hpp"
#include "RegularExpression.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::regexprepBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return RegexPrepBuiltin(eval, nLhs, argIn);
}
//=============================================================================
