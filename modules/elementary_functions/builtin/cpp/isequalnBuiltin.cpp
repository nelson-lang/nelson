//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isequalnBuiltin.hpp"
#include "isequalBuiltinHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::isequalnBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return isequalCommonBuiltin(eval, nLhs, argIn, "isequaln", false, true);
}
//=============================================================================
