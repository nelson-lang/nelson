//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "colonBuiltin.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::colonBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    switch (argIn.size()) {
    case 2: {
        return eval->colonUnitOperator(argIn[0], argIn[1]);
    } break;
    case 3: {
        return eval->colonOperator(argIn[0], argIn[1], argIn[2]);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return {};
}
//=============================================================================
