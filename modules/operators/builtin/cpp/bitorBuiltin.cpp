//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "bitorBuiltin.hpp"
#include "BitwiseOperators.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::bitorBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 1);
    std::wstring assumedType = L"";
    bool withAssumedType = false;
    if (argIn.size() > 2) {
        assumedType = argIn[2].getContentAsWideString();
        withAssumedType = true;
    }
    return BitwiseOperator(
        BITWISE_OPERATOR::BIT_OR, argIn[0], argIn[1], assumedType, withAssumedType);
}
//=============================================================================
