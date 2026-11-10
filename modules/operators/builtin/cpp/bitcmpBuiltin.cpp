//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "bitcmpBuiltin.hpp"
#include "BitwiseOperators.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::bitcmpBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    std::wstring assumedType;
    bool withAssumedType = false;
    if (argIn.size() > 1) {
        assumedType = argIn[1].asWideString();
        withAssumedType = true;
    }
    ArrayOfVector retval;
    retval << Bitcmp(argIn[0], assumedType, withAssumedType);
    return retval;
}
//=============================================================================
