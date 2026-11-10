//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "bitgetBuiltin.hpp"
#include "BitwiseBits.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::bitgetBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    std::wstring assumedType;
    bool withAssumedType = argIn.size() == 3;
    if (withAssumedType) {
        assumedType = argIn[2].asWideString();
    }
    ArrayOfVector retval;
    retval << Bitget(argIn[0], argIn[1], assumedType, withAssumedType);
    return retval;
}
//=============================================================================
