//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "bitsetBuiltin.hpp"
#include "BitwiseBits.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::bitsetBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 4);
    nargoutcheck(nLhs, 0, 1);
    std::wstring assumedType;
    bool withAssumedType = false;
    ArrayOf V = ArrayOf::doubleScalar(1.0);
    if (argIn.size() == 4) {
        V = argIn[2];
        assumedType = argIn[3].asWideString();
        withAssumedType = true;
    } else if (argIn.size() == 3) {
        // a text third argument is the assumed type, otherwise the bit value
        if (argIn[2].isRowVectorCharacterArray() || argIn[2].isStringArray()) {
            assumedType = argIn[2].asWideString();
            withAssumedType = true;
        } else {
            V = argIn[2];
        }
    }
    ArrayOfVector retval;
    retval << Bitset(argIn[0], argIn[1], V, assumedType, withAssumedType);
    return retval;
}
//=============================================================================
