//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "bitshiftBuiltin.hpp"
#include "Error.hpp"
#include "BitwiseShift.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::bitshiftBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isClassType() || argIn[0].isHandle()) {
        OverloadRequired("bitshift");
    }
    std::wstring assumedType;
    bool withAssumedType = false;
    if (argIn.size() > 2) {
        assumedType = argIn[2].asWideString();
        withAssumedType = true;
    }
    retval << Bitshift(argIn[0], argIn[1], assumedType, withAssumedType);
    return retval;
}
//=============================================================================
