//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "complexBuiltin.hpp"
#include "Error.hpp"
#include "ComplexConstructor.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::complexBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct() || argIn[0].isClassType()) {
        OverloadRequired("complex");
    }
    if (argIn.size() == 1) {
        retval << ComplexConstructor(argIn[0]);
    } else {
        retval << ComplexConstructor(argIn[0], argIn[1]);
    }
    return retval;
}
//=============================================================================
