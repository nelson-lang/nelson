//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "imformatsBuiltin.hpp"
#include "ImageFormats.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
GraphicsIoGateway::imformatsBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;

    if (nLhs == 0 && argIn.size() == 0) {
        imageFormatDisplay(eval);
        return retval;
    }
    if (nLhs == 1 && argIn.size() == 0) {
        return imageSupport(eval);
    }

    if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        return imageSupport(eval, argIn[0].getContentAsWideString());
    } else {
        Error(_W("The input argument must be a string."));
    }
    return retval;
}
//=============================================================================
