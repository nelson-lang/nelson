//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "castBuiltin.hpp"
#include "Cast.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::castBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);

    if (argIn.size() == 3) {
        std::wstring like = argIn[1].asWideString();
        if (like != L"like") {
            Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
        }
        retval << Cast(eval, argIn[0], argIn[2]);
    } else {
        retval << Cast(eval, argIn[0], argIn[1].asUtf8String());
    }
    return retval;
}
//=============================================================================
