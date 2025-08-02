//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__interp1__Builtin.hpp"
#include "LinearInterpolation1D.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::__interp1__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 4);
    nargoutcheck(nLhs, 0, 1);
    switch (argIn.size()) {
    case 2: {
        // vq = interp1(v, xq)
        retval << LinearInterpolation1D(argIn[0], argIn[1]);
    } break;
    case 3: {
        if (argIn[2].isRowVectorCharacterArray()
            || (argIn[2].isStringArray() && argIn[2].isScalar())) {
            // vq = interp1(v, xq, 'linear')
            std::wstring methodName = argIn[2].getContentAsWideString();
            if (methodName != L"linear") {
                Error(_W("'linear' method expected."));
            }
            retval << LinearInterpolation1D(argIn[0], argIn[1]);
        } else {
            // vq = interp1(x, v, xq)
            retval << LinearInterpolation1D(argIn[0], argIn[1], argIn[2]);
        }
    } break;
    case 4: {
        // vq = interp1(x, v, xq, 'linear')
        std::wstring methodName = argIn[3].getContentAsWideString();
        if (methodName != L"linear") {
            Error(_W("'linear' method expected."));
        }
        retval << LinearInterpolation1D(argIn[0], argIn[1], argIn[2]);
    } break;
    default: {
    } break;
    }
    return retval;
}
//=============================================================================
