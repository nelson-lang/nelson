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
    nargincheck(argIn, 2, 5);
    nargoutcheck(nLhs, 0, 1);
    auto isTextArgument = [](const ArrayOf& arg) {
        return arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar());
    };
    auto getTextArgument = [](const ArrayOf& arg) { return arg.getContentAsWideString(); };
    auto isExtrapolateText = [&](const ArrayOf& arg) {
        return isTextArgument(arg) && getTextArgument(arg) == L"extrap";
    };
    switch (argIn.size()) {
    case 2: {
        // vq = interp1(v, xq)
        retval << LinearInterpolation1D(argIn[0], argIn[1]);
    } break;
    case 3: {
        if (isTextArgument(argIn[2])) {
            // vq = interp1(v, xq, method)
            retval << LinearInterpolation1D(
                argIn[0], argIn[1], getTextArgument(argIn[2]), L"default");
        } else {
            // vq = interp1(x, v, xq)
            retval << LinearInterpolation1D(argIn[0], argIn[1], argIn[2]);
        }
    } break;
    case 4: {
        if (isTextArgument(argIn[2])) {
            // vq = interp1(v, xq, method, extrap)
            const ArrayOf* extrapolationValue = nullptr;
            std::wstring extrapolationMode = L"default";
            if (isExtrapolateText(argIn[3])) {
                extrapolationMode = L"extrap";
            } else {
                extrapolationMode = L"constant";
                extrapolationValue = &argIn[3];
            }
            retval << LinearInterpolation1D(argIn[0], argIn[1], getTextArgument(argIn[2]),
                extrapolationMode, extrapolationValue);
        } else {
            // vq = interp1(x, v, xq, method)
            retval << LinearInterpolation1D(
                argIn[0], argIn[1], argIn[2], getTextArgument(argIn[3]), L"default", nullptr);
        }
    } break;
    case 5: {
        // vq = interp1(x, v, xq, method, extrap)
        const ArrayOf* extrapolationValue = nullptr;
        std::wstring extrapolationMode = L"default";
        if (isExtrapolateText(argIn[4])) {
            extrapolationMode = L"extrap";
        } else {
            extrapolationMode = L"constant";
            extrapolationValue = &argIn[4];
        }
        retval << LinearInterpolation1D(argIn[0], argIn[1], argIn[2], getTextArgument(argIn[3]),
            extrapolationMode, extrapolationValue);
    } break;
    default: {
    } break;
    }
    return retval;
}
//=============================================================================
