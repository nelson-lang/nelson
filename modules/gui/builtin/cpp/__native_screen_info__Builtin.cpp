//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__native_screen_info__Builtin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ScreenInfo.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// Test hook: the primary display as the operating system reports it
// (struct with DeviceName, Position = [x y width height] in physical pixels,
// Dpi), or [] where no native query exists. Lets tests check ScreenSize and
// DevicePixelRatio against the real display scale.
ArrayOfVector
Nelson::GuiGateway::__native_screen_info__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    std::wstring deviceName;
    int x = 0;
    int y = 0;
    int width = 0;
    int height = 0;
    double dpi = 96.0;
    ArrayOfVector retval;
    if (!nativePrimaryDisplay(deviceName, x, y, width, height, dpi)) {
        retval << ArrayOf::empty();
        return retval;
    }
    wstringVector fieldnames = { L"DeviceName", L"Position", L"Dpi" };
    ArrayOfVector values;
    values << ArrayOf::characterArray(deviceName);
    values << ArrayOf::doubleRowVector({ static_cast<double>(x), static_cast<double>(y),
        static_cast<double>(width), static_cast<double>(height) });
    values << ArrayOf::doubleScalar(dpi);
    retval << ArrayOf::structScalar(fieldnames, values);
    return retval;
}
//=============================================================================
