//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "__web_graphics_handle__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GOHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <cmath>
//=============================================================================
namespace Nelson::GraphicsGateway {
ArrayOfVector
__web_graphics_handle__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    const std::wstring mode = argIn[1].asWideString();
    if (mode == L"number" && argIn[0].isGraphicsObject()) {
        return { ArrayOf::doubleScalar(static_cast<double>(argIn[0].asGraphicsObjectScalar())) };
    }
    if (mode == L"object" && argIn[0].isNumeric() && argIn[0].isScalar()) {
        const double number = argIn[0].asDoubleScalar();
        if (std::isfinite(number) && number >= 0 && number <= 9007199254740991.0
            && std::floor(number) == number) {
            return { graphicsObjectHandle(static_cast<int64>(number)) };
        }
    }
    Error(L"Nelson:graphics:invalidWebHandle", L"Invalid web graphics handle conversion.");
    return {};
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
