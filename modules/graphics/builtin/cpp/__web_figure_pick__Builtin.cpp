//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "__web_figure_pick__Builtin.hpp"
#include "WebFigureBridgeExports.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include <string>
//=============================================================================
namespace Nelson::GraphicsGateway {
ArrayOfVector
__web_figure_pick__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 5);
    nargoutcheck(nLhs, 0, 1);
    std::string json;
    const auto sink = [](void* context, const char* text, int length) {
        static_cast<std::string*>(context)->append(text, static_cast<size_t>(length));
    };
    const bool valid = argIn.size() == 5
        ? nlsWebGuiInspectDataPoint(argIn[0].asGraphicsObjectScalar(),
              argIn[3].asGraphicsObjectScalar(), argIn[4].asDoubleScalar(), sink, &json)
        : argIn.size() == 3
            && nlsWebGuiPickDataPoint(argIn[0].asGraphicsObjectScalar(), argIn[1].asDoubleScalar(),
                argIn[2].asDoubleScalar(), sink, &json);
    if (!valid) {
        Error(L"Nelson:graphics:invalidPick", L"Cannot inspect this figure.");
    }
    return { ArrayOf::characterArray(utf8_to_wstring(json)) };
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
