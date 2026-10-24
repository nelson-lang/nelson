//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__web_uicontrol_action__Builtin.hpp"
#include "GOHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "characters_encoding.hpp"
#include "WebFigureBridgeExports.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// __web_uicontrol_action__(h, value [, text]): applies a web frontend
// interaction to a uicontrol or UI component, as the browser does.
ArrayOfVector
Nelson::GraphicsGateway::__web_uicontrol_action__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].isScalar() || (!argIn[0].isGraphicsObject() && !argIn[0].isNumeric())) {
        Error(L"Nelson:graphics:expectedGraphicsObject:expectedGraphicsObjectOrWebHandle",
            L"Expected graphics object or web graphics handle.");
    }
    const nelson_handle handle = argIn[0].isGraphicsObject()
        ? static_cast<const nelson_handle*>(argIn[0].dataPointer())[0]
        : static_cast<nelson_handle>(argIn[0].asDoubleScalar());
    if (isDeletedGraphicsObject(handle)) {
        Error(L"Nelson:graphics:invalidGraphicsObject", L"Valid graphics object expected.");
    }
    double value = argIn[1].asDoubleScalar();
    std::string text = argIn.size() > 2 ? wstring_to_utf8(argIn[2].asWideString()) : std::string();
    // The control callback is queued, not run here: it executes at the next
    // drawnow()/pause(), never inside the statement of a running script that
    // drained this command (see __web_flush_callbacks__).
    bool applied = nlsWebGuiUIControlAction(static_cast<int64_t>(handle), value, text.c_str());
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(applied);
    return retval;
}
//=============================================================================
