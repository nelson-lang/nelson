//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__web_figure_key_event__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "WebFigureBridgeExports.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::__web_figure_key_event__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 8, 8);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].isScalar() || (!argIn[0].isGraphicsObject() && !argIn[0].isNumeric())) {
        Error(L"Nelson:graphics:expectedGraphicsObject:expectedGraphicsObjectOrWebHandle",
            L"Expected graphics object or web graphics handle.");
    }
    const nelson_handle handle = argIn[0].isGraphicsObject()
        ? static_cast<const nelson_handle*>(argIn[0].dataPointer())[0]
        : static_cast<nelson_handle>(argIn[0].asDoubleScalar());
    if (isDeletedGraphicsObject(handle) || handle == HANDLE_ROOT_OBJECT
        || handle >= HANDLE_OFFSET_OBJECT) {
        Error(L"Nelson:graphics:expectedFigure:figureExpected", L"Figure object expected.");
    }
    for (size_t index = 1; index < argIn.size(); ++index) {
        if ((index == 2 || index == 3) ? !argIn[index].isCharacterArray()
                                       : !argIn[index].isLogical()) {
            Error(L"Nelson:graphics:webFigureKeyEventInvalidArguments",
                L"Invalid web figure key event.");
        }
    }
    const std::string character = wstring_to_utf8(argIn[2].asWideString());
    const std::string key = wstring_to_utf8(argIn[3].asWideString());
    // The key callback is queued, not run here: like a native window event it
    // executes at the next drawnow()/pause(), never inside the statement of a
    // running script that drained this command (see __web_flush_callbacks__).
    const bool delivered = nlsWebGuiFigureKeyEvent(handle, argIn[1].asLogicalScalar(),
        character.c_str(), key.c_str(), argIn[4].asLogicalScalar(), argIn[5].asLogicalScalar(),
        argIn[6].asLogicalScalar(), argIn[7].asLogicalScalar());
    ArrayOfVector result;
    result << ArrayOf::logicalScalar(delivered);
    return result;
}
//=============================================================================
