//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "drawnowBuiltin.hpp"
#include "DrawNow.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GraphicsEnvironment.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "QueueProcessing.hpp"
#include "StringHelpers.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static DrawNowMode
getDrawNowMode(const ArrayOfVector& argIn)
{
    if (argIn.empty()) {
        return DrawNowMode::Immediate;
    }
    const ArrayOf& option = argIn[0];
    if (!option.isRowVectorCharacterArray() && !option.isScalarStringArray()) {
        Error("Nelson:drawnow:unknownOption", "Unknown option.");
    }
    std::wstring optionName = option.asWideString();
    StringHelpers::to_lower(optionName);
    if (optionName == L"limitrate") {
        return DrawNowMode::LimitRate;
    }
    Error("Nelson:drawnow:unknownOption", "Unknown option.");
    return DrawNowMode::Immediate;
}
//=============================================================================
ArrayOfVector
drawnowBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval = {};
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 0);

    GraphicsEnvironment::processGuiEvents();
    processPendingCallbacksAndTimers();
    drawNow(getDrawNowMode(argIn));

    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
