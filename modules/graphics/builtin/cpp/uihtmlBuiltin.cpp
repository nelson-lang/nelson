//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "uihtmlBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "GOHelpers.hpp"
#include "characters_encoding.hpp"
#include "NelSon_engine_mode.h"
#include "NelsonConfiguration.hpp"
#include "UIComponentBuilder.hpp"
#include "GOUIHTML.h"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
uihtmlBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    // Only a desktop able to run an HTML page can host this component. Nothing renders
    // it in the Qt desktop, so raise an error instead of returning a handle that never displays.
    if (NelsonConfiguration::getInstance()->getNelsonEngineMode() != NELSON_ENGINE_MODE::WEB_GUI) {
        Error(L"Nelson:uihtml:webviewDesktopRequired",
            L"uihtml is supported only in the WebView desktop. See help uihtml.");
    }
    ArrayOfVector retval;
    retval << graphicsObjectHandle(UIHtmlBuilder(argIn));
    return retval;
}
//=============================================================================
ArrayOfVector
sendeventtohtmlsourceBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 0);
    if (!argIn[0].isGraphicsObject() || !argIn[0].isScalar()) {
        Error(
            L"Nelson:sendEventToHTMLSource:invalidHandle", L"A scalar uihtml object is expected.");
    }
    GraphicsObject* object = findGraphicsObject(argIn[0].asGraphicsObjectScalar(), false);
    GOUIHTML* component = dynamic_cast<GOUIHTML*>(object);
    if (component == nullptr) {
        Error(
            L"Nelson:sendEventToHTMLSource:invalidHandle", L"A scalar uihtml object is expected.");
    }
    if (!argIn[1].isRowVectorCharacterArray() && !argIn[1].isScalarStringArray()) {
        Error(L"Nelson:sendEventToHTMLSource:invalidEventName",
            L"An event name is expected: character vector or scalar string.");
    }
    const std::string eventName = wstring_to_utf8(argIn[1].asWideString());
    const ArrayOf eventData = (argIn.size() > 2) ? argIn[2] : ArrayOf::empty();
    if (!component->queueEventToPage(eventName, eventData)) {
        Error(L"Nelson:sendEventToHTMLSource:invalidEventName",
            L"An event name is expected: character vector or scalar string.");
    }
    return {};
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
