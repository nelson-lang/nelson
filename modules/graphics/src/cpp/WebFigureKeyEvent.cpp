//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOCallbackProperty.hpp"
#include "GOFigure.hpp"
#include "GOFigureHost.hpp"
#include "GOFiguresManager.hpp"
#include "GOPropertyNames.hpp"
#include "Types.hpp"
#include "WebFigureBridgeExports.hpp"
#include "characters_encoding.hpp"
//=============================================================================
extern "C"
{
    //=============================================================================
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiFigureKeyEvent(int64_t handle, bool pressed, const char* character, const char* key,
        bool shift, bool control, bool alt, bool meta)
    {
        Nelson::GOFigureHost* window = Nelson::getFigure(static_cast<Nelson::int64>(handle));
        if (window == nullptr || window->hostFigure() == nullptr) {
            return false;
        }
        Nelson::GOFigure* figure = window->hostFigure();
        // The figure CurrentCharacter reflects the last character typed, whether
        // or not a callback is set. Non-character keys carry no text and leave
        // it unchanged.
        if (pressed && character != nullptr && character[0] != '\0') {
            Nelson::GOGenericProperty* currentCharacter
                = figure->findProperty(GO_CURRENT_CHARACTER_PROPERTY_NAME_STR, false);
            if (currentCharacter != nullptr) {
                currentCharacter->set(
                    Nelson::ArrayOf::characterArray(Nelson::utf8_to_wstring(character)));
                currentCharacter->clearModified();
            }
        }
        const std::wstring propertyName = pressed ? GO_WINDOW_KEY_PRESS_FCN_PROPERTY_NAME_STR
                                                  : GO_WINDOW_KEY_RELEASE_FCN_PROPERTY_NAME_STR;
        auto* callback
            = dynamic_cast<Nelson::GOCallbackProperty*>(figure->findProperty(propertyName, false));
        if (callback == nullptr || callback->get().isEmpty()) {
            return false;
        }

        Nelson::wstringVector modifiers;
        if (shift) {
            modifiers.push_back(L"shift");
        }
        if (control) {
            modifiers.push_back(L"control");
        }
        if (alt) {
            modifiers.push_back(L"alt");
        }
        if (meta) {
            modifiers.push_back(L"meta");
        }
        callback->pushKeyEvent(figure, L"KeyData", pressed ? L"KeyPress" : L"KeyRelease",
            Nelson::utf8_to_wstring(character != nullptr ? character : ""),
            Nelson::utf8_to_wstring(key != nullptr ? key : ""), modifiers);
        return true;
    }
    //=============================================================================
}
//=============================================================================
