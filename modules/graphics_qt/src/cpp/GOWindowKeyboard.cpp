//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOWindow.hpp"
#include "FigureWorkspaceQt.hpp"
#include "GOCallbackProperty.hpp"
#include "GOPropertyNames.hpp"
#include "QStringConverter.hpp"
#include <QtGui/QKeyEvent>
#include <QtGui/QKeySequence>
//=============================================================================
namespace Nelson {
void
GOWindow::keyReleaseEvent(QKeyEvent* event)
{
    event->accept();
    this->getGOFigure()->setRenderingStateInvalid(true);
    if (keyReleaseState.deferRelease(*event)) {
        return;
    }
    handleKeyEvent(event, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR, L"KeyRelease");
}
//=============================================================================
void
GOWindow::keyPressEvent(QKeyEvent* event)
{
    event->accept();

    // The figure CurrentCharacter reflects the last character typed, whether
    // or not a callback is set. Non-character keys (arrows, modifiers) leave it
    // unchanged, as they carry no text.
    std::wstring typedCharacter = getCharacterString(event);
    if (!typedCharacter.empty()) {
        GOGenericProperty* currentCharacter
            = goFig->findProperty(GO_CURRENT_CHARACTER_PROPERTY_NAME_STR, false);
        if (currentCharacter != nullptr) {
            currentCharacter->set(ArrayOf::characterArray(typedCharacter));
            currentCharacter->clearModified();
        }
    }

    keyReleaseState.press();
    releaseKeyTimer.start(delayMs);

    handleKeyEvent(event, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR, L"KeyPress");
    if (waitKeyOrMousePressedMode == WAIT_PRESS_MODE::WAIT) {
        waitKeyOrMousePressedMode = WAIT_PRESS_MODE::KEY;
        return;
    }

    if (workspace) {
        workspace->shortcut(event);
    }
}
//=============================================================================
void
GOWindow::onKeyReleaseTimeout()
{
    std::unique_ptr<QKeyEvent> event = keyReleaseState.takePendingRelease();
    if (!event) {
        return;
    }
    handleKeyEvent(event.get(), GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR, L"KeyRelease");
}
//=============================================================================
bool
GOWindow::handleKeyEvent(
    QEvent* event, const std::wstring& callbackPropertyStr, const std::wstring& eventType)
{
    GOCallbackProperty* goCallback = (GOCallbackProperty*)goFig->findProperty(callbackPropertyStr);
    ArrayOf callbackArrayOf = goCallback->get();
    if (callbackArrayOf.isEmpty()) {
        return true;
    }

    QKeyEvent* keyEvent = static_cast<QKeyEvent*>(event);
    wstringVector modifiersWstringVector = getModifiers(keyEvent);
    std::wstring key = getKeyString(keyEvent);
    std::wstring character = getCharacterString(keyEvent);

    goCallback->pushKeyEvent(goFig, L"KeyData", eventType, character, key, modifiersWstringVector);
    return true;
}
//=============================================================================
wstringVector
GOWindow::getModifiers(QKeyEvent* keyEvent)
{
    wstringVector modifiersWstringVector;
    modifiersWstringVector.reserve(4);
    Qt::KeyboardModifiers modifiers = keyEvent->modifiers();
    if (modifiers & Qt::ShiftModifier) {
        modifiersWstringVector.push_back(L"shift");
    }
    if (modifiers & Qt::ControlModifier) {
        modifiersWstringVector.push_back(L"control");
    }
    if (modifiers & Qt::AltModifier) {
        modifiersWstringVector.push_back(L"alt");
    }
    if (modifiers & Qt::MetaModifier) {
        modifiersWstringVector.push_back(L"meta");
    }
    return modifiersWstringVector;
}
//=============================================================================
std::wstring
GOWindow::getKeyString(QKeyEvent* keyEvent)
{
    return QStringTowstring(QKeySequence(keyEvent->key()).toString());
}
//=============================================================================
std::wstring
GOWindow::getCharacterString(QKeyEvent* keyEvent)
{
    return QStringTowstring(keyEvent->text());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
