//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QuestionBox.hpp"
#include "GuiDialogProvider.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
QuestionBox(const std::wstring& title, const std::wstring& question, const std::wstring& button1,
    const std::wstring& button2, const std::wstring& button3, const std::wstring& defaultButton,
    int numberOfButtons)
{
    // The question is served by the desktop front end through the registered
    // dialog provider. With no front end (e.g. the headless CLI) there is no
    // place to show it.
    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"question";
        request.title = title;
        request.message = question;
        request.icon = L"question";
        request.buttons.push_back(button1);
        if (numberOfButtons > 1) {
            request.buttons.push_back(button2);
        }
        if (numberOfButtons > 2) {
            request.buttons.push_back(button3);
        }
        request.defaultButton = defaultButton;
        GuiDialogResponse response;
        if (runGuiDialog(request, response)) {
            return ArrayOf::characterArray(response.ok ? response.button : std::wstring());
        }
    }
    Error(L"Nelson:questdlg:noDisplay",
        L"questdlg requires a graphical desktop, which is not available.");
    return ArrayOf::characterArray(std::wstring());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
