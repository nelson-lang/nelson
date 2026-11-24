//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GuiDialogs.hpp"
#include "GuiDialogProvider.hpp"
#include "GuiDialogsHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
UiAlert(const ArrayOfVector& argIn)
{
    std::wstring message = GuiDialogHelpers::asText(argIn[1]);
    std::wstring title = argIn[2].asWideString();
    std::wstring icon = L"info";
    for (size_t k = 3; k + 1 < argIn.size(); k += 2) {
        if (GuiDialogHelpers::isName(argIn[k], L"Icon")) {
            icon = argIn[k + 1].asWideString();
        }
    }
    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"message";
        request.title = title;
        request.message = message;
        request.icon = icon;
        GuiDialogResponse response;
        if (runGuiDialog(request, response)) {
            return ArrayOf::empty();
        }
    }
    Error(L"Nelson:uialert:noDisplay",
        L"uialert requires a graphical desktop, which is not available.");
    return ArrayOf::empty();
}
//=============================================================================
ArrayOf
UiConfirm(const ArrayOfVector& argIn)
{
    std::wstring message = GuiDialogHelpers::asText(argIn[1]);
    std::wstring title = argIn[2].asWideString();
    wstringVector options = { _W("OK"), _W("Cancel") };
    std::wstring defaultOption = options[0];
    for (size_t k = 3; k + 1 < argIn.size(); k += 2) {
        if (GuiDialogHelpers::isName(argIn[k], L"Options")) {
            options = GuiDialogHelpers::asTextList(argIn[k + 1]);
        } else if (GuiDialogHelpers::isName(argIn[k], L"DefaultOption")) {
            defaultOption = argIn[k + 1].asWideString();
        }
    }
    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"question";
        request.title = title;
        request.message = message;
        request.icon = L"question";
        request.buttons = options;
        request.defaultButton = defaultOption;
        GuiDialogResponse response;
        if (runGuiDialog(request, response)) {
            return ArrayOf::characterArray(response.ok ? response.button : std::wstring());
        }
    }
    Error(L"Nelson:uiconfirm:noDisplay",
        L"uiconfirm requires a graphical desktop, which is not available.");
    return ArrayOf::characterArray(std::wstring());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
