//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <functional>
#include <string>
#include <vector>
#include "nlsNelson_manager_exports.h"
//=============================================================================
// The desktop front end (nelson-webview) serves the GUI dialogs (inputdlg,
// uialert, uiconfirm, questdlg, uigetfile, uiprogressdlg, etc.) by registering a
// provider. The dialog functions and the ProgressDialog graphics object consult
// it; when it handles a request, the response is filled from the web front end.
// This seam carries no toolkit dependency and lives in nelson_manager so that
// both the webview module and the graphics module can reach it.
//=============================================================================
namespace Nelson {
//=============================================================================
struct GuiDialogField
{
    std::wstring prompt;
    std::wstring value;
    int lines = 1;
};
//=============================================================================
struct GuiDialogRequest
{
    std::wstring
        kind; // "input" | "message" | "question" | "preference" | "list" | "file" | "progress"
    std::wstring title;
    std::wstring message; // message / question / prompt text
    std::wstring icon; // info | warning | error | question | success
    std::vector<GuiDialogField> fields; // input fields
    std::vector<std::wstring> buttons; // question buttons
    std::vector<std::wstring> buttonValues; // preference buttons internal values
    std::vector<std::wstring> extraButtons; // buttons that are not persisted
    std::wstring defaultButton;
    std::wstring checkboxText;
    bool checkboxVisible = false;
    bool checkboxState = false;
    std::wstring helpString; // preference: help button label, no button when empty
    // kind == "list": choose from a list of items.
    std::vector<std::wstring> items; // list choices
    bool multiSelect = false;
    std::vector<int> initialSelection; // 1-based indices
    // kind == "file": pick a file / directory.
    std::wstring fileMode; // "open" | "save" | "dir"
    std::vector<std::wstring> fileFilters; // e.g. "*.m", "*.txt"
    std::wstring defaultPath;
    std::wstring suggestedName; // save mode: file name kept separate from its directory
    std::wstring historyKey; // persistent directory key, e.g. "desktop.open"
    // kind == "color": defaultPath carries the initial "#rrggbb".
    // kind == "font": initial font.
    std::wstring fontFamily;
    double fontSize = 0.0;
    bool fontBold = false;
    bool fontItalic = false;
    // kind == "progress": publish the complete state of a non-blocking dialog.
    long long progressId = 0;
    bool progressVisible = true;
    int progressValue = 0;
    int progressMinimum = 0;
    int progressMaximum = 100;
    bool progressIndeterminate = false;
};
//=============================================================================
struct GuiDialogResponse
{
    bool ok = false; // false = cancelled / closed
    std::vector<std::wstring> values; // input field values (kind == input)
    std::wstring button; // pressed button (kind == question/preference)
    bool checkboxChecked = false;
    std::vector<int> selectedIndices; // 1-based selection (kind == list)
    std::wstring path; // chosen path (kind == file)
    bool helpRequested = false; // help button pressed (kind == preference)
};
//=============================================================================
using GuiDialogProviderFn
    = std::function<bool(const GuiDialogRequest& request, GuiDialogResponse& response)>;
//=============================================================================
NLSNELSON_MANAGER_IMPEXP void
setGuiDialogProvider(GuiDialogProviderFn provider);
NLSNELSON_MANAGER_IMPEXP bool
hasGuiDialogProvider();
// Returns true when the provider handled the dialog (response filled).
NLSNELSON_MANAGER_IMPEXP bool
runGuiDialog(const GuiDialogRequest& request, GuiDialogResponse& response);
//=============================================================================
// A C entry point so a host that registers the provider across a module
// boundary can do so after resolving this symbol dynamically. The context
// pointer is passed back untouched.
//=============================================================================
using GuiDialogProviderRawFn
    = bool (*)(void* context, const GuiDialogRequest& request, GuiDialogResponse& response);
extern "C" NLSNELSON_MANAGER_IMPEXP void
nlsGuiRegisterDialogProvider(GuiDialogProviderRawFn provider, void* context);
//=============================================================================
} // namespace Nelson
//=============================================================================
