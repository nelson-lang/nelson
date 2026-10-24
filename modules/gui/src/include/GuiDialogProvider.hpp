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
#include "nlsGui_exports.h"
//=============================================================================
// A non-Qt front end (nelson-webview) can serve the GUI dialogs (inputdlg,
// uialert, uiconfirm, etc.) by registering a provider. The dialog functions in
// GuiDialogs consult it first; when handled, they skip the Qt path entirely.
// This keeps the dialog builtins working where no Qt event loop runs.
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
    std::wstring helpString;
    std::wstring helpFcn;
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
};
//=============================================================================
using GuiDialogProviderFn
    = std::function<bool(const GuiDialogRequest& request, GuiDialogResponse& response)>;
//=============================================================================
NLSGUI_IMPEXP void
setGuiDialogProvider(GuiDialogProviderFn provider);
NLSGUI_IMPEXP bool
hasGuiDialogProvider();
// Returns true when the provider handled the dialog (response filled).
NLSGUI_IMPEXP bool
runGuiDialog(const GuiDialogRequest& request, GuiDialogResponse& response);
//=============================================================================
// A C entry point so a module that must not link Qt (nelson-webview's nlsWeb_gui)
// can register the provider after dynamically loading libnlsGui. The context
// pointer is passed back untouched.
//=============================================================================
using GuiDialogProviderRawFn
    = bool (*)(void* context, const GuiDialogRequest& request, GuiDialogResponse& response);
extern "C" NLSGUI_IMPEXP void
nlsGuiRegisterDialogProvider(GuiDialogProviderRawFn provider, void* context);
//=============================================================================
// Runs the OS file chooser for a front end that does not link Qt (it resolves
// this entry point dynamically from libnlsGui). `parent` is a native window
// handle (HWND on Windows) or nullptr. Reads request.fileMode
// ("open"|"save"|"dir"), request.title, request.defaultPath and
// request.fileFilters[0] (Qt filter syntax, e.g. "NFlow (*.nflow);;All Files (*)").
// Fills response.ok (false on cancel) and response.path. Safe to call from an
// RPC worker thread: on Windows the native IFileDialog runs on the calling
// thread; elsewhere QFileDialog is marshalled to the QApplication thread.
// Returns false when the dialog cannot be shown (non-Windows only: no
// QApplication, or its thread does not pump events), so the caller can use its
// own dialog. QApplication is never constructed here; a WebView host must call
// nlsGuiEnsureApplication on its main thread.
using GuiRunFileDialogRawFn
    = bool (*)(void* parent, const GuiDialogRequest& request, GuiDialogResponse& response);
extern "C" NLSGUI_IMPEXP bool
nlsGuiRunFileDialog(void* parent, const GuiDialogRequest& request, GuiDialogResponse& response);
// Initializes the optional Qt application used by native dialogs when Nelson
// is running in desktop WebView mode. Returns false when Qt cannot be started.
extern "C" NLSGUI_IMPEXP bool
nlsGuiEnsureApplication(void);
// Processes pending Qt events from the WebView host's main thread.
extern "C" NLSGUI_IMPEXP void
nlsGuiProcessEvents(void);
//=============================================================================
} // namespace Nelson
//=============================================================================
