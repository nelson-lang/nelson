//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <filesystem>
#include "UiGetDirectory.hpp"
#include "GuiDialogProvider.hpp"
#include "DialogDirectories.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
UiGetDirectory(
    const std::wstring& pathOrigin, const std::wstring& title, std::wstring& pathSelected)
{
    const std::wstring historyKey = L"desktop.directory";
    std::error_code ec;
    const std::wstring currentPath = std::filesystem::current_path(ec).wstring();
    const std::wstring initialDirectory
        = resolveDialogDirectory(historyKey, pathOrigin, currentPath);
    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"file";
        request.title = title.empty() ? _W("Select a directory") : title;
        request.fileMode = L"dir";
        request.defaultPath = initialDirectory;
        request.historyKey = historyKey;
        GuiDialogResponse response;
        if (runGuiDialog(request, response)) {
            if (!response.ok || response.path.empty()) {
                return true; // cancelled
            }
            pathSelected = response.path;
            setDialogDirectory(historyKey, pathSelected);
            return false;
        }
    }
    // No desktop front end: nothing was selected.
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
