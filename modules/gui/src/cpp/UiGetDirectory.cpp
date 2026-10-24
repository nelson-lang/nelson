//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QDir>
#include <QtWidgets/QFileDialog>
#include <memory>
#include "FileSystemWrapper.hpp"
#ifdef _MSC_VER
#include "ForceWindowsTitleBarToDark.hpp"
#endif
#include "UiGetDirectory.hpp"
#include "GuiDialogProvider.hpp"
#include "i18n.hpp"
#include "GuiDialogsHelpers.hpp"
#include "QStringConverter.hpp"
#include "DialogDirectories.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
UiGetDirectory(
    const std::wstring& pathOrigin, const std::wstring& title, std::wstring& pathSelected)
{
    const std::wstring historyKey = L"desktop.directory";
    const std::wstring initialDirectory
        = resolveDialogDirectory(historyKey, pathOrigin, QDir::currentPath().toStdWString());
    // Non-Qt front end (web): serve the directory picker through the provider.
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
    // Defaults to "cancelled": if the dialog cannot be created, no directory
    // has been selected and the caller must not use pathSelected.
    bool bCancelled = true;
    std::unique_ptr<QFileDialog> fd;
    QWidget* parent = GuiDialogHelpers::mainWindowParent();
    if (title.empty()) {
        try {
            fd = std::make_unique<QFileDialog>(parent);
        } catch (const std::bad_alloc&) {
            fd.reset();
        }
    } else {
        try {
            fd = std::make_unique<QFileDialog>(parent, wstringToQString(title));
        } catch (const std::bad_alloc&) {
            fd.reset();
        }
    }
    if (fd) {
#ifdef _MSC_VER
        forceWindowsTitleBarToDark(fd->winId());
#endif
        if (!initialDirectory.empty()) {
            bool bRes = FileSystemWrapper::Path::is_directory(initialDirectory);
            std::wstring _pathOrigin = initialDirectory;
            if (!bRes) {
                FileSystemWrapper::Path pwd = FileSystemWrapper::Path::current_path();
                _pathOrigin = pwd.generic_wstring();
            }
            fd->setDirectory(QDir(wstringToQString(_pathOrigin)));
        }
        fd->setFileMode(QFileDialog::Directory);
        fd->setOption(QFileDialog::ShowDirsOnly);
        fd->setViewMode(QFileDialog::Detail);
        int resexec = fd->exec();
        if (resexec == QDialog::Accepted && !fd->selectedFiles().isEmpty()) {
            QString directory = fd->selectedFiles()[0];
            bCancelled = false;
            pathSelected = QStringTowstring(directory);
            setDialogDirectory(historyKey, pathSelected);
        } else {
            bCancelled = true;
        }
    }
    return bCancelled;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
