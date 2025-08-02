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
#include "FileSystemWrapper.hpp"
#include "UiGetDirectory.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
UiGetDirectory(
    const std::wstring& pathOrigin, const std::wstring& title, std::wstring& pathSelected)
{
    bool bCancelled = false;
    QFileDialog* fd = nullptr;
    if (title.empty()) {
        try {
            fd = new QFileDialog;
        } catch (const std::bad_alloc&) {
            fd = nullptr;
        }
    } else {
        try {
            fd = new QFileDialog(nullptr, wstringToQString(title));
        } catch (const std::bad_alloc&) {
            fd = nullptr;
        }
    }
    if (fd) {
        if (!pathOrigin.empty()) {
            bool bRes = FileSystemWrapper::Path::is_directory(pathOrigin);
            std::wstring _pathOrigin = pathOrigin;
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
        if (resexec == QDialog::Accepted) {
            QString directory = fd->selectedFiles()[0];
            bCancelled = false;
            pathSelected = QStringTowstring(directory);
        } else {
            bCancelled = true;
        }
    }
    return bCancelled;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
