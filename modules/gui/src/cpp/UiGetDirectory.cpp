//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QDir>
#include <QtWidgets/QFileDialog>
#include "FileSystemHelpers.hpp"
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
            std::filesystem::path data_dir = createFileSystemPath(pathOrigin);
            bool bRes = false;
            try {
                bRes = std::filesystem::is_directory(data_dir);
            } catch (const std::filesystem::filesystem_error& e) {
                if (e.code() == std::errc::permission_denied) {
                    // ONLY FOR DEBUG
                }
                bRes = false;
            }
            std::wstring _pathOrigin = pathOrigin;
            if (!bRes) {
                std::filesystem::path pwd = std::filesystem::current_path();
                _pathOrigin = convertFileSytemPathToGenericWString(pwd);
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
