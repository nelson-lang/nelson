//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "UiGetDirectory.hpp"
#include "QStringConverter.hpp"
#include <QtCore/QDir>
#include <QtWidgets/QFileDialog>
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
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
            boost::filesystem::path data_dir(pathOrigin);
            bool bRes = false;
            try {
                bRes = boost::filesystem::is_directory(data_dir);
            } catch (const boost::filesystem::filesystem_error& e) {
                if (e.code() == boost::system::errc::permission_denied) {
                    // ONLY FOR DEBUG
                }
                bRes = false;
            }
            std::wstring _pathOrigin = pathOrigin;
            if (!bRes) {
                boost::filesystem::path pwd = boost::filesystem::current_path();
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
