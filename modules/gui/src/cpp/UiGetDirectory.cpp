//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
UiGetDirectory(std::wstring pathOrigin, std::wstring title, std::wstring& pathSelected)
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
            fd = new QFileDialog(0, wstringToQString(title));
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
            if (!bRes) {
                boost::filesystem::path pwd = boost::filesystem::current_path();
                pathOrigin = pwd.generic_wstring();
            }
            fd->setDirectory(QDir(wstringToQString(pathOrigin)));
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
}
//=============================================================================
