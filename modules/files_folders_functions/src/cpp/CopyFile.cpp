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
#include "CopyFile.hpp"
#include "Error.hpp"
#include "IsDirectory.hpp"
#include "IsFile.hpp"
#include "characters_encoding.hpp"
#include <boost/filesystem.hpp>
//=============================================================================
namespace Nelson {
bool
CopyFile(std::wstring srcFile, std::wstring destFileOrDirectory, bool bForce, std::wstring& message)
{
    bool bRes = false;
    message = L"";
    if (!IsFile(srcFile)) {
        Error(_W("File source does not exist."));
    }
    boost::filesystem::path srcPath = srcFile;
    boost::filesystem::path destPath = destFileOrDirectory;
    if (IsDirectory(destFileOrDirectory)) {
        destPath = destPath / srcPath.filename();
    }
    try {
        boost::filesystem::copy_file(
            srcPath, destPath, boost::filesystem::copy_option::overwrite_if_exists);
        bRes = true;
    } catch (const boost::filesystem::filesystem_error& e) {
        bRes = false;
        boost::system::error_code error_code = e.code();
        message = utf8_to_wstring(error_code.message());
    }
    if (bForce) {
        if (!bRes) {
            bRes = true;
            message = L"";
        }
    }
    return bRes;
}
//=============================================================================
bool
CopyDirectory(std::wstring srcDir, std::wstring destDir, bool bForce, std::wstring& message)
{
    bool bRes = false;
    message = L"";
    if (!IsDirectory(srcDir)) {
        Error(_W("Directory source does not exist."));
    }
    if (!IsDirectory(destDir)) {
        Error(_W("Directory destination does not exist."));
    }
    boost::filesystem::path srcPath = srcDir;
    boost::filesystem::path destPath = destDir;
    try {
        boost::filesystem::copy_directory(srcPath, destPath);
        bRes = true;
    } catch (const boost::filesystem::filesystem_error& e) {
        bRes = false;
        boost::system::error_code error_code = e.code();
        message = utf8_to_wstring(error_code.message());
    }
    if (bForce) {
        if (!bRes) {
            bRes = true;
            message = L"";
        }
    }
    return bRes;
}
//=============================================================================
bool
CopyFiles(wstringVector srcFiles, std::wstring destDir, bool bForce, std::wstring& message)
{
    bool bRes = false;
    message = L"";
    for (size_t k = 0; k < srcFiles.size(); k++) {
        if (!IsFile(srcFiles[k])) {
            Error(_W("A cell of existing filenames expected."));
        }
    }
    if (!IsDirectory(destDir)) {
        Error(_W("Directory destination does not exist."));
    }
    for (size_t k = 0; k < srcFiles.size(); k++) {
        boost::filesystem::path srcPath = srcFiles[k];
        boost::filesystem::path destPath = destDir;
        destPath = destPath / srcPath.filename();
        try {
            boost::filesystem::copy_file(srcPath, destPath);
            bRes = true;
        } catch (const boost::filesystem::filesystem_error& e) {
            bRes = false;
            boost::system::error_code error_code = e.code();
            message = utf8_to_wstring(error_code.message());
        }
        if (bForce) {
            if (!bRes) {
                bRes = true;
                message = L"";
            }
        } else {
            if (!bRes) {
                return bRes;
            }
        }
    }
    return bRes;
}
//=============================================================================
}
//=============================================================================
