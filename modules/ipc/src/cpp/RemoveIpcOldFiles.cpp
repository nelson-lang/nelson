//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/interprocess/detail/shared_dir_helpers.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/operations.hpp>
#include "RemoveIpcOldFiles.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
RemoveIpcOldFiles()
{
    bool result = false;
    std::string ipcDirectory;
    boost::interprocess::ipcdetail::get_shared_dir(ipcDirectory);
    boost::filesystem::path branch(ipcDirectory);
    bool isDirectory;
    try {
        isDirectory = boost::filesystem::is_directory(branch);
    } catch (const boost::filesystem::filesystem_error&) {
        isDirectory = false;
    }
    if (isDirectory) {
        for (boost::filesystem::directory_iterator p(branch), end; p != end; ++p) {
            boost::filesystem::path filepath = p->path();
            std::wstring filename = filepath.leaf().wstring();
            if (boost::algorithm::starts_with(filename, L"NELSON_COMMAND_INTERPROCESS_")
                || boost::algorithm::starts_with(filename, L"NELSON_COMMAND_FILE_EXTENSION_")) {
                try {
                    result = true;
                    boost::filesystem::remove(filepath);
                } catch (const boost::filesystem::filesystem_error&) {
                    result = false;
                }
            }
        }
    }
    return result;
}
//=============================================================================
}
//=============================================================================
