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
#pragma once
//=============================================================================
#include "File.hpp"
#include "Interface.hpp"
#include "Types.hpp"
#include "nlsStream_manager_exports.h"
#include <boost/container/vector.hpp>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSSTREAM_MANAGER_IMPEXP FilesManager
{
private:
    boost::container::vector<File*> userFiles;
    int
    getAvailableFileID();

public:
    FilesManager(Interface* io = nullptr);
    ~FilesManager();
    bool
    isOpened(std::wstring filenameToSearch);
    bool
    isOpened(int fileID);
    bool
    isStdIn(int fileID);
    bool
    isStdOut(int fileID);
    bool
    isStdErr(int fileID);
    bool
    isStdStream(int fileID);
    int
    addFile(File* userfile);

    // file also closed when you removeFile
    bool
    removeFile(int no);
    wstringVector
    getFilenames();
    wstringVector
    getMode();
    boost::container::vector<uint64>
    getIDs();
    File*
    getFile(int no);
    File*
    getFile(std::wstring filename);
    void*
    getFilePointer(int no);
    size_t
    getNumberOfFiles();
};
//=============================================================================
} // namespace Nelson
  //=============================================================================