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
#include "File.hpp"
#include "Interface.hpp"
#include "Types.hpp"
#include "nlsStream_manager_exports.h"
#include <vector>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSSTREAM_MANAGER_IMPEXP FilesManager
{
private:
    std::vector<File*> userFiles;
    int
    getAvailableFileID();

public:
    FilesManager(Interface* io = nullptr);
    ~FilesManager();
    bool
    isOpened(const std::wstring& filenameToSearch);
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
    std::vector<uint64>
    getIDs();
    File*
    getFile(int no);
    File*
    getFile(const std::wstring& filename);
    void*
    getFilePointer(int no);
    size_t
    getNumberOfFiles();
};
//=============================================================================
} // namespace Nelson
  //=============================================================================