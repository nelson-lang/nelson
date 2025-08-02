//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/interprocess/detail/shared_dir_helpers.hpp>
#include "StringHelpers.hpp"
#include <boost/filesystem.hpp>
#include "RemoveIpcOldFiles.hpp"
#include "NelsonInterprocess.hpp"
#include "NelsonPIDs.hpp"
#include "characters_encoding.hpp"
#include "FileSystemWrapper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
RemoveIpcOldFiles()
{
    bool result = false;
    std::string ipcDirectory;
    boost::interprocess::ipcdetail::get_shared_dir(ipcDirectory);
    FileSystemWrapper::Path branch(ipcDirectory);
    bool isDirectory = FileSystemWrapper::Path::is_directory(branch);
    if (isDirectory) {
        for (boost::filesystem::directory_iterator p(branch.native()), end; p != end; ++p) {
            FileSystemWrapper::Path filepath(p->path().native());
            std::wstring filename = filepath.filename().wstring();
            if (StringHelpers::starts_with(
                    filename, utf8_to_wstring(NELSON_COMMAND_INTERPROCESS))) {
                std::wstring pidStr = StringHelpers::replace_all_copy(
                    filename, utf8_to_wstring(NELSON_COMMAND_INTERPROCESS) + L"_", L"");
                bool usedPid = false;
                try {
                    long pid = std::stoul(pidStr);
                    usedPid = isPIDRunning(pid);

                } catch (const std::exception&) {
                    usedPid = true;
                }
                result = true;
                if (!usedPid) {
                    result = FileSystemWrapper::Path::remove(filepath);
                }
            }
        }
    }
    return result;
}
//=============================================================================
}
//=============================================================================
