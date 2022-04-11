//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/interprocess/detail/shared_dir_helpers.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/operations.hpp>
#include "RemoveIpcOldFiles.hpp"
#include "NelsonInterprocess.hpp"
#include "NelsonPIDs.hpp"
#include "characters_encoding.hpp"
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
            if (boost::algorithm::starts_with(
                    filename, utf8_to_wstring(NELSON_COMMAND_INTERPROCESS))) {
                std::wstring pidStr = boost::replace_all_copy(
                    filename, utf8_to_wstring(NELSON_COMMAND_INTERPROCESS) + L"_", L"");
                bool usedPid = false;
                try {
                    long pid = std::stoul(pidStr);
                    usedPid = isPIDRunning(pid);

                } catch (const std::exception&) {
                    usedPid = true;
                }
                try {
                    result = true;
                    if (!usedPid) {
                        boost::filesystem::remove(filepath);
                    }
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
