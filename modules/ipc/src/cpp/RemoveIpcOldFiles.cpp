//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string>
#include "RemoveIpcOldFiles.hpp"
#include "IpcChannel.hpp"
#include "NelsonPIDs.hpp"
#include "StringHelpers.hpp"
#include "FileSystemWrapper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
RemoveIpcOldFiles()
{
    // Windows endpoints are named-pipe kernel objects with no on-disk files, so
    // there is nothing to clean; the directory is empty there.
    const std::string ipcDirectory = ipcChannelEndpointDirectory();
    if (ipcDirectory.empty()) {
        return true;
    }
    FileSystemWrapper::Path branch(ipcDirectory);
    if (!FileSystemWrapper::Path::is_directory(branch)) {
        return true;
    }
    // Endpoint sockets are named "ipc_<pid>.sock"; drop those whose owner pid is
    // no longer running.
    for (std::filesystem::directory_iterator p(branch.native()), end; p != end; ++p) {
        FileSystemWrapper::Path filepath(p->path().native());
        const std::string filename = filepath.filename().string();
        if (!StringHelpers::starts_with(filename, std::string("ipc_"))
            || !StringHelpers::ends_with(filename, std::string(".sock"))) {
            continue;
        }
        std::string pidStr = filename.substr(4, filename.size() - 4 - 5);
        bool usedPid = true;
        try {
            usedPid = isPIDRunning((int)std::stoul(pidStr));
        } catch (const std::exception&) {
            usedPid = true;
        }
        if (!usedPid) {
            FileSystemWrapper::Path::remove(filepath);
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
