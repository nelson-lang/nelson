//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <chrono>
#include <string>
#include <atomic>
#include <tuple>
#include <boost/asio.hpp>
#include <boost/process.hpp>
#include <boost/process/shell.hpp>
#include "Types.hpp"
#include "FileSystemWrapper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class SystemCommandTask
{
public:
    //=============================================================================
    void
    terminate();
    //=============================================================================
    bool
    isRunning();
    //=============================================================================
    std::tuple<int, std::wstring, uint64>
    getResult();
    //=============================================================================
    uint64
    getDuration();
    //=============================================================================
    void
    evaluateCommand(const std::wstring& command, uint64 timeout);
    //=============================================================================
private:
    //=============================================================================
    std::atomic<bool> _running = false;
    std::atomic<bool> _terminate = false;
    int _exitCode = 0;
    std::wstring _message = std::wstring();
    uint64 _duration = uint64(0);
    std::chrono::steady_clock::time_point _beginTimePoint;
    //=============================================================================
    std::wstring
    buildCommandString(const std::wstring& _command);
    //=============================================================================
    void
    executeDetachedProcess(const std::wstring& cmd);
    //=============================================================================
    void
    executeAttachedProcess(const std::wstring& cmd, const FileSystemWrapper::Path& tempOutputFile,
        const FileSystemWrapper::Path& tempErrorFile, uint64 timeout);
    //=============================================================================
    void
    monitorChildProcess(boost::process::child& childProcess, uint64 timeout);
    //=============================================================================
    std::wstring
    readProcessOutput(const FileSystemWrapper::Path& tempOutputFile,
        const FileSystemWrapper::Path& tempErrorFile);
    //=============================================================================
    void
    cleanupTempFiles(const FileSystemWrapper::Path& tempOutputFile,
        const FileSystemWrapper::Path& tempErrorFile);
    //=============================================================================
    std::wstring
    getPlatformSpecificShellArgs();
    //=============================================================================
    int
    exitCodeAbort();
    //=============================================================================
    std::wstring
    readFile(const FileSystemWrapper::Path& filePath);
    //=============================================================================
    std::wstring
    detectDetachProcess(const std::wstring& command, bool& haveDetach);
    //=============================================================================
    std::wstring
    cleanCommand(const std::wstring& command);
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
