//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================

#include <chrono>
#include <string>
#include <atomic>
#include <tuple>
#include <thread>
#include <future>
#include <vector>
#include <cstdio>
#include <cstdlib>

#include "Types.hpp"
#include "FileSystemWrapper.hpp"

#ifdef _MSC_VER
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#else
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <signal.h>
#endif

//=============================================================================
// Define PROCESS_CHILD as a small wrapper around platform-specific process handle
#ifdef _MSC_VER
struct PROCESS_CHILD
{
    HANDLE processHandle = nullptr;
    DWORD processId = 0;
    bool
    valid() const
    {
        return processHandle != nullptr;
    }
};
#else
struct PROCESS_CHILD
{
    pid_t pid = -1;
    bool
    valid() const
    {
        return pid > 0;
    }
};
#endif

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
    monitorChildProcess(PROCESS_CHILD& childProcess, uint64 timeout);
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
