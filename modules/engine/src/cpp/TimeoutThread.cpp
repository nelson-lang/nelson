//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <csignal>
#include <unistd.h>
#endif
#include <chrono>
#include <condition_variable>
#include <cstdlib>
#include <memory>
#include <mutex>
#include <thread>
#include "TimeoutThread.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class WaitTimeout
{
    //=============================================================================
private:
    //=============================================================================
    std::mutex _mutex;
    std::condition_variable _condition;
    bool _stopRequested = false;
    //=============================================================================
    static int
    timeoutExitCode()
    {
#ifdef _MSC_VER
        // https://msdn.microsoft.com/en-us/library/windows/desktop/ms681382(v=vs.85).aspx
        // WAIT_TIMEOUT (258)
        return 258;
#else
        return 128 + SIGABRT;
#endif
    }
    //=============================================================================
    static void
    forceTerminateProcess()
    {
#ifdef _MSC_VER
        TerminateProcess(GetCurrentProcess(), (UINT)timeoutExitCode());
#else
        _exit(timeoutExitCode());
#endif
    }
    //=============================================================================
    [[noreturn]] static void
    exitWithCode()
    {
        forceTerminateProcess();
        std::_Exit(timeoutExitCode());
    }
    //=============================================================================
public:
    //=============================================================================
    void
    start(uint64 _timeout_seconds)
    {
        std::unique_lock<std::mutex> lock(_mutex);
        bool stopped = _condition.wait_for(
            lock, std::chrono::seconds(_timeout_seconds), [this]() { return _stopRequested; });
        if (!stopped) {
            exitWithCode();
        }
    }
    //=============================================================================
    void
    stop()
    {
        {
            std::lock_guard<std::mutex> lock(_mutex);
            _stopRequested = true;
        }
        _condition.notify_all();
    }
    //=============================================================================
};
//=============================================================================
static std::thread* timeout_thread = nullptr;
static WaitTimeout* waitTask = nullptr;
//=============================================================================
bool
createTimeoutThread(uint64 _timeoutseconds)
{
    try {
        auto newWaitTask = std::make_unique<WaitTimeout>();
        auto newTimeoutThread = std::make_unique<std::thread>(
            &WaitTimeout::start, newWaitTask.get(), _timeoutseconds);
        waitTask = newWaitTask.release();
        timeout_thread = newTimeoutThread.release();
    } catch (const std::bad_alloc&) {
        timeout_thread = nullptr;
        waitTask = nullptr;
    }
    return timeout_thread != nullptr;
}
//=============================================================================
bool
destroyTimeoutThread()
{
    if (timeout_thread) {
        waitTask->stop();
        if (timeout_thread->joinable()) {
            timeout_thread->join();
        }
        delete timeout_thread;
        delete waitTask;
        waitTask = nullptr;
        timeout_thread = nullptr;
        return true;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
