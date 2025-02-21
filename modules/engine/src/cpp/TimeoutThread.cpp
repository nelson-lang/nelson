//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdlib>
#include <thread>
#ifndef _MSC_VER
#include <csignal>
#endif
#include "TimeoutThread.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class WaitTimeout
{
    //=============================================================================
private:
    //=============================================================================
    bool _running = false;
    bool _stop = false;
    //=============================================================================
    void
    exitWithCode()
    {
#ifdef _MSC_VER
        // https://msdn.microsoft.com/en-us/library/windows/desktop/ms681382(v=vs.85).aspx
        // WAIT_TIMEOUT (258)
        exit(258);
#else
        // SIGABRT
        exit(128 + SIGABRT);
#endif
    }
    //=============================================================================
public:
    //=============================================================================
    bool
    isRunning()
    {
        return _running;
    }
    //=============================================================================
    void
    start(uint64 _timeout_seconds)
    {
        _running = true;
        std::chrono::nanoseconds begin_time = std::chrono::steady_clock::now().time_since_epoch();
        bool bContinue = true;
        do {
            std::this_thread::sleep_for(std::chrono::milliseconds(uint64(1)));
            std::chrono::nanoseconds current_time
                = std::chrono::steady_clock::now().time_since_epoch();
            std::chrono::nanoseconds difftime = (current_time - begin_time);
            bContinue = (difftime <= std::chrono::seconds(_timeout_seconds));
        } while (bContinue && !_stop);
        if (!_stop) {
            exitWithCode();
        }
        _running = false;
    }
    //=============================================================================
    void
    stop()
    {
        _stop = true;
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
        waitTask = new WaitTimeout();
        timeout_thread = new std::thread(&WaitTimeout::start, waitTask, _timeoutseconds);
    } catch (const std::bad_alloc&) {
        timeout_thread = nullptr;
        waitTask = nullptr;
    }

    if (timeout_thread) {
        timeout_thread->detach();
        return true;
    }
    return false;
}
//=============================================================================
bool
destroyTimeoutThread()
{
    if (timeout_thread) {
        waitTask->stop();
        while (waitTask->isRunning()) {
            std::this_thread::sleep_for(std::chrono::milliseconds(uint64(1)));
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
