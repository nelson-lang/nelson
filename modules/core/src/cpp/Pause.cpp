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
#include <thread>
#if defined(_WIN32)
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <Windows.h>
#endif
#include "Pause.hpp"
#include "NelsonConfiguration.hpp"
#include "RuntimeDispatch.hpp"
#include "QueueProcessing.hpp"
#include "WebDesktopBridge.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
#if defined(_WIN32)
#ifndef CREATE_WAITABLE_TIMER_HIGH_RESOLUTION
#define CREATE_WAITABLE_TIMER_HIGH_RESOLUTION 0x00000002
#endif
    //=============================================================================
    class ScopedWaitTimer
    {
        HANDLE timer = nullptr;

    public:
        ScopedWaitTimer()
        {
            timer = CreateWaitableTimerExW(
                nullptr, nullptr, CREATE_WAITABLE_TIMER_HIGH_RESOLUTION, TIMER_ALL_ACCESS);
            if (!timer) {
                timer = CreateWaitableTimerExW(nullptr, nullptr, 0, TIMER_ALL_ACCESS);
            }
        }

        ~ScopedWaitTimer()
        {
            if (timer) {
                CloseHandle(timer);
            }
        }

        void
        sleepMilliseconds(uint64 milliseconds)
        {
            if (!timer) {
                std::this_thread::sleep_for(std::chrono::milliseconds(milliseconds));
                return;
            }
            LARGE_INTEGER dueTime;
            dueTime.QuadPart = -static_cast<LONGLONG>(milliseconds) * 10000LL;
            if (!SetWaitableTimer(timer, &dueTime, 0, nullptr, nullptr, FALSE)) {
                std::this_thread::sleep_for(std::chrono::milliseconds(milliseconds));
                return;
            }
            WaitForSingleObject(timer, INFINITE);
        }
    };
#else
    //=============================================================================
    class ScopedWaitTimer
    {
    public:
        void
        sleepMilliseconds(uint64 milliseconds)
        {
            std::this_thread::sleep_for(std::chrono::milliseconds(milliseconds));
        }
    };
#endif
    //=============================================================================
} // namespace
//=============================================================================
// Paced figure loops (e.g. the raycaster demo) spend most of each frame in pause().
// On single-threaded WebAssembly the key/mouse mailbox is otherwise polled only at
// bytecode statement boundaries, so held or released keys lag.
// No-op on native builds (no pump hook registered).
static inline void
servicePausedWebEvents()
{
    WebDesktopBridge* bridge = WebDesktopBridge::getInstance();
    if (bridge->hasWebEventPumpHook()) {
        bridge->pumpWebEvents();
    }
}
//=============================================================================
void
Pause(Evaluator* eval, double seconds)
{
    ScopedWaitTimer waitTimer;
    if (std::isinf(seconds)) {
        while (!NelsonConfiguration::getInstance()->getInterruptPending(eval->getID())) {
            waitTimer.sleepMilliseconds(uint64(1));
            if (eval && eval->haveEventsLoop()) {
                servicePausedWebEvents();
                processPendingCallbacksAndTimers(eval);
                Nelson::processGuiEvents(false);
            }
        }
    } else if (std::isnan(seconds)) {
        // DO NOTHING
    } else {
        std::chrono::nanoseconds begin_time
            = std::chrono::high_resolution_clock::now().time_since_epoch();
        bool bContinue = true;
        do {
            waitTimer.sleepMilliseconds(uint64(1));
            std::chrono::nanoseconds current_time
                = std::chrono::high_resolution_clock::now().time_since_epoch();
            std::chrono::nanoseconds difftime = (current_time - begin_time);
            bContinue = !(difftime.count() > int64(seconds * 1e9));
            if (eval != nullptr && eval->haveEventsLoop()) {
                servicePausedWebEvents();
                processPendingCallbacksAndTimers(eval);
                Nelson::processGuiEvents(false);
            }
        } while (!NelsonConfiguration::getInstance()->getInterruptPending(eval->getID())
            && (static_cast<int>(bContinue) == true));
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
