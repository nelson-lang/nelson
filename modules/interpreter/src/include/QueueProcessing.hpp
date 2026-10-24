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
#include <atomic>
#include "CallbackQueue.hpp"
#include "EventQueue.hpp"
#include "MainThreadTaskQueue.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Optional hook run at the start of every callback/timer drain. On single-
// threaded WebAssembly the timer module registers it so due timers are advanced
// here (no worker thread); on native builds nothing registers it and it is a
// no-op, so the desktop timer path is unchanged.
inline std::atomic<void (*)()>&
pendingTimerPumpHook()
{
    static std::atomic<void (*)()> hook { nullptr };
    return hook;
}
//=============================================================================
inline void
setPendingTimerPumpHook(void (*hook)())
{
    pendingTimerPumpHook().store(hook, std::memory_order_relaxed);
}
//=============================================================================
inline void
processPendingCallbacksAndTimers(Evaluator* eval = nullptr)
{
    if (eval == nullptr) {
        eval = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    }
    if (eval == nullptr) {
        return;
    }
    void (*timerPump)() = pendingTimerPumpHook().load(std::memory_order_relaxed);
    if (timerPump != nullptr) {
        timerPump();
    }
    MainThreadTaskQueue::getInstance()->processTasks(eval);
    CallbackQueue::getInstance()->processCallback(eval);
    EventQueue::getInstance()->processCallback(eval);
}
//=============================================================================
// Runs every queued graphics callback now and returns how many ran. Only the
// __web_flush_callbacks__ builtin uses it: a web host appends it to a figure
// action evaluated while the interpreter is idle (a direct RPC), so the frames
// returned by that RPC already reflect the callbacks. A web action drained at a
// statement boundary of a running script only queues its callbacks; they run
// at the next drawnow()/pause(), as native window events do.
inline size_t
processPendingGraphicsCallbacksNow(Evaluator* eval = nullptr)
{
    if (eval == nullptr) {
        eval = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    }
    size_t executed = 0;
    while (eval != nullptr && CallbackQueue::getInstance()->processCallbackNow(eval)) {
        ++executed;
    }
    return executed;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
