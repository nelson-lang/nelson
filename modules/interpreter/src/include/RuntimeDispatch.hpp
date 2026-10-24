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
#include <functional>
#include <cstddef>
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
using RuntimeDispatchTask = std::function<void()>;
//=============================================================================
struct RuntimeDispatchBackend
{
    std::function<void(RuntimeDispatchTask)> postToEvaluator;
    std::function<void(RuntimeDispatchTask)> postToGui;
    std::function<void(RuntimeDispatchTask)> callGuiSync;
    std::function<void(size_t)> notifyInterrupt;
    std::function<void(bool)> processGuiEvents;
    // Runs the GUI event loop for at most maxTimeMs milliseconds, waking
    // immediately on any posted event (cross-thread calls, timers, input).
    // Used by the engine main loop so it does not have to poll with a sleep
    // whose granularity is an OS timer tick (~15.6 ms on Windows).
    std::function<void(int)> pumpGuiEventsBlocking;
    std::function<bool()> hasQtRuntime;
    std::function<bool()> hasGuiServices;
    std::function<bool()> hasGraphicsServices;
    std::function<bool()> hasQmlServices;
    std::function<bool()> isGuiThread;
    std::function<bool()> isEvaluatorThread;
};
//=============================================================================
NLSINTERPRETER_IMPEXP void
registerRuntimeDispatchBackend(const RuntimeDispatchBackend& backend);
//=============================================================================
NLSINTERPRETER_IMPEXP void
resetRuntimeDispatchBackend();
//=============================================================================
NLSINTERPRETER_IMPEXP void
postToEvaluator(RuntimeDispatchTask task);
//=============================================================================
NLSINTERPRETER_IMPEXP void
postToGui(RuntimeDispatchTask task);
//=============================================================================
NLSINTERPRETER_IMPEXP void
callGuiSync(RuntimeDispatchTask task);
//=============================================================================
NLSINTERPRETER_IMPEXP void
notifyInterrupt(size_t evaluatorId);
//=============================================================================
NLSINTERPRETER_IMPEXP void
processGuiEvents(bool waitEvents);
//=============================================================================
// Returns false when no backend pump is registered (caller should fall back
// to polling); otherwise runs the GUI event loop for at most maxTimeMs.
NLSINTERPRETER_IMPEXP bool
pumpGuiEventsBlocking(int maxTimeMs);
//=============================================================================
NLSINTERPRETER_IMPEXP bool
hasQtRuntime();
//=============================================================================
NLSINTERPRETER_IMPEXP bool
hasGuiServices();
//=============================================================================
NLSINTERPRETER_IMPEXP bool
hasGraphicsServices();
//=============================================================================
NLSINTERPRETER_IMPEXP bool
hasQmlServices();
//=============================================================================
NLSINTERPRETER_IMPEXP bool
isGuiThread();
//=============================================================================
NLSINTERPRETER_IMPEXP bool
isEvaluatorThread();
//=============================================================================
/**
 * True when the calling thread may run an interpreter task in place while the
 * interpreter is parked at the prompt: the host has no GUI thread, or the
 * caller is that thread. In desktop GUI mode the prompt loop runs on the GUI
 * thread (QtTerminal::getLine): a task posted by a background worker that needs
 * that thread (an nflow StopFcn opening a figure) must be queued and drained by
 * the prompt loop instead. Run on the worker, the figure call would wait for a
 * GUI thread itself blocked on the idle hand-off: a deadlock.
 */
NLSINTERPRETER_IMPEXP bool
canRunIdleInterpreterTaskOnCallingThread();
//=============================================================================
} // namespace Nelson
//=============================================================================
