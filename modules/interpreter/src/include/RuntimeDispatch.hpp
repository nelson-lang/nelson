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
    std::function<void(RuntimeDispatchTask)> callGuiSync;
    std::function<void(bool)> processGuiEvents;
    std::function<bool()> hasGuiServices;
    std::function<bool()> isGuiThread;
};
//=============================================================================
NLSINTERPRETER_IMPEXP void
registerRuntimeDispatchBackend(const RuntimeDispatchBackend& backend);
//=============================================================================
NLSINTERPRETER_IMPEXP void
resetRuntimeDispatchBackend();
//=============================================================================
NLSINTERPRETER_IMPEXP void
callGuiSync(RuntimeDispatchTask task);
//=============================================================================
NLSINTERPRETER_IMPEXP void
processGuiEvents(bool waitEvents);
//=============================================================================
NLSINTERPRETER_IMPEXP bool
hasGuiServices();
//=============================================================================
NLSINTERPRETER_IMPEXP bool
isGuiThread();
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
// Hook installed by a console front end so queued interpreter work wakes its blocking prompt.
using InterpreterWakeupHook = std::function<void()>;
NLSINTERPRETER_IMPEXP void
setInterpreterWakeupHook(InterpreterWakeupHook hook);
// Called by producers after enqueueing; cheap no-op when no hook is installed.
NLSINTERPRETER_IMPEXP void
notifyInterpreterWakeup();
//=============================================================================
} // namespace Nelson
//=============================================================================
