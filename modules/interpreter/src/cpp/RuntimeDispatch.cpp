//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RuntimeDispatch.hpp"
#include <atomic>
#include <mutex>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    std::mutex backendMutex;
    RuntimeDispatchBackend backend;
    bool
    callBool(const std::function<bool()>& fn)
    {
        return fn ? fn() : false;
    }
    //=============================================================================
    RuntimeDispatchBackend
    currentBackend()
    {
        std::lock_guard<std::mutex> lock(backendMutex);
        return backend;
    }
    //=============================================================================
} // namespace
//=============================================================================
void
registerRuntimeDispatchBackend(const RuntimeDispatchBackend& newBackend)
{
    std::lock_guard<std::mutex> lock(backendMutex);
    backend = newBackend;
}
//=============================================================================
void
resetRuntimeDispatchBackend()
{
    std::lock_guard<std::mutex> lock(backendMutex);
    backend = RuntimeDispatchBackend();
}
//=============================================================================
void
postToEvaluator(RuntimeDispatchTask task)
{
    if (!task) {
        return;
    }
    RuntimeDispatchBackend localBackend = currentBackend();
    if (localBackend.postToEvaluator) {
        localBackend.postToEvaluator(std::move(task));
    } else {
        task();
    }
}
//=============================================================================
void
postToGui(RuntimeDispatchTask task)
{
    if (!task) {
        return;
    }
    RuntimeDispatchBackend localBackend = currentBackend();
    if (localBackend.postToGui) {
        localBackend.postToGui(std::move(task));
    } else {
        task();
    }
}
//=============================================================================
void
callGuiSync(RuntimeDispatchTask task)
{
    if (!task) {
        return;
    }
    RuntimeDispatchBackend localBackend = currentBackend();
    if (localBackend.callGuiSync) {
        localBackend.callGuiSync(std::move(task));
    } else {
        task();
    }
}
//=============================================================================
void
notifyInterrupt(size_t evaluatorId)
{
    RuntimeDispatchBackend localBackend = currentBackend();
    if (localBackend.notifyInterrupt) {
        localBackend.notifyInterrupt(evaluatorId);
    }
}
//=============================================================================
void
processGuiEvents(bool waitEvents)
{
    RuntimeDispatchBackend localBackend = currentBackend();
    if (localBackend.processGuiEvents) {
        localBackend.processGuiEvents(waitEvents);
    }
}
//=============================================================================
bool
pumpGuiEventsBlocking(int maxTimeMs)
{
    RuntimeDispatchBackend localBackend = currentBackend();
    if (localBackend.pumpGuiEventsBlocking) {
        localBackend.pumpGuiEventsBlocking(maxTimeMs);
        return true;
    }
    return false;
}
//=============================================================================
bool
hasQtRuntime()
{
    RuntimeDispatchBackend localBackend = currentBackend();
    return callBool(localBackend.hasQtRuntime);
}
//=============================================================================
bool
hasGuiServices()
{
    RuntimeDispatchBackend localBackend = currentBackend();
    return callBool(localBackend.hasGuiServices);
}
//=============================================================================
bool
hasGraphicsServices()
{
    RuntimeDispatchBackend localBackend = currentBackend();
    return callBool(localBackend.hasGraphicsServices);
}
//=============================================================================
bool
hasQmlServices()
{
    RuntimeDispatchBackend localBackend = currentBackend();
    return callBool(localBackend.hasQmlServices);
}
//=============================================================================
bool
isGuiThread()
{
    RuntimeDispatchBackend localBackend = currentBackend();
    return callBool(localBackend.isGuiThread);
}
//=============================================================================
bool
isEvaluatorThread()
{
    RuntimeDispatchBackend localBackend = currentBackend();
    return callBool(localBackend.isEvaluatorThread);
}
//=============================================================================
bool
canRunIdleInterpreterTaskOnCallingThread()
{
    RuntimeDispatchBackend localBackend = currentBackend();
    return !callBool(localBackend.hasGuiServices) || callBool(localBackend.isGuiThread);
}
//=============================================================================
namespace {
    std::mutex wakeupHookMutex;
    InterpreterWakeupHook wakeupHook;
    // Fast path for producers: skip the lock while no front end listens.
    std::atomic<bool> wakeupHookInstalled { false };
} // namespace
//=============================================================================
void
setInterpreterWakeupHook(InterpreterWakeupHook hook)
{
    std::lock_guard<std::mutex> lock(wakeupHookMutex);
    wakeupHook = std::move(hook);
    wakeupHookInstalled.store(static_cast<bool>(wakeupHook), std::memory_order_release);
}
//=============================================================================
void
notifyInterpreterWakeup()
{
    if (!wakeupHookInstalled.load(std::memory_order_acquire)) {
        return;
    }
    InterpreterWakeupHook hook;
    {
        std::lock_guard<std::mutex> lock(wakeupHookMutex);
        hook = wakeupHook;
    }
    if (hook) {
        hook();
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
