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
    // copy one callback under the lock, not the whole backend
    template <typename Callback>
    Callback
    backendCallback(Callback RuntimeDispatchBackend::* member)
    {
        std::lock_guard<std::mutex> lock(backendMutex);
        return backend.*member;
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
callGuiSync(RuntimeDispatchTask task)
{
    if (!task) {
        return;
    }
    auto callGuiSyncFn = backendCallback(&RuntimeDispatchBackend::callGuiSync);
    if (callGuiSyncFn) {
        callGuiSyncFn(std::move(task));
    } else {
        task();
    }
}
//=============================================================================
void
processGuiEvents(bool waitEvents)
{
    auto processGuiEventsFn = backendCallback(&RuntimeDispatchBackend::processGuiEvents);
    if (processGuiEventsFn) {
        processGuiEventsFn(waitEvents);
    }
}
//=============================================================================
bool
hasGuiServices()
{
    return callBool(backendCallback(&RuntimeDispatchBackend::hasGuiServices));
}
//=============================================================================
bool
isGuiThread()
{
    return callBool(backendCallback(&RuntimeDispatchBackend::isGuiThread));
}
//=============================================================================
bool
canRunIdleInterpreterTaskOnCallingThread()
{
    return !hasGuiServices() || isGuiThread();
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
