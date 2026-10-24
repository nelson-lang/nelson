//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Thread-affinity policy behind MainThreadTaskQueue::runNowIfIdleOrQueue: a
// background poster may run an interpreter task in place at the prompt only
// when no GUI thread owns that prompt. In desktop GUI mode the prompt loop is
// the GUI thread, and an nflow StopFcn run on the simulation worker froze the
// editor (figure creation waited for a GUI thread blocked on the idle
// hand-off), so the policy must send such a task to the queue.
//=============================================================================
#include "RuntimeDispatch.hpp"
#include <cstdio>
#include <thread>
//=============================================================================
namespace {
//=============================================================================
int failures = 0;
//=============================================================================
void
check(bool condition, const char* what, int line)
{
    if (!condition) {
        std::fprintf(stderr, "FAILED (line %d): %s\n", line, what);
        ++failures;
    }
}
#define CHECK(condition) check((condition), #condition, __LINE__)
//=============================================================================
bool
workerMayRunInPlace()
{
    bool result = false;
    std::thread worker(
        [&result]() { result = Nelson::canRunIdleInterpreterTaskOnCallingThread(); });
    worker.join();
    return result;
}
//=============================================================================
} // namespace
//=============================================================================
int
main()
{
    // Command-line host without any dispatch backend: every thread may run an
    // idle task in place (the advanced terminal path with webview figures).
    Nelson::resetRuntimeDispatchBackend();
    CHECK(Nelson::canRunIdleInterpreterTaskOnCallingThread());
    CHECK(workerMayRunInPlace());

    // Desktop GUI host: the prompt loop is the GUI thread (this thread).
    const std::thread::id guiThread = std::this_thread::get_id();
    Nelson::RuntimeDispatchBackend backend;
    backend.hasGuiServices = []() { return true; };
    backend.isGuiThread = [guiThread]() { return std::this_thread::get_id() == guiThread; };
    Nelson::registerRuntimeDispatchBackend(backend);
    // A poster already on the GUI thread keeps the direct path.
    CHECK(Nelson::canRunIdleInterpreterTaskOnCallingThread());
    // A background worker must queue its task for the GUI thread.
    CHECK(!workerMayRunInPlace());

    // A backend without GUI services (no Qt thread) keeps the direct path for
    // workers as well.
    backend.hasGuiServices = []() { return false; };
    Nelson::registerRuntimeDispatchBackend(backend);
    CHECK(workerMayRunInPlace());

    Nelson::resetRuntimeDispatchBackend();
    return failures == 0 ? 0 : 1;
}
//=============================================================================
