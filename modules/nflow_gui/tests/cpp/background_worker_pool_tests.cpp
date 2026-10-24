//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BackgroundWorkerPool.hpp"
#include <atomic>
#include <cassert>
#include <chrono>
#include <thread>
//=============================================================================
using Nelson::NFlowGateway::BackgroundWorkerPool;
//=============================================================================
int
main()
{
    BackgroundWorkerPool workers;
    std::atomic<bool> taskPosted { false };
    std::atomic<bool> taskProcessed { false };
    std::atomic<bool> workerFinished { false };

    const bool started = workers.start([&]() {
        taskPosted.store(true);
        while (!taskProcessed.load()) {
            std::this_thread::sleep_for(std::chrono::milliseconds(1));
        }
        workerFinished.store(true);
    });
    assert(started);

    int polls = 0;
    workers.stopAndJoin([&]() {
        ++polls;
        if (taskPosted.load()) {
            taskProcessed.store(true);
        }
    });

    assert(polls > 0);
    assert(workerFinished.load());
    assert(workers.activeCount() == 0);
    const bool restarted = workers.start([]() { });
    assert(!restarted);
    return 0;
}
//=============================================================================
