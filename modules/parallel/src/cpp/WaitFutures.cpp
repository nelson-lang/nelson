//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <limits>
#include <thread>
#include <chrono>
#include "WaitFutures.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
allAreFinished(const std::vector<FutureObject*>& futures)
{
    return std::all_of(futures.begin(), futures.end(),
        [](FutureObject* f) { return (f->state == THREAD_STATE::FINISHED); });
}
//=============================================================================
static bool
allAreRunningOrFinished(const std::vector<FutureObject*>& futures)
{
    return std::all_of(futures.begin(), futures.end(), [](FutureObject* f) {
        return (f->state == THREAD_STATE::RUNNING) || (f->state == THREAD_STATE::FINISHED);
    });
}
//=============================================================================
bool
WaitFutures(Evaluator* eval, const std::vector<FutureObject*>& futures, THREAD_STATE expectedState,
    double timeoutSeconds)
{
    if (futures.empty()) {
        return true;
    }

    int64_t timeout_ns = std::isinf(timeoutSeconds) ? std::numeric_limits<int64_t>::max()
                                                    : int64_t(timeoutSeconds * 1e9);

    auto begin_time = std::chrono::high_resolution_clock::now();

    while (true) {
        if (expectedState == THREAD_STATE::FINISHED) {
            if (allAreFinished(futures)) {
                return true;
            }
        } else {
            if (allAreRunningOrFinished(futures)) {
                return true;
            }
        }
        auto current_time = std::chrono::high_resolution_clock::now();
        auto difftime_ns
            = std::chrono::duration_cast<std::chrono::nanoseconds>(current_time - begin_time)
                  .count();

        if (difftime_ns > timeout_ns) {
            return false;
        }

        if (eval != nullptr
            && NelsonConfiguration::getInstance()->getInterruptPending(eval->getID())) {
            return false;
        }

        std::this_thread::sleep_for(std::chrono::milliseconds(50));

        if (eval != nullptr && eval->haveEventsLoop()) {
            ProcessEventsDynamicFunctionWithoutWait();
        }
    }

    return false;
}
//=============================================================================
}
//=============================================================================
