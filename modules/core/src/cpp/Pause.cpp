//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <chrono>
#include <thread>
#include "Pause.hpp"
#include "NelsonConfiguration.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "CallbackQueue.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
Pause(Evaluator* eval, double seconds)
{
    if (std::isinf(seconds)) {
        while (!NelsonConfiguration::getInstance()->getInterruptPending(eval->getID())) {
            std::this_thread::sleep_for(std::chrono::milliseconds(uint64(1)));
            if (eval && eval->haveEventsLoop()) {
                CallbackQueue::getInstance()->processCallback(eval);
                ProcessEventsDynamicFunctionWithoutWait();
            }
        }
    } else if (std::isnan(seconds)) {
        // DO NOTHING
    } else {
        std::chrono::nanoseconds begin_time
            = std::chrono::high_resolution_clock::now().time_since_epoch();
        bool bContinue = true;
        do {
            std::this_thread::sleep_for(std::chrono::milliseconds(uint64(1)));
            std::chrono::nanoseconds current_time
                = std::chrono::high_resolution_clock::now().time_since_epoch();
            std::chrono::nanoseconds difftime = (current_time - begin_time);
            bContinue = !(difftime.count() > int64(seconds * 1e9));
            if (eval != nullptr && eval->haveEventsLoop()) {
                CallbackQueue::getInstance()->processCallback(eval);
                ProcessEventsDynamicFunctionWithoutWait();
            }
        } while (!NelsonConfiguration::getInstance()->getInterruptPending(eval->getID())
            && (static_cast<int>(bContinue) == true));
    }
}
//=============================================================================
}
//=============================================================================
