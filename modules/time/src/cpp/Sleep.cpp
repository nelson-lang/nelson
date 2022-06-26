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
#include <cmath>
#include "Sleep.hpp"
#include "NelsonConfiguration.hpp"
#include "ProcessEventsDynamicFunction.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
SleepSeconds(uint64 tValue)
{
    std::this_thread::sleep_for(std::chrono::seconds(tValue));
}
//=============================================================================
bool
Sleep(Evaluator* eval, double tValue)
{
    if (tValue > 0) {
        if (std::isinf(tValue)) {
            while (!NelsonConfiguration::getInstance()->getInterruptPending()) {
                std::this_thread::sleep_for(std::chrono::milliseconds(uint64(10)));
                if (eval != nullptr && eval->haveEventsLoop()) {
                    ProcessEventsDynamicFunctionWithoutWait();
                }
            }
        } else {
            std::chrono::nanoseconds begin_time
                = std::chrono::high_resolution_clock::now().time_since_epoch();
            bool bContinue = true;
            do {
                std::this_thread::sleep_for(std::chrono::nanoseconds(uint64(10)));
                std::chrono::nanoseconds current_time
                    = std::chrono::high_resolution_clock::now().time_since_epoch();
                std::chrono::nanoseconds difftime = (current_time - begin_time);
                bContinue = !(difftime.count() > int64(tValue * 1e9));
                if (eval != nullptr && eval->haveEventsLoop()) {
                    ProcessEventsDynamicFunctionWithoutWait();
                }
            } while (!NelsonConfiguration::getInstance()->getInterruptPending()
                && (static_cast<int>(bContinue) == true));
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
