//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/chrono/chrono.hpp>
#include <boost/thread/thread.hpp>
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
    boost::this_thread::sleep_for(boost::chrono::seconds(tValue));
}
//=============================================================================
bool
Sleep(Evaluator* eval, double tValue)
{
    if (tValue > 0) {
        if (std::isinf(tValue)) {
            while (!NelsonConfiguration::getInstance()->getInterruptPending()) {
                boost::this_thread::sleep_for(boost::chrono::milliseconds(uint64(10)));
                if (eval != nullptr && eval->haveEventsLoop()) {
                    ProcessEventsDynamicFunctionWithoutWait();
                }
            }
        } else {
            boost::chrono::nanoseconds begin_time
                = boost::chrono::high_resolution_clock::now().time_since_epoch();
            bool bContinue = true;
            do {
                boost::this_thread::sleep_for(boost::chrono::nanoseconds(uint64(10)));
                boost::chrono::nanoseconds current_time
                    = boost::chrono::high_resolution_clock::now().time_since_epoch();
                boost::chrono::nanoseconds difftime = (current_time - begin_time);
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
