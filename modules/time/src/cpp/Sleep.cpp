//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "Sleep.hpp"
#include "NelsonConfiguration.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include <boost/chrono/chrono.hpp>
#include <boost/thread/thread.hpp>
#include <math.h>
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
                if (eval->haveEventsLoop()) {
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
                if (eval->haveEventsLoop()) {
                    ProcessEventsDynamicFunctionWithoutWait();
                }
            } while (
                !NelsonConfiguration::getInstance()->getInterruptPending() && (bContinue == true));
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
