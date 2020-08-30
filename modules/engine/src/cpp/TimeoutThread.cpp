//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/thread/thread.hpp>
#include "TimeoutThread.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static boost::thread* timeout_thread = nullptr;
//=============================================================================
void
timeout(uint64 _timeout_seconds)
{
    try {
        boost::this_thread::sleep(boost::posix_time::seconds(_timeout_seconds));
    } catch (boost::thread_interrupted&) {
        return;
    }
    // https://msdn.microsoft.com/en-us/library/windows/desktop/ms681382(v=vs.85).aspx
    // WAIT_TIMEOUT (258)
    exit(258);
}
//=============================================================================
bool
createTimeoutThread(uint64 _timeoutseconds)
{
    try {
        timeout_thread = new boost::thread(timeout, _timeoutseconds);
    } catch (const std::bad_alloc&) {
        timeout_thread = nullptr;
    }
    if (timeout_thread) {
        timeout_thread->detach();
        return true;
    }
    return false;
}
//=============================================================================
bool
destroyTimeoutThread()
{
    if (timeout_thread) {
        timeout_thread->interrupt();
        delete timeout_thread;
        timeout_thread = nullptr;
        return true;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
