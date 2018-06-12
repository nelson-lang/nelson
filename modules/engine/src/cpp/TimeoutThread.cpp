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
#include "TimeoutThread.hpp"
#include "Sleep.hpp"
#include <boost/thread/thread.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
void
timeout(uint64 _timeout_seconds)
{
    SleepSeconds(_timeout_seconds);
    // https://msdn.microsoft.com/en-us/library/windows/desktop/ms681382(v=vs.85).aspx
    // WAIT_TIMEOUT (258)
    exit(258);
}
//=============================================================================
bool
TimeoutThread(uint64 _timeoutseconds)
{
    boost::thread timeout_thread(timeout, _timeoutseconds);
    timeout_thread.detach();
    return false;
}
//=============================================================================
}
//=============================================================================
