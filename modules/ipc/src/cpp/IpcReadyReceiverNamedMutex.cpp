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
#include <boost/interprocess/shared_memory_object.hpp>
#include <boost/interprocess/sync/named_mutex.hpp>
#include "IpcReadyReceiverNamedMutex.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static boost::interprocess::named_mutex* ipc_receiver_ready_mutex = nullptr;
//=============================================================================
static std::string
getNamedMutex(int pid)
{
    return std::string("NELSON_IPC_") + std::to_string(pid);
}
//=============================================================================
bool
openIpcReceiverIsReadyMutex(int pid)
{
    bool res = false;
    if (ipc_receiver_ready_mutex == nullptr) {
        std::string name = getNamedMutex(pid);
        try {
            boost::interprocess::named_mutex::remove(name.c_str());
            ipc_receiver_ready_mutex = new boost::interprocess::named_mutex(
                boost::interprocess::open_or_create, name.c_str());
            res = true;
        } catch (boost::interprocess::interprocess_exception&) {
            res = false;
        }
    }
    return res;
}
//=============================================================================
bool
closeIpcReceiverIsReadyMutex(int pid)
{
    bool res = false;
    if (ipc_receiver_ready_mutex) {
        std::string name = getNamedMutex(pid);
        ipc_receiver_ready_mutex->remove(name.c_str());
        delete ipc_receiver_ready_mutex;
        ipc_receiver_ready_mutex = nullptr;
    }
    return res;
}
//=============================================================================
bool
haveIpcReceiverIsReadyMutex(int pid)
{
    bool res = false;
    std::string name = getNamedMutex(pid);
    try {
        boost::interprocess::named_mutex other_nelson_mutex(
            boost::interprocess::open_only, name.c_str());
        res = true;
    } catch (const boost::interprocess::interprocess_exception&) {
        res = false;
    }
    return res;
}
//=============================================================================
}
//=============================================================================
