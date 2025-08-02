//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
