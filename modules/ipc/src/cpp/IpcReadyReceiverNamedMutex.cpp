//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "IpcReadyReceiverNamedMutex.hpp"
#include <string>
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <semaphore.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
#ifdef _MSC_VER
static HANDLE ipc_receiver_ready_mutex = nullptr;
#else
static sem_t* ipc_receiver_ready_sem = nullptr;
#endif
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
    if (
#ifdef _MSC_VER
        ipc_receiver_ready_mutex == nullptr
#else
        ipc_receiver_ready_sem == nullptr
#endif
    ) {
        std::string name = getNamedMutex(pid);
#ifdef _MSC_VER
        // Create or open named mutex (unowned)
        HANDLE h = CreateMutexA(nullptr, FALSE, name.c_str());
        if (h != nullptr) {
            ipc_receiver_ready_mutex = h;
            res = true;
        } else {
            res = false;
        }
#else
        // POSIX named semaphore: name must start with '/'
        std::string sname = std::string("/") + name;
        // Remove any existing semaphore with that name then create
        sem_unlink(sname.c_str());
        sem_t* s = sem_open(sname.c_str(), O_CREAT | O_EXCL, 0600, 1);
        if (s == SEM_FAILED) {
            // Try open without O_EXCL if already exists
            s = sem_open(sname.c_str(), O_CREAT, 0600, 1);
            if (s == SEM_FAILED) {
                res = false;
            } else {
                ipc_receiver_ready_sem = s;
                res = true;
            }
        } else {
            ipc_receiver_ready_sem = s;
            res = true;
        }
#endif
    }
    return res;
}
//=============================================================================
bool
closeIpcReceiverIsReadyMutex(int pid)
{
    bool res = false;
    std::string name = getNamedMutex(pid);
#ifdef _MSC_VER
    if (ipc_receiver_ready_mutex) {
        CloseHandle(ipc_receiver_ready_mutex);
        ipc_receiver_ready_mutex = nullptr;
        res = true;
    }
#else
    if (ipc_receiver_ready_sem) {
        std::string sname = std::string("/") + name;
        sem_unlink(sname.c_str());
        sem_close(ipc_receiver_ready_sem);
        ipc_receiver_ready_sem = nullptr;
        res = true;
    }
#endif
    return res;
}
//=============================================================================
bool
haveIpcReceiverIsReadyMutex(int pid)
{
    bool res = false;
    std::string name = getNamedMutex(pid);
#ifdef _MSC_VER
    HANDLE h = OpenMutexA(SYNCHRONIZE, FALSE, name.c_str());
    if (h) {
        CloseHandle(h);
        res = true;
    } else {
        res = false;
    }
#else
    std::string sname = std::string("/") + name;
    sem_t* s = sem_open(sname.c_str(), 0);
    if (s != SEM_FAILED) {
        sem_close(s);
        res = true;
    } else {
        res = false;
    }
#endif
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
