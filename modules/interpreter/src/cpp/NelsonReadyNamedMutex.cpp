//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include "NelsonReadyNamedMutex.hpp"
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
static HANDLE nelson_ready_mutex = nullptr;
#else
static sem_t* nelson_ready_sem = nullptr;
#endif
//=============================================================================
static std::string
getNamedMutex(int pid)
{
    return std::string("NELSON_READY_") + fmt::to_string(pid);
}
//=============================================================================
bool
openIsReadyNelsonMutex(int pid)
{
    bool res = false;
    std::string name = getNamedMutex(pid);
#ifdef _MSC_VER
    if (nelson_ready_mutex == nullptr) {
        // Try to remove any existing handle by opening and closing it
        HANDLE hOld = OpenMutexA(SYNCHRONIZE | MUTEX_MODIFY_STATE, FALSE, name.c_str());
        if (hOld) {
            CloseHandle(hOld);
        }
        // Create a named mutex and take initial ownership (locked)
        HANDLE h = CreateMutexA(nullptr, TRUE, name.c_str());
        if (h != nullptr) {
            nelson_ready_mutex = h;
            res = true;
        } else {
            res = false;
        }
    }
#else
    // POSIX named semaphore: use name starting with '/'
    std::string sname = std::string("/") + name;
    if (nelson_ready_sem == nullptr) {
        // unlink any previous semaphore with same name
        sem_unlink(sname.c_str());
        // create semaphore with initial value 0 so it's considered 'locked'
        sem_t* s = sem_open(sname.c_str(), O_CREAT | O_EXCL, 0600, 0);
        if (s == SEM_FAILED) {
            // If already exists try opening without O_EXCL
            s = sem_open(sname.c_str(), O_CREAT, 0600, 0);
            if (s == SEM_FAILED) {
                res = false;
            } else {
                nelson_ready_sem = s;
                res = true;
            }
        } else {
            // successfully created
            nelson_ready_sem = s;
            res = true;
        }
    }
#endif
    return res;
}
//=============================================================================
bool
closeIsReadyNelsonMutex(int pid)
{
    bool res = false;
    std::string name = getNamedMutex(pid);
#ifdef _MSC_VER
    if (nelson_ready_mutex) {
        // Release ownership if we hold it
        // It's safe to call ReleaseMutex only if we own it. Try, ignore errors.
        ReleaseMutex(nelson_ready_mutex);
        CloseHandle(nelson_ready_mutex);
        nelson_ready_mutex = nullptr;
        res = true;
    }
#else
    std::string sname = std::string("/") + name;
    if (nelson_ready_sem) {
        // unlink the named semaphore and close our handle
        sem_unlink(sname.c_str());
        sem_close(nelson_ready_sem);
        nelson_ready_sem = nullptr;
        res = true;
    }
#endif
    return res;
}
//=============================================================================
bool
haveIsReadyNelsonMutex(int pid)
{
    bool res = false;
    std::string name = getNamedMutex(pid);
#ifdef _MSC_VER
    // Try to open existing named mutex without creating it
    HANDLE h = OpenMutexA(SYNCHRONIZE | MUTEX_MODIFY_STATE, FALSE, name.c_str());
    if (h) {
        CloseHandle(h);
        res = true;
    } else {
        res = false;
    }
#else
    std::string sname = std::string("/") + name;
    // Try to open existing semaphore without creating it
    sem_t* s = sem_open(sname.c_str(), 0);
    if (s != SEM_FAILED) {
        sem_close(s);
        res = true;
    } else {
        // errno == ENOENT means does not exist
        res = false;
    }
#endif
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
