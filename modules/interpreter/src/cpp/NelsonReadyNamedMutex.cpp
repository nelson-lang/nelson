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
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "NelsonReadyNamedMutex.hpp"
#if defined(_MSC_VER)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#elif !defined(__EMSCRIPTEN__)
#include <semaphore.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
#if defined(_MSC_VER)
static HANDLE nelson_ready_mutex = nullptr;
#elif defined(__EMSCRIPTEN__)
static bool nelson_ready_mutex = false;
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
#if defined(_MSC_VER)
    if (nelson_ready_mutex == nullptr) {
        HANDLE hOld = OpenMutexA(SYNCHRONIZE | MUTEX_MODIFY_STATE, FALSE, name.c_str());
        if (hOld) {
            CloseHandle(hOld);
        }
        HANDLE h = CreateMutexA(nullptr, TRUE, name.c_str());
        if (h != nullptr) {
            nelson_ready_mutex = h;
            res = true;
        } else {
            res = false;
        }
    }
#elif defined(__EMSCRIPTEN__)
    nelson_ready_mutex = true;
    res = true;
#else
    // POSIX named semaphore: use name starting with '/'
    std::string sname = std::string("/") + name;
    if (nelson_ready_sem == nullptr) {
        sem_unlink(sname.c_str());
        sem_t* s = sem_open(sname.c_str(), O_CREAT | O_EXCL, 0600, 0);
        if (s == SEM_FAILED) {
            s = sem_open(sname.c_str(), O_CREAT, 0600, 0);
            if (s == SEM_FAILED) {
                res = false;
            } else {
                nelson_ready_sem = s;
                res = true;
            }
        } else {
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
#if defined(_MSC_VER)
    if (nelson_ready_mutex) {
        // ReleaseMutex fails when the mutex is not owned; the error is ignored.
        ReleaseMutex(nelson_ready_mutex);
        CloseHandle(nelson_ready_mutex);
        nelson_ready_mutex = nullptr;
        res = true;
    }
#elif defined(__EMSCRIPTEN__)
    res = nelson_ready_mutex;
    nelson_ready_mutex = false;
#else
    std::string sname = std::string("/") + name;
    if (nelson_ready_sem) {
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
#if defined(_MSC_VER)
    HANDLE h = OpenMutexA(SYNCHRONIZE | MUTEX_MODIFY_STATE, FALSE, name.c_str());
    if (h) {
        CloseHandle(h);
        res = true;
    } else {
        res = false;
    }
#elif defined(__EMSCRIPTEN__)
    res = nelson_ready_mutex;
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
