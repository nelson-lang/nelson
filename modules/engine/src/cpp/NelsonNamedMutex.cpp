//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonNamedMutex.hpp"
#include "Nelson_VERSION.h"
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <semaphore.h>
#include <fcntl.h>
#include <errno.h>
#include <string>
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
#ifdef _MSC_VER
static HANDLE nelson_mutex = nullptr;
#else
static sem_t* nelson_sem = nullptr;
#endif
//=============================================================================
bool
openNelsonMutex()
{
    bool res = false;
#ifdef _MSC_VER
    if (nelson_mutex == nullptr) {
        // Create or open named mutex (unowned)
        HANDLE h = CreateMutexA(nullptr, FALSE, NELSON_VERSION_NMMM_STRING);
        if (h != nullptr) {
            nelson_mutex = h;
            res = true;
        } else {
            res = false;
        }
    } else {
        res = true;
    }
#else
    if (nelson_sem == nullptr) {
        // POSIX named semaphore name must start with '/'
        std::string name = std::string("/") + NELSON_VERSION_NMMM_STRING;
        // Try to create with initial value 1 (unlocked). If exists, open it.
        sem_t* s = sem_open(name.c_str(), O_CREAT | O_EXCL, 0600, 1);
        if (s == SEM_FAILED) {
            // If already exists, open without O_EXCL
            s = sem_open(name.c_str(), O_CREAT, 0600, 1);
            if (s == SEM_FAILED) {
                res = false;
            } else {
                nelson_sem = s;
                res = true;
            }
        } else {
            nelson_sem = s;
            res = true;
        }
    } else {
        res = true;
    }
#endif
    return res;
}
//=============================================================================
bool
closeNelsonMutex()
{
    bool res = false;
#ifdef _MSC_VER
    if (nelson_mutex) {
        CloseHandle(nelson_mutex);
        nelson_mutex = nullptr;
        res = true;
    }
#else
    if (nelson_sem) {
        std::string name = std::string("/") + NELSON_VERSION_NMMM_STRING;
        // unlink removes the named semaphore; close our descriptor
        sem_unlink(name.c_str());
        sem_close(nelson_sem);
        nelson_sem = nullptr;
        res = true;
    }
#endif
    return res;
}
//=============================================================================
bool
haveNelsonMutex()
{
    bool res = false;
#ifdef _MSC_VER
    // Try to open existing named mutex without creating it
    HANDLE h = OpenMutexA(SYNCHRONIZE, FALSE, NELSON_VERSION_NMMM_STRING);
    if (h) {
        CloseHandle(h);
        res = true;
    } else {
        res = false;
    }
#else
    std::string name = std::string("/") + NELSON_VERSION_NMMM_STRING;
    sem_t* s = sem_open(name.c_str(), 0);
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
