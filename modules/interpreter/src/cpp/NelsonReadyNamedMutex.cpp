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
#include <boost/interprocess/shared_memory_object.hpp>
#include <boost/interprocess/sync/named_mutex.hpp>
#include "NelsonReadyNamedMutex.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static boost::interprocess::named_mutex* nelson_ready_mutex = nullptr;
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
    if (nelson_ready_mutex == nullptr) {
        std::string name = getNamedMutex(pid);
        try {
            boost::interprocess::named_mutex::remove(name.c_str());
            nelson_ready_mutex = new boost::interprocess::named_mutex(
                boost::interprocess::open_or_create, name.c_str());
            nelson_ready_mutex->lock();
            res = true;
        } catch (boost::interprocess::interprocess_exception&) {
            res = false;
        }
    }
    return res;
}
//=============================================================================
bool
closeIsReadyNelsonMutex(int pid)
{
    bool res = false;
    if (nelson_ready_mutex) {
        std::string name = getNamedMutex(pid);
        nelson_ready_mutex->remove(name.c_str());
        delete nelson_ready_mutex;
        nelson_ready_mutex = nullptr;
    }
    return res;
}
//=============================================================================
bool
haveIsReadyNelsonMutex(int pid)
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
