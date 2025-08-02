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
#include <boost/interprocess/shared_memory_object.hpp>
#include <boost/interprocess/sync/named_mutex.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
// http://www.vincenzo.net/isxkb/index.php?title=Application_considerations
// creates a named mutex used by Innosetup
//=============================================================================
static boost::interprocess::named_mutex* nelson_mutex = nullptr;
//=============================================================================
bool
openNelsonMutex()
{
    bool res = false;
    if (nelson_mutex == nullptr) {
        try {
            nelson_mutex = new boost::interprocess::named_mutex(
                boost::interprocess::open_or_create, NELSON_VERSION_NMMM_STRING);
            res = true;
        } catch (boost::interprocess::interprocess_exception&) {
            res = false;
        }
    }
    return res;
}
//=============================================================================
bool
closeNelsonMutex()
{
    bool res = false;
    if (nelson_mutex) {
        nelson_mutex->remove(NELSON_VERSION_NMMM_STRING);
        delete nelson_mutex;
        nelson_mutex = nullptr;
    }
    return res;
}
//=============================================================================
bool
haveNelsonMutex()
{
    bool res = false;
    try {
        boost::interprocess::named_mutex other_nelson_mutex(
            boost::interprocess::open_only, NELSON_VERSION_NMMM_STRING);
        res = true;
    } catch (const boost::interprocess::interprocess_exception&) {
        res = false;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
