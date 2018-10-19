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
static boost::interprocess::shared_memory_object* nelson_shared = nullptr;
//=============================================================================
bool
openNelsonMutex()
{
    bool res = false;
    if (nelson_mutex == nullptr) {
        nelson_mutex = new boost::interprocess::named_mutex(
            boost::interprocess::open_or_create, NELSON_VERSION_NMMM_STRING);
        res = true;
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
}
//=============================================================================
