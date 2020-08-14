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
#include <boost/interprocess/managed_shared_memory.hpp>
#if _MSC_VER
#include <Windows.h>
#else
#include <signal.h>
#endif
#include "NelsonPIDs.hpp"
//=============================================================================
#define MAX_NB_PIDS 256
#define NELSON_PIDS "NELSON_PIDS"
#define PIDS_ID_DATA "PIDS_ID_DATA"
#define PIDS_MODE_DATA "PIDS_MODE_DATA"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
isPIDRunning(int pID)
{
    if (pID <= 0) {
        return false;
    }
#if _MSC_VER
    HANDLE handle = OpenProcess(SYNCHRONIZE, 0, pID);
    if (handle == nullptr) {
        return false;
    }
    bool alive = WaitForSingleObject(handle, 0) == WAIT_TIMEOUT;
    CloseHandle(handle);
    return alive;

#else
    if (kill(pID, 0) == -1) {
        return false;
    } else {
        return true;
    }
#endif
}
//=============================================================================
int
getCurrentPID()
{
    return (int)boost::interprocess::ipcdetail::get_current_process_id();
}
//=============================================================================
static void
removeOldPIDs(int* pids)
{
    if (pids != nullptr) {
        for (int j = 0; j < MAX_NB_PIDS; j++) {
            if (pids[j] != 0) {
                if (!isPIDRunning(pids[j])) {
                    pids[j] = 0;
                }
            }
        }
    }
}
//=============================================================================
static bool
needToCreateSharedMemory()
{
    bool bExist = false;
    try {
        boost::interprocess::managed_shared_memory managed_shm{ boost::interprocess::open_only,
            NELSON_PIDS };
        bExist = false;
    } catch (boost::interprocess::interprocess_exception&) {
        bExist = true;
    }
    return bExist;
}
//=============================================================================
bool
registerPidInSharedMemory(int pid, NELSON_ENGINE_MODE mode)
{
    bool needToCreate = needToCreateSharedMemory();
    try {
        size_t size_shm = sizeof(int*) * MAX_NB_PIDS * 2 + 100;
        boost::interprocess::managed_shared_memory managed_shm{ boost::interprocess::open_or_create,
            NELSON_PIDS, size_shm };
        int* pids = nullptr;
        int* modes = nullptr;
        int index = 0;
        if (needToCreate) {
            pids = managed_shm.construct<int>(PIDS_ID_DATA)[MAX_NB_PIDS](0);
            modes = managed_shm.construct<int>(PIDS_MODE_DATA)[MAX_NB_PIDS](-1);
            for (int j = 0; j < MAX_NB_PIDS; j++) {
                pids[j] = 0;
            }
        } else {
            std::pair<int*, std::size_t> pPIDs = managed_shm.find<int>(PIDS_ID_DATA);
            std::pair<int*, std::size_t> pModes = managed_shm.find<int>(PIDS_MODE_DATA);
            pids = pPIDs.first;
            modes = pModes.first;
            bool indexModified = false;
            removeOldPIDs(pids);
            if (pids != nullptr) {
                for (int j = 0; j < MAX_NB_PIDS; j++) {
                    if (pids[j] == 0) {
                        index = j;
                        indexModified = true;
                        break;
                    }
                }
            }
            if (!indexModified) {
                index = MAX_NB_PIDS - 1;
            }
        }
        if (pids != nullptr) {
            pids[index] = pid;
        }
        if (modes != nullptr) {
            modes[index] = (int)mode;
        }
        if (pids == nullptr || modes == nullptr) {
            boost::interprocess::shared_memory_object::remove(NELSON_PIDS);
        }

    } catch (boost::interprocess::interprocess_exception&) {
        boost::interprocess::shared_memory_object::remove(NELSON_PIDS);
        return false;
    }
    return true;
}
//=============================================================================
bool
unregisterPidInSharedMemory(int pid)
{
    bool needToCreate = needToCreateSharedMemory();
    if (!needToCreate) {
        std::vector<int> PIDs = getNelsonPIDs();
        if (PIDs.empty() || (PIDs.size() == 1 && PIDs[0] == pid)) {
            boost::interprocess::shared_memory_object::remove(NELSON_PIDS);
            return true;
        }
        try {
            boost::interprocess::managed_shared_memory managed_shm{ boost::interprocess::open_only,
                NELSON_PIDS };
            std::pair<int*, std::size_t> pPIDs = managed_shm.find<int>(PIDS_ID_DATA);
            int* pids = pPIDs.first;
            if (pids != nullptr) {
                for (int j = 0; j < MAX_NB_PIDS; j++) {
                    if (pids[j] == pid) {
                        pids[j] = 0;
                    }
                }
            }
            return true;
        } catch (boost::interprocess::interprocess_exception&) {
        }
    }
    return false;
}
//=============================================================================
std::vector<int>
getNelsonPIDs()
{
    std::vector<int> PIDs;
    try {
        boost::interprocess::managed_shared_memory managed_shm{ boost::interprocess::open_read_only,
            NELSON_PIDS };
        std::pair<int*, std::size_t> pValues = managed_shm.find<int>(PIDS_ID_DATA);

        int* pids = pValues.first;
        for (int j = 0; j < MAX_NB_PIDS; j++) {
            if (pids[j] != 0) {
                if (isPIDRunning(pids[j])) {
                    PIDs.push_back(pids[j]);
                }
            }
        }
    } catch (boost::interprocess::interprocess_exception& ex) {
        ex.get_error_code();
    }
    return PIDs;
}
//=============================================================================
std::vector<NELSON_ENGINE_MODE>
getNelsonPIDModes()
{
    std::vector<NELSON_ENGINE_MODE> Modes;
    try {
        boost::interprocess::managed_shared_memory managed_shm{ boost::interprocess::open_read_only,
            NELSON_PIDS };
        std::pair<int*, std::size_t> pIDs = managed_shm.find<int>(PIDS_ID_DATA);
        std::pair<int*, std::size_t> pModes = managed_shm.find<int>(PIDS_MODE_DATA);

        int* pids = pIDs.first;
        int* pmodes = pModes.first;
        for (int j = 0; j < MAX_NB_PIDS; j++) {
            if (pids[j] != 0) {
                if (isPIDRunning(pids[j])) {
                    Modes.push_back((NELSON_ENGINE_MODE)pmodes[j]);
                }
            }
        }
    } catch (boost::interprocess::interprocess_exception& ex) {
        ex.get_error_code();
    }
    return Modes;
}
//=============================================================================
int
getLatestPidWithModeInSharedMemory(NELSON_ENGINE_MODE _mode)
{
    int pid = 0;
    try {
        boost::interprocess::managed_shared_memory managed_shm{ boost::interprocess::open_read_only,
            NELSON_PIDS };
        std::pair<int*, std::size_t> pIDs = managed_shm.find<int>(PIDS_ID_DATA);
        std::pair<int*, std::size_t> pModes = managed_shm.find<int>(PIDS_MODE_DATA);

        int* pids = pIDs.first;
        int* pmodes = pModes.first;

        for (int j = MAX_NB_PIDS - 1; j >= 0; j--) {
            if (isPIDRunning(pids[j]) && pmodes[j] == (int)_mode) {
                return pids[j];
            }
        }
    } catch (boost::interprocess::interprocess_exception& ex) {
        ex.get_error_code();
    }
    return 0;
}
//=============================================================================
}
//=============================================================================
