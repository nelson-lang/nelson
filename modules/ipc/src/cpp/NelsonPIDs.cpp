//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if _MSC_VER
#define _WIN32_WINNT 0x0550
#endif
//=============================================================================
#include <boost/interprocess/managed_shared_memory.hpp>
#include <boost/process.hpp>
#include "NelsonPIDs.hpp"
#include "GetUsername.hpp"
#include "characters_encoding.hpp"
//=============================================================================
#define MAX_NB_PIDS 256
#define NELSON_PIDS "NELSON_PIDS"
#define PIDS_ID_DATA "PIDS_ID_DATA"
#define PIDS_MODE_DATA "PIDS_MODE_DATA"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::string channelName;
//=============================================================================
bool
isPIDRunning(int pID)
{
    if (pID <= 0) {
        return false;
    }
    try {

        boost::process::pid_t _pid = (boost::process::pid_t)pID;
        boost::process::child child(_pid);
        return child.valid();
    } catch (const std::runtime_error&) {
        return false;
    }
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
static std::string
buildNelsonPIDsChannelName()
{
    if (channelName.empty()) {
#ifdef _MSC_VER
#ifdef _WIN64
        std::string arch = "win64";
#else
        std::string arch = "win32";
#endif
#else
        std::string arch = "other";
#endif
        channelName = std::string(NELSON_PIDS) + "_" + arch + "_" + wstring_to_utf8(GetUsername());
    }
    return channelName;
}
//=============================================================================
static bool
needToCreateSharedMemory()
{
    bool bNeedToCreate = false;
    try {
        boost::interprocess::managed_shared_memory managed_shm { boost::interprocess::open_only,
            buildNelsonPIDsChannelName().c_str() };
        bNeedToCreate = false;
    } catch (boost::interprocess::interprocess_exception&) {
        bNeedToCreate = true;
    }
    return bNeedToCreate;
}
//=============================================================================
bool
registerPidInSharedMemory(int pid, NELSON_ENGINE_MODE mode)
{
    bool needToCreate = needToCreateSharedMemory();
    try {
        size_t size_shm = sizeof(int*) * MAX_NB_PIDS * 2 + 1024;
        boost::interprocess::managed_shared_memory managed_shm {
            boost::interprocess::open_or_create, buildNelsonPIDsChannelName().c_str(), size_shm
        };
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
            boost::interprocess::shared_memory_object::remove(buildNelsonPIDsChannelName().c_str());
        }

    } catch (boost::interprocess::interprocess_exception&) {
        boost::interprocess::shared_memory_object::remove(buildNelsonPIDsChannelName().c_str());
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
            boost::interprocess::shared_memory_object::remove(buildNelsonPIDsChannelName().c_str());
            return true;
        }
        try {
            boost::interprocess::managed_shared_memory managed_shm { boost::interprocess::open_only,
                buildNelsonPIDsChannelName().c_str() };
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
        boost::interprocess::managed_shared_memory managed_shm {
            boost::interprocess::open_read_only, buildNelsonPIDsChannelName().c_str()
        };
        std::pair<int*, std::size_t> pValues = managed_shm.find<int>(PIDS_ID_DATA);

        int* pids = pValues.first;
        for (int j = 0; j < MAX_NB_PIDS; j++) {
            if (pids[j] != 0) {
                if (isPIDRunning(pids[j])) {
                    PIDs.push_back(pids[j]);
                }
            }
        }
    } catch (boost::interprocess::interprocess_exception&) {
    }
    return PIDs;
}
//=============================================================================
std::vector<NELSON_ENGINE_MODE>
getNelsonPIDModes()
{
    std::vector<NELSON_ENGINE_MODE> Modes;
    try {
        boost::interprocess::managed_shared_memory managed_shm {
            boost::interprocess::open_read_only, buildNelsonPIDsChannelName().c_str()
        };
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
    } catch (boost::interprocess::interprocess_exception&) {
    }
    return Modes;
}
//=============================================================================
int
getLatestPidInSharedMemory()
{
    int pid = 0;
    try {
        boost::interprocess::managed_shared_memory managed_shm {
            boost::interprocess::open_read_only, buildNelsonPIDsChannelName().c_str()
        };
        std::pair<int*, std::size_t> pIDs = managed_shm.find<int>(PIDS_ID_DATA);
        int* pids = pIDs.first;

        for (int j = MAX_NB_PIDS - 1; j >= 0; j--) {
            if (isPIDRunning(pids[j])) {
                return pids[j];
            }
        }
    } catch (boost::interprocess::interprocess_exception&) {
    }
    return 0;
}
//=============================================================================
int
getLatestPidWithModeInSharedMemory(NELSON_ENGINE_MODE _mode)
{
    int pid = 0;
    try {
        boost::interprocess::managed_shared_memory managed_shm {
            boost::interprocess::open_read_only, buildNelsonPIDsChannelName().c_str()
        };
        std::pair<int*, std::size_t> pIDs = managed_shm.find<int>(PIDS_ID_DATA);
        std::pair<int*, std::size_t> pModes = managed_shm.find<int>(PIDS_MODE_DATA);

        int* pids = pIDs.first;
        int* pmodes = pModes.first;

        for (int j = MAX_NB_PIDS - 1; j >= 0; j--) {
            if (isPIDRunning(pids[j]) && pmodes[j] == (int)_mode) {
                return pids[j];
            }
        }
    } catch (boost::interprocess::interprocess_exception&) {
    }
    return 0;
}
//=============================================================================
}
//=============================================================================
