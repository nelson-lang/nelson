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
#include <boost/version.hpp>
#include <boost/interprocess/managed_shared_memory.hpp>
#if BOOST_VERSION >= 108800
#include <boost/process/v1/child.hpp>
#else
#include <boost/process.hpp>
#endif
#ifndef _MSC_VER
#include <sys/utsname.h>
#endif
#include "NelsonPIDs.hpp"
#include "GetUsername.hpp"
#include "characters_encoding.hpp"
//=============================================================================
#if BOOST_VERSION >= 108800
#define PROCESS_PID_T boost::process::v1::pid_t
#define PROCESS_CHILD boost::process::v1::child
#else
#define PROCESS_PID_T boost::process::pid_t
#define PROCESS_CHILD boost::process::child
#endif
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
        PROCESS_PID_T _pid = (PROCESS_PID_T)pID;
        PROCESS_CHILD child(_pid);
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
        std::string arch; // Declare the variable here

#ifdef _MSC_VER
#ifdef _WIN64
        arch = "win64";
#else
        arch = "win32";
#endif
#elif defined(__linux__)
        // Get more specific Linux architecture information
        struct utsname sysInfo;
        if (uname(&sysInfo) == 0) {
            arch = std::string("linux_") + sysInfo.machine;
        } else {
            arch = "linux";
        }
#else
        arch = "other";
#endif
        channelName = std::string(NELSON_PIDS) + "_" + arch + "_" + wstring_to_utf8(GetUsername());
#ifndef _MSC_VER
        // Add process owner uid to avoid permission issues on Linux (NixOS)
        uid_t uid = geteuid();
        channelName += "_" + std::to_string(getCurrentPID());
#else
        channelName += "_" + std::to_string(getCurrentPID());
#endif
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
