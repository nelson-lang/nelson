//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0601
#endif
#endif
//=============================================================================
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <string>
#ifdef _MSC_VER
#include <windows.h>
#else
#include <sys/utsname.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <csignal>
#include <cerrno>
#endif
#include "NelsonPIDs.hpp"
#include "GetUsername.hpp"
#include "characters_encoding.hpp"
//=============================================================================
#define MAX_NB_PIDS 256
#define NELSON_PIDS "NELSON_PIDS"
//=============================================================================
// Fixed-layout PID registry. A single POD struct is mapped by every Nelson
// process. It is created on demand, freed by the OS when the last mapping
// handle closes (Windows) or unlinked by the last process to leave (POSIX).
#define NELSON_PIDS_REGISTRY_MAGIC 0x4450494Eu // 'N','I','P','D'
#define NELSON_PIDS_REGISTRY_VERSION 1u
//=============================================================================
namespace Nelson {
//=============================================================================
struct NelsonPidRegistry
{
    uint32_t magic;
    uint32_t version;
    int32_t pids[MAX_NB_PIDS];
    int32_t modes[MAX_NB_PIDS];
};
//=============================================================================
static std::string channelName;
#ifdef _MSC_VER
static HANDLE registryMappingHandle = nullptr;
#else
static int registryShmFd = -1;
#endif
static NelsonPidRegistry* registryView = nullptr;
//=============================================================================
static void
removeOldPIDs(int* pids);
static std::string
buildNelsonPIDsChannelName();
static std::string
registryObjectName();
static void
initializeRegistry(NelsonPidRegistry* registry);
static NelsonPidRegistry*
mapRegistry(bool createIfMissing);
static void
unmapRegistry(bool unlinkSegment);
static bool
registryHasReadableData(const NelsonPidRegistry* registry);
//=============================================================================
bool
isPIDRunning(int pID)
{
    if (pID <= 0) {
        return false;
    }
#ifdef _MSC_VER
    HANDLE process
        = OpenProcess(SYNCHRONIZE | PROCESS_QUERY_LIMITED_INFORMATION, FALSE, (DWORD)pID);
    if (process == nullptr) {
        // A process owned by another user denies access but still exists.
        return GetLastError() == ERROR_ACCESS_DENIED;
    }
    DWORD waitResult = WaitForSingleObject(process, 0);
    CloseHandle(process);
    return waitResult == WAIT_TIMEOUT;
#else
    if (kill((pid_t)pID, 0) == 0) {
        return true;
    }
    return errno == EPERM;
#endif
}
//=============================================================================
int
getCurrentPID()
{
#ifdef _MSC_VER
    return (int)GetCurrentProcessId();
#else
    return (int)getpid();
#endif
}
//=============================================================================
bool
registerPidInSharedMemory(int pid, NELSON_ENGINE_MODE mode)
{
    NelsonPidRegistry* registry = mapRegistry(true);
    if (registry == nullptr) {
        return false;
    }
    removeOldPIDs(registry->pids);
    int index = MAX_NB_PIDS - 1;
    for (int j = 0; j < MAX_NB_PIDS; ++j) {
        if (registry->pids[j] == 0) {
            index = j;
            break;
        }
    }
    registry->pids[index] = pid;
    registry->modes[index] = (int)mode;
    return true;
}
//=============================================================================
bool
unregisterPidInSharedMemory(int pid)
{
    NelsonPidRegistry* registry = mapRegistry(false);
    if (registry == nullptr) {
        return false;
    }
    for (int j = 0; j < MAX_NB_PIDS; ++j) {
        if (registry->pids[j] == pid) {
            registry->pids[j] = 0;
            registry->modes[j] = -1;
        }
    }
    bool anyAlive = false;
    for (int j = 0; j < MAX_NB_PIDS; ++j) {
        if (registry->pids[j] != 0 && isPIDRunning(registry->pids[j])) {
            anyAlive = true;
            break;
        }
    }
    if (!anyAlive) {
        unmapRegistry(true);
    }
    return true;
}
//=============================================================================
std::vector<int>
getNelsonPIDs()
{
    std::vector<int> PIDs;
    NelsonPidRegistry* registry = mapRegistry(false);
    if (!registryHasReadableData(registry)) {
        return PIDs;
    }
    for (int j = 0; j < MAX_NB_PIDS; ++j) {
        if (registry->pids[j] != 0 && isPIDRunning(registry->pids[j])) {
            PIDs.push_back(registry->pids[j]);
        }
    }
    return PIDs;
}
//=============================================================================
std::vector<NELSON_ENGINE_MODE>
getNelsonPIDModes()
{
    std::vector<NELSON_ENGINE_MODE> Modes;
    NelsonPidRegistry* registry = mapRegistry(false);
    if (!registryHasReadableData(registry)) {
        return Modes;
    }
    for (int j = 0; j < MAX_NB_PIDS; ++j) {
        if (registry->pids[j] != 0 && isPIDRunning(registry->pids[j])) {
            Modes.push_back((NELSON_ENGINE_MODE)registry->modes[j]);
        }
    }
    return Modes;
}
//=============================================================================
int
getLatestPidInSharedMemory()
{
    NelsonPidRegistry* registry = mapRegistry(false);
    if (!registryHasReadableData(registry)) {
        return 0;
    }
    for (int j = MAX_NB_PIDS - 1; j >= 0; --j) {
        if (registry->pids[j] != 0 && isPIDRunning(registry->pids[j])) {
            return registry->pids[j];
        }
    }
    return 0;
}
//=============================================================================
int
getLatestPidWithModeInSharedMemory(NELSON_ENGINE_MODE _mode)
{
    NelsonPidRegistry* registry = mapRegistry(false);
    if (!registryHasReadableData(registry)) {
        return 0;
    }
    for (int j = MAX_NB_PIDS - 1; j >= 0; --j) {
        if (registry->pids[j] != 0 && isPIDRunning(registry->pids[j])
            && registry->modes[j] == (int)_mode) {
            return registry->pids[j];
        }
    }
    return 0;
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
        std::string arch;

#ifdef _MSC_VER
#ifdef _WIN64
        arch = "win64";
#else
        arch = "win32";
#endif
#elif defined(__linux__)
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
        channelName += "_" + std::to_string((int)uid);
#endif
    }
    return channelName;
}
//=============================================================================
static std::string
registryObjectName()
{
    // Short, bumped name so this native layout never collides with an older
    // boost segment and fits the POSIX shm name limit (macOS: 31 characters).
    const std::string identity = buildNelsonPIDsChannelName();
    uint32_t hash = 2166136261u;
    for (unsigned char c : identity) {
        hash ^= c;
        hash *= 16777619u;
    }
    char buffer[24];
    std::snprintf(buffer, sizeof(buffer), "nls_pids2_%08x", hash);
    return std::string(buffer);
}
//=============================================================================
static void
initializeRegistry(NelsonPidRegistry* registry)
{
    for (int j = 0; j < MAX_NB_PIDS; ++j) {
        registry->pids[j] = 0;
        registry->modes[j] = -1;
    }
    registry->version = NELSON_PIDS_REGISTRY_VERSION;
    registry->magic = NELSON_PIDS_REGISTRY_MAGIC; // published last
}
//=============================================================================
static NelsonPidRegistry*
mapRegistry(bool createIfMissing)
{
    if (registryView != nullptr) {
        return registryView;
    }
#ifdef _MSC_VER
    const std::string name = "Local\\" + registryObjectName();
    HANDLE handle = nullptr;
    bool created = false;
    if (createIfMissing) {
        handle = CreateFileMappingA(INVALID_HANDLE_VALUE, nullptr, PAGE_READWRITE, 0,
            (DWORD)sizeof(NelsonPidRegistry), name.c_str());
        if (handle == nullptr) {
            return nullptr;
        }
        created = (GetLastError() != ERROR_ALREADY_EXISTS);
    } else {
        handle = OpenFileMappingA(FILE_MAP_ALL_ACCESS, FALSE, name.c_str());
        if (handle == nullptr) {
            return nullptr;
        }
    }
    auto* view = (NelsonPidRegistry*)MapViewOfFile(
        handle, FILE_MAP_ALL_ACCESS, 0, 0, sizeof(NelsonPidRegistry));
    if (view == nullptr) {
        CloseHandle(handle);
        return nullptr;
    }
    if (created) {
        initializeRegistry(view); // a fresh pagefile-backed mapping arrives zeroed
    }
    registryMappingHandle = handle;
    registryView = view;
#else
    const std::string name = "/" + registryObjectName();
    int fd = -1;
    bool created = false;
    if (createIfMissing) {
        fd = shm_open(name.c_str(), O_CREAT | O_EXCL | O_RDWR, 0600);
        if (fd >= 0) {
            created = true;
            if (ftruncate(fd, (off_t)sizeof(NelsonPidRegistry)) != 0) {
                ::close(fd);
                shm_unlink(name.c_str());
                return nullptr;
            }
        } else {
            fd = shm_open(name.c_str(), O_RDWR, 0600);
        }
    } else {
        fd = shm_open(name.c_str(), O_RDWR, 0600);
    }
    if (fd < 0) {
        return nullptr;
    }
    void* addr
        = mmap(nullptr, sizeof(NelsonPidRegistry), PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    if (addr == MAP_FAILED) {
        ::close(fd);
        if (created) {
            shm_unlink(name.c_str());
        }
        return nullptr;
    }
    auto* view = (NelsonPidRegistry*)addr;
    if (created) {
        initializeRegistry(view);
    }
    registryShmFd = fd;
    registryView = view;
#endif
    if (registryView->magic != NELSON_PIDS_REGISTRY_MAGIC && createIfMissing) {
        // Segment created by another process but not yet initialized (tiny
        // startup race); a writer safely initializes it.
        initializeRegistry(registryView);
    }
    return registryView;
}
//=============================================================================
static void
unmapRegistry(bool unlinkSegment)
{
#ifdef _MSC_VER
    (void)unlinkSegment; // the OS frees the mapping when the last handle closes
    if (registryView != nullptr) {
        UnmapViewOfFile(registryView);
        registryView = nullptr;
    }
    if (registryMappingHandle != nullptr) {
        CloseHandle(registryMappingHandle);
        registryMappingHandle = nullptr;
    }
#else
    const std::string name = "/" + registryObjectName();
    if (registryView != nullptr) {
        munmap(registryView, sizeof(NelsonPidRegistry));
        registryView = nullptr;
    }
    if (registryShmFd >= 0) {
        ::close(registryShmFd);
        registryShmFd = -1;
    }
    if (unlinkSegment) {
        shm_unlink(name.c_str());
    }
#endif
}
//=============================================================================
static bool
registryHasReadableData(const NelsonPidRegistry* registry)
{
    return registry != nullptr && registry->magic == NELSON_PIDS_REGISTRY_MAGIC;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
