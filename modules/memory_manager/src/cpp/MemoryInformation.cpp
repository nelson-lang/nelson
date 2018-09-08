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
#ifdef _MSC_VER
#include <Windows.h>
#include <psapi.h>
#else
#if defined(__APPLE__) || defined(__MACH__)
#include <mach/mach.h>
#include <mach/mach_host.h>
#include <mach/mach_init.h>
#include <mach/mach_types.h>
#include <mach/vm_statistics.h>
#include <sys/mount.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/sysctl.h>
#include <sys/types.h>
#else
#include <errno.h>
#include <fstream>
#include <iostream>
#include <stdint.h>
#include <sys/resource.h>
#include <sys/sysctl.h>
#include <sys/sysinfo.h>
#include <sys/types.h>
#include <unistd.h>
#endif

#endif
#include "MemoryInformation.hpp"
//=============================================================================
namespace Nelson {
#ifdef _MSC_VER
//=============================================================================
double
getTotalVirtualMemory()
{
    double res;
    MEMORYSTATUSEX memInfo;
    memInfo.dwLength = sizeof(MEMORYSTATUSEX);
    GlobalMemoryStatusEx(&memInfo);
    DWORDLONG totalVirtualMem = memInfo.ullTotalPageFile;
    res = (double)totalVirtualMem;
    return res;
}
//=============================================================================
double
getTotalVirtualMemoryUsed()
{
    double res;
    MEMORYSTATUSEX memInfo;
    memInfo.dwLength = sizeof(MEMORYSTATUSEX);
    GlobalMemoryStatusEx(&memInfo);
    DWORDLONG totalVirtualMemUsed = memInfo.ullTotalPageFile - memInfo.ullAvailPageFile;
    res = (double)totalVirtualMemUsed;
    return res;
}
//=============================================================================
double
getTotalVirtualMemoryByNelson()
{
    double res;
    PROCESS_MEMORY_COUNTERS_EX pmc;
    GetProcessMemoryInfo(GetCurrentProcess(), (PROCESS_MEMORY_COUNTERS*)&pmc, sizeof(pmc));
    SIZE_T virtualMemUsedByMe = pmc.PrivateUsage;
    res = (double)virtualMemUsedByMe;
    return res;
}
//=============================================================================
double
getTotalPhysicalMemory()
{
    double res;
    MEMORYSTATUSEX memInfo;
    memInfo.dwLength = sizeof(MEMORYSTATUSEX);
    GlobalMemoryStatusEx(&memInfo);
    DWORDLONG totalPhysMem = memInfo.ullTotalPhys;
    res = (double)totalPhysMem;
    return res;
}
//=============================================================================
double
getTotalPhysicalMemoryUsed()
{
    double res;
    MEMORYSTATUSEX memInfo;
    memInfo.dwLength = sizeof(MEMORYSTATUSEX);
    GlobalMemoryStatusEx(&memInfo);
    DWORDLONG physMemUsed = memInfo.ullTotalPhys - memInfo.ullAvailPhys;
    res = (double)physMemUsed;
    return res;
}
//=============================================================================
double
getTotalPhysicalMemoryByNelson()
{
    double res;
    PROCESS_MEMORY_COUNTERS_EX pmc;
    GetProcessMemoryInfo(GetCurrentProcess(), (PROCESS_MEMORY_COUNTERS*)&pmc, sizeof(pmc));
    SIZE_T physMemUsedByMe = pmc.WorkingSetSize;
    res = (double)physMemUsedByMe;
    return res;
}
//=============================================================================
#else
double
getTotalVirtualMemory()
{
    double res = 0;
#if defined(__APPLE__) || defined(__MACH__)
    struct statfs stats;
    if (0 == statfs("/", &stats)) {
        res = (double)((uint64_t)stats.f_bsize * stats.f_bfree);
    }
#else
    struct sysinfo si;
    sysinfo(&si);
    res = (double)(si.totalswap);
#endif
    return res;
}
//=============================================================================
double
getTotalVirtualMemoryUsed()
{
    double res;
#if defined(__APPLE__) || defined(__MACH__)
    xsw_usage vmusage = { 0 };
    size_t size = sizeof(vmusage);
    if (sysctlbyname("vm.swapusage", &vmusage, &size, NULL, 0) != 0) {
        res = 0;
    } else {
        res = (double)vmusage.xsu_used;
    }
#else
    struct sysinfo si;
    sysinfo(&si);
    res = (double)(si.totalswap - si.freeswap);
#endif
    return res;
}
//=============================================================================
double
getTotalVirtualMemoryByNelson()
{
    double res = 0;
#if defined(__APPLE__) || defined(__MACH__)
    struct task_basic_info t_info;
    mach_msg_type_number_t t_info_count = TASK_BASIC_INFO_COUNT;
    if (KERN_SUCCESS
        != task_info(mach_task_self(), TASK_BASIC_INFO, (task_info_t)&t_info, &t_info_count)) {
        return 0;
    }
    res = (double)t_info.virtual_size;
#else
    std::ifstream f;
    f.open("/proc/self/stat", std::ifstream::in);
    if (f.is_open()) {
        int pid;
        char name[1024];
        char state;
        int ppid, pgrp, session, tty, tpgid;
        unsigned flags, minflt, cminflt, majflt, cmajflt;
        int utime, stime, cutime, cstime, counter, priority, timeout;
        unsigned itrealvalue, starttime;
        int vsize = 0, rss = 0;
        f >> pid >> name >> state >> ppid >> pgrp >> session >> tty >> tpgid
            >> flags; // lgtm [cpp/dangerous-cin]
        f >> minflt >> cminflt >> majflt >> cmajflt >> utime >> stime >> cutime;
        f >> cstime >> counter >> priority >> timeout >> itrealvalue >> starttime;
        f >> vsize >> rss;
        res = (double)vsize;
    }
#endif
    return res;
}
//=============================================================================
double
getTotalPhysicalMemory()
{
    double res = 0;
#if defined(__APPLE__) || defined(__MACH__)
    unsigned int physmem;
    size_t len = sizeof physmem;
    static int mib[2] = { CTL_HW, HW_PHYSMEM };
    if (sysctl(mib, 2, &physmem, &len, NULL, 0) == 0 && len == sizeof(physmem)) {
        res = (double)physmem;
    }
#else
    struct sysinfo si;
    sysinfo(&si);
    res = (double)(si.totalram);
    //		res = (double)(sysconf(_SC_PHYS_PAGES) * sysconf(_SC_PAGESIZE));
#endif
    return res;
}
//=============================================================================
double
getTotalPhysicalMemoryUsed()
{
    double res = 0;
#if defined(__APPLE__) || defined(__MACH__)
    vm_size_t page_size;
    mach_port_t mach_port;
    mach_msg_type_number_t count;
    vm_statistics64_data_t vm_stats;
    mach_port = mach_host_self();
    count = sizeof(vm_stats) / sizeof(natural_t);
    if (KERN_SUCCESS == host_page_size(mach_port, &page_size)
        && KERN_SUCCESS
            == host_statistics64(mach_port, HOST_VM_INFO, (host_info64_t)&vm_stats, &count)) {
        long long used_memory = ((int64_t)vm_stats.active_count + (int64_t)vm_stats.inactive_count
                                    + (int64_t)vm_stats.wire_count)
            * (int64_t)page_size;
        res = (double)used_memory;
    }
#else
    struct sysinfo si;
    sysinfo(&si);
    res = (double)(si.totalram - si.freeram);
#endif
    return res;
}
//=============================================================================
double
getTotalPhysicalMemoryByNelson()
{
    double res = 0;
#if defined(__APPLE__) || defined(__MACH__)
    struct task_basic_info t_info;
    mach_msg_type_number_t t_info_count = TASK_BASIC_INFO_COUNT;
    if (KERN_SUCCESS
        != task_info(mach_task_self(), TASK_BASIC_INFO, (task_info_t)&t_info, &t_info_count)) {
        return 0;
    }
    res = (double)t_info.resident_size;
#else
    errno = 0;
    struct rusage r_usage;
    getrusage(RUSAGE_SELF, &r_usage);
    if ((errno == EFAULT) || (errno == EINVAL)) {
        res = 0;
    } else {
        res = (double)r_usage.ru_maxrss * 1024;
    }
#endif
    return res;
}
//=============================================================================
#endif
}
//=============================================================================
