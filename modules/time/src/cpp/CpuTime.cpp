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
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#else
#include <sys/resource.h>
#include <sys/times.h>
#include <ctime>
#include <unistd.h>
#endif
#include "CpuTime.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
double
CpuTime()
{
#if defined(_MSC_VER)
    FILETIME createTime;
    FILETIME exitTime;
    FILETIME kernelTime;
    FILETIME userTime;
    if (GetProcessTimes(GetCurrentProcess(), &createTime, &exitTime, &kernelTime, &userTime)
        != -1) {
        SYSTEMTIME userSystemTime;
        if (FileTimeToSystemTime(&userTime, &userSystemTime) != -1) {
            return static_cast<double>(userSystemTime.wHour) * 3600.0
                + static_cast<double>(userSystemTime.wMinute) * 60.0
                + static_cast<double>(userSystemTime.wSecond)
                + static_cast<double>(userSystemTime.wMilliseconds) / 1000.0;
        }
    }
#else
#if defined(_POSIX_TIMERS) && (_POSIX_TIMERS > 0)
    /* Prefer high-res POSIX timers, when available. */
    {
        clockid_t id;
        struct timespec ts;
#if _POSIX_CPUTIME > 0
        /* Clock ids vary by OS.  Query the id, if possible. */
        if (clock_getcpuclockid(0, &id) == -1)
#endif
#if defined(CLOCK_PROCESS_CPUTIME_ID)
            /* Use known clock id for AIX, Linux, or Solaris. */
            id = CLOCK_PROCESS_CPUTIME_ID;
#elif defined(CLOCK_VIRTUAL)
        /* Use known clock id for BSD or HP-UX. */
        id = CLOCK_VIRTUAL;
#else
        id = (clockid_t)-1;
#endif
        if (id != (clockid_t)-1 && clock_gettime(id, &ts) != -1)
            return (double)ts.tv_sec + (double)ts.tv_nsec / 1000000000.0;
    }
#endif
#if defined(RUSAGE_SELF)
    {
        struct rusage rusage;
        if (getrusage(RUSAGE_SELF, &rusage) != -1)
            return (double)rusage.ru_utime.tv_sec + (double)rusage.ru_utime.tv_usec / 1000000.0;
    }
#endif
#if defined(_SC_CLK_TCK)
    {
        const double ticks = (double)sysconf(_SC_CLK_TCK);
        struct tms tms;
        if (times(&tms) != (clock_t)-1) {
            return (double)tms.tms_utime / ticks;
        }
    }
#endif
#if defined(CLOCKS_PER_SEC)
    {
        clock_t cl = clock();
        if (cl != (clock_t)-1) {
            return (double)cl / (double)CLOCKS_PER_SEC;
        }
    }
#endif
#endif
    return -1; /* Failed. */
}
//=============================================================================
} // namespace Nelson
//=============================================================================
