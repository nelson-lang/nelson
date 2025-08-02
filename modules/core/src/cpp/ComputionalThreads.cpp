//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/Dense>

#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef max
#elif __APPLE__
#include <sys/param.h>
#include <sys/sysctl.h>
#else
#include <unistd.h>
#endif
#include "nlsBuildConfig.h"
#if WITH_OPENMP
#include <omp.h>
#endif
#include <algorithm>
#include "StringHelpers.hpp"
#include "ComputionalThreads.hpp"
#include "GetVariableEnvironment.hpp"
#include "SetVariableEnvironment.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#ifdef _MSC_VER
static int
getNumberOfLogicalCores()
{
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    int nbLogicalCores = (int)sysinfo.dwNumberOfProcessors;
    return std::max((int)1, nbLogicalCores);
}
#endif
//=============================================================================
static int
getNumberOfPhysicalCores()
{
    int nbPhysicalCores = 0;
#ifdef _MSC_VER
    DWORD length = 0;
    GetLogicalProcessorInformationEx(RelationProcessorCore, nullptr, &length);
    if (GetLastError() != ERROR_INSUFFICIENT_BUFFER) {
        return getNumberOfLogicalCores();
    }

    std::unique_ptr<uint8_t[]> buffer(new uint8_t[length]);
    const PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX info
        = reinterpret_cast<PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX>(buffer.get());

    const BOOL result_second
        = GetLogicalProcessorInformationEx(RelationProcessorCore, info, &length);
    if (result_second == FALSE) {
        return getNumberOfLogicalCores();
    }
    size_t offset = 0;
    do {
        const PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX current_info
            = reinterpret_cast<PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX>(buffer.get() + offset);
        offset += current_info->Size;
        ++nbPhysicalCores;
    } while (offset < length);

#ifdef _WIN64
#else
    if (nbPhysicalCores > 4) {
        nbPhysicalCores = 4;
    }
#endif
#elif __APPLE__
    int nm[2];
    size_t len = 4;
    uint32_t count;

    nm[0] = CTL_HW;
    nm[1] = HW_AVAILCPU;
    sysctl(nm, 2, &count, &len, NULL, 0);

    if (count < 1) {
        nm[1] = HW_NCPU;
        sysctl(nm, 2, &count, &len, NULL, 0);
        if (count < 1) {
            count = 1;
        }
    }
    nbPhysicalCores = count;
#else
    nbPhysicalCores = sysconf(_SC_NPROCESSORS_ONLN);
#endif
    return std::max((int)1, nbPhysicalCores);
}
//=============================================================================
unsigned int
setMaxNumCompThreads(unsigned int _nbOfCores)
{
    unsigned int previousValue = NelsonConfiguration::getInstance()->getMaxNumCompThreads();
    NelsonConfiguration::getInstance()->setMaxNumCompThreads(_nbOfCores);
#if WITH_OPENMP
    omp_set_num_threads(_nbOfCores);
#endif
    Eigen::setNbThreads(_nbOfCores);
    return previousValue;
}
//=============================================================================
unsigned int
setDefaultMaxNumCompThreads()
{
    std::wstring omp_env = GetVariableEnvironment(L"OMP_NUM_THREADS", L"0");
    int nbOfThreadsToUse = 1;
    if (omp_env == L"0") {
        nbOfThreadsToUse = getNumberOfPhysicalCores();
    } else {
        if (!StringHelpers::str2integer(omp_env, nbOfThreadsToUse)) {
            nbOfThreadsToUse = 0;
        }
        if (nbOfThreadsToUse == 0) {
            nbOfThreadsToUse = getNumberOfPhysicalCores();
        }
    }
    NelsonConfiguration::getInstance()->setMaxNumCompThreads(nbOfThreadsToUse);
    return nbOfThreadsToUse;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
