//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _WIN32
#include <windows.h>
#undef max
#elif __APPLE__
#include <sys/param.h>
#include <sys/sysctl.h>
#else
#include <unistd.h>
#endif
#include <Eigen/Dense>
#include <boost/lexical_cast.hpp>
#include <algorithm>
#include "nlsConfig.h"
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
    const BOOL result_first
        = GetLogicalProcessorInformationEx(RelationProcessorCore, nullptr, &length);
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
#if defined(_NLS_WITH_OPENMP)
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
        try {
            nbOfThreadsToUse = boost::lexical_cast<unsigned int>(omp_env.c_str());
        } catch (const boost::bad_lexical_cast&) {
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
