//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/Dense>
#include <boost/lexical_cast.hpp>
#include <boost/thread/thread.hpp>
#include "nlsConfig.h"
#include "ComputionalThreads.hpp"
#include "GetVariableEnvironment.hpp"
#include "SetVariableEnvironment.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static unsigned int nbOfThreadsToUse = 0;
//=============================================================================
unsigned int
getMaxNumCompThreads()
{
    return nbOfThreadsToUse;
}
//=============================================================================
unsigned int
setMaxNumCompThreads(unsigned int _nbOfCores)
{
    unsigned int previousValue = nbOfThreadsToUse;
    nbOfThreadsToUse = _nbOfCores;
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
    if (omp_env == L"0") {
        nbOfThreadsToUse = boost::thread::hardware_concurrency();
    } else {
        try {
            nbOfThreadsToUse = boost::lexical_cast<unsigned int>(omp_env.c_str());
        } catch (const boost::bad_lexical_cast&) {
            nbOfThreadsToUse = 0;
        }
        if (nbOfThreadsToUse == 0) {
            nbOfThreadsToUse = boost::thread::hardware_concurrency();
        }
    }
    setMaxNumCompThreads(nbOfThreadsToUse);
    return nbOfThreadsToUse;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
