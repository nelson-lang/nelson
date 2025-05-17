//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsBuildConfig.h"
#if WITH_OPENMP
#include "NelsonConfiguration.hpp"
#endif
//=============================================================================
#if WITH_OPENMP
#define STRINGIFY(x) #x
#define EXPAND(x) x

#define GET_MACRO(_1, _2, NAME, ...) NAME

#define OMP_PARALLEL_FOR_LOOP_1(elementCount)                                                                   \
  _Pragma(STRINGIFY(omp parallel for if(NelsonConfiguration::getInstance()->isOpenMPParallelizationEnabled() && \
                                        (uint64_t)(elementCount) > NelsonConfiguration::getInstance()->getOpenMPParallelizationThreshold())))

#define OMP_PARALLEL_FOR_LOOP_2(elementCount, numThreads)                                                                               \
  _Pragma(STRINGIFY(omp parallel for num_threads(numThreads) if(NelsonConfiguration::getInstance()->isOpenMPParallelizationEnabled() && \
                                        (uint64_t)(elementCount) > NelsonConfiguration::getInstance()->getOpenMPParallelizationThreshold())))
#define OMP_PARALLEL_FOR_LOOP(...)                                                                 \
    EXPAND(GET_MACRO(__VA_ARGS__, OMP_PARALLEL_FOR_LOOP_2, OMP_PARALLEL_FOR_LOOP_1)(__VA_ARGS__))

#else
#define OMP_PARALLEL_FOR_LOOP(...)
#endif
