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
#define OMP_PARALLEL_FOR_LOOP(elementCount)                                                        \
  _Pragma(STRINGIFY(omp parallel for if(NelsonConfiguration::getInstance()->isOpenMPParallelizationEnabled() && (uint64_t)elementCount > NelsonConfiguration::getInstance()->getOpenMPParallelizationThreshold())))
#else
#define OMP_PARALLEL_FOR_LOOP(elementCount)
#endif
//=============================================================================
