//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
bool
getFiniteMinMax(const T* val, indexType nbElements, T& min, T& max, bool& isFinite)
{
    T minValue = (T)std::nan("NaN");
    T maxValue = (T)std::nan("NaN");
    T shared_max = (T)std::nan("NaN");
    T shared_min = (T)std::nan("NaN");
    isFinite = true;

    for (indexType k = 0; k < nbElements; ++k) {
        if (std::isfinite(val[k])) {
            minValue = val[k];
            maxValue = val[k];
            shared_max = maxValue;
            shared_min = minValue;
            break;
        }
    }
    if (std::isnan(shared_min) && std::isnan(shared_max)) {
        min = shared_min;
        max = shared_max;
        isFinite = false;
        return false;
    }
#if WITH_OPENMP
#pragma omp parallel
#endif
    {
#if WITH_OPENMP
#pragma omp for nowait
#endif
        for (ompIndexType idx = 0; idx < (ompIndexType)nbElements; ++idx) {
            if (std::isfinite(val[idx])) {
                maxValue = std::max(val[idx], maxValue);
                minValue = std::min(val[idx], minValue);
            } else {
                isFinite = false;
            }
        }
#if WITH_OPENMP
#pragma omp critical
#endif
        {
            shared_max = std::max(shared_max, maxValue);
            shared_min = std::min(shared_min, minValue);
        }
    }
    min = shared_min;
    max = shared_max;
    return true;
}
//=============================================================================
}
//=============================================================================
