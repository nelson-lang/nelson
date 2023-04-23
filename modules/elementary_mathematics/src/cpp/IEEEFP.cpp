//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <cstdint>
#include "nlsBuildConfig.h"
#if defined(_NLS_WITH_OPENMP)
#include <omp.h>
#endif
#include "IEEEFP.hpp"
//=============================================================================
bool
IsInfinite(float t)
{
    return std::isinf(t);
}
//=============================================================================
bool
IsInfinite(double t)
{
    return std::isinf(t);
}
//=============================================================================
bool
IsNaN(int t)
{
    return true;
}
//=============================================================================
bool
IsNaN(unsigned int t)
{
    return true;
}
//=============================================================================
bool
IsNaN(float t)
{
    return std::isnan(t);
}
//=============================================================================
bool
IsNaN(double t)
{
    return std::isnan(t);
}
//=============================================================================
bool
IsFinite(float t)
{
    return std::isfinite(t);
}
//=============================================================================
bool
IsFinite(double t)
{
    return std::isfinite(t);
}
//=============================================================================
template <class T>
bool
IsIntegerForm(T t)
{
    return (rint((long double)(t)) == (long double)(t) && IsFinite(t));
}
//=============================================================================
bool
IsIntegerForm(double t)
{
    return IsIntegerForm<double>(t);
}
//=============================================================================
bool
IsIntegerForm(float t)
{
    return IsIntegerForm<float>(t);
}
//=============================================================================
template <class T>
bool
IsIntegerFormOrNotFinite(T t)
{
    return (rint((long double)(t)) == (long double)(t) || !IsFinite(t));
}
//=============================================================================
bool
IsIntegerFormOrNotFinite(double t)
{
    return IsIntegerFormOrNotFinite<double>(t);
}
//=============================================================================
bool
IsIntegerFormOrNotFinite(float t)
{
    return IsIntegerFormOrNotFinite<float>(t);
}
//=============================================================================
#define NB_ELEMENTS_MAX_ONE_THREAD 1000
//=============================================================================
template <class T>
bool
IsIntegerForm(const T* t, size_t nbElements)
{
    if (t != nullptr && nbElements > 0) {
        if (nbElements < NB_ELEMENTS_MAX_ONE_THREAD) {
            for (size_t k = 0; k < nbElements; k++) {
                if (!IsIntegerForm(t[k])) {
                    return false;
                }
            }
            return true;
        } else {
            bool result = true;
#ifdef _NLS_WITH_OPENMP
#pragma omp parallel for shared(result)
#endif
            for (long long k = 0; k < (long long)nbElements; k++) {
                if (!IsIntegerForm(t[k])) {
                    result = false;
                }
            }
            return result;
        }
    }
    return false;
}
//=============================================================================
bool
IsIntegerForm(const float* t, size_t nbElements)
{
    return IsIntegerForm<float>(t, nbElements);
}
//=============================================================================
bool
IsIntegerForm(const double* t, size_t nbElements)
{
    return IsIntegerForm<double>(t, nbElements);
}
//=============================================================================
template <class T>
bool
IsIntegerFormOrNotFinite(const T* t, size_t nbElements)
{
    if (t != nullptr && nbElements > 0) {
        if (nbElements < NB_ELEMENTS_MAX_ONE_THREAD) {
            for (size_t k = 0; k < nbElements; k++) {
                if (!IsIntegerFormOrNotFinite(t[k])) {
                    return false;
                }
            }
            return true;
        } else {
            bool result = true;
#ifdef _NLS_WITH_OPENMP
#pragma omp parallel for reduction(&& : result)
#endif
            for (long long k = 0; k < (long long)nbElements; k++) {
                if (!IsIntegerFormOrNotFinite(t[k])) {
#ifdef _NLS_WITH_OPENMP
#pragma omp critical
#endif
                    {
                        result = false;
                    }
                }
            }
            return result;
        }
    }
    return false;
}
//=============================================================================
bool
IsIntegerFormOrNotFinite(const double* t, size_t nbElements)
{
    return IsIntegerFormOrNotFinite<double>(t, nbElements);
}
//=============================================================================
bool
IsIntegerFormOrNotFinite(const float* t, size_t nbElements)
{
    return IsIntegerFormOrNotFinite<float>(t, nbElements);
}
//=============================================================================
