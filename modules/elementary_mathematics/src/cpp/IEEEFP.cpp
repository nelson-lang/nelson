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
#if WITH_OPENMP
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
template <class T>
bool
IsIntegerFormNoOpenMP(const T* t, size_t nbElements)
{
    if (t != nullptr && nbElements > 0) {
        for (size_t k = 0; k < nbElements; k++) {
            if (!IsIntegerForm(t[k])) {
                return false;
            }
        }
        return true;
    }
    return false;
}
//=============================================================================
template <class T>
bool
IsIntegerFormOpenMP(const T* t, size_t nbElements)
{
    if (t != nullptr && nbElements > 0) {
        switch (nbElements) {
        case 0: {
            return false;
        } break;
        case 1: {
            return IsIntegerForm(t[0]);
        } break;
        default: {
            bool result = true;
#if WITH_OPENMP
#pragma omp parallel for shared(result) if (nbElements > OMP_DEFAULT_THRESHOLD)
#endif
            for (long long k = 0; k < (long long)nbElements; k++) {
                if (!IsIntegerForm(t[k])) {
                    result = false;
                }
            }
            return result;
        } break;
        }
    }
    return false;
}
//=============================================================================
template <class T>
bool
IsIntegerForm(const T* t, size_t nbElements)
{
#if WITH_OPENMP
    return IsIntegerFormOpenMP<T>(t, nbElements);
#else
    return IsIntegerFormNoOpenMP<T>(t, nbElements);
#endif
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
IsIntegerFormOrNotFiniteNoOpenMP(const T* t, size_t nbElements)
{
    if (t != nullptr && nbElements > 0) {
        for (size_t k = 0; k < nbElements; k++) {
            if (!IsIntegerFormOrNotFinite(t[k])) {
                return false;
            }
        }
        return true;
    }
    return false;
}
//=============================================================================
template <class T>
bool
IsIntegerFormOrNotFiniteOpenMP(const T* t, size_t nbElements)
{
    if (t != nullptr && nbElements > 0) {
        bool result = true;
#pragma omp parallel for reduction(&& : result) schedule(static) if (nbElements > OMP_DEFAULT_THRESHOLD)
        for (long long k = 0; k < (long long)nbElements; k++) {
            if (!IsIntegerFormOrNotFinite(t[k])) {
                result = false;
            }
        }
        return result;
    }
    return false;
}
//=============================================================================
template <class T>
bool
IsIntegerFormOrNotFinite(const T* t, size_t nbElements)
{
#if WITH_OPENMP
    return IsIntegerFormOrNotFiniteOpenMP<T>(t, nbElements);
#else
    return IsIntegerFormOrNotFiniteNoOpenMP<T>(t, nbElements);
#endif
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
