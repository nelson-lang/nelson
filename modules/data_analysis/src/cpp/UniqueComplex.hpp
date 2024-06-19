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
#include "ArrayOf.hpp"
#include <vector>
#include <algorithm>
#include <omp.h>
#include "ParallelSort.hpp"
#include "complex_abs.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T> class UniqueComplexEntry
{
public:
    ompIndexType n;
    std::complex<T> value;
};
//=============================================================================
template <class T>
bool
operator<(const UniqueComplexEntry<T>& a, const UniqueComplexEntry<T>& b)
{

    if ((std::isnan(b.value.real()) || std::isnan(b.value.imag()))
        && (!std::isnan(a.value.real()) && !std::isnan(a.value.imag()))) {
        return true;
    }
    return complex_abs(a.value.real(), a.value.imag())
        < complex_abs(b.value.real(), b.value.imag());
}
//=============================================================================
template <class T>
bool
operator==(const UniqueComplexEntry<T>& a, const UniqueComplexEntry<T>& b)
{
    return a.value == b.value;
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueComplexOneLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType len = input.getElementCount();
    const T* dp = static_cast<const T*>(input.getDataPointer());
    auto* ptrDpz = reinterpret_cast<std::complex<T>*>((T*)dp);

    std::vector<UniqueComplexEntry<T>> values(len);
#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < len; ++k) {
        values[k].value = ptrDpz[k];
    }
    parallelSort(values);
    values.erase(std::unique(values.begin(), values.end()), values.end());

    ArrayOfVector retval;
    NelsonType cls(input.getDataClass());
    T* op = (T*)ArrayOf::allocateArrayOf(cls, values.size());
    auto* ptrOpz = reinterpret_cast<std::complex<T>*>((T*)op);

#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)values.size(); k++) {
        ptrOpz[k] = values[k].value;
    }
    Dimensions dimsOut = isRowVector ? Dimensions(1, values.size()) : Dimensions(values.size(), 1);
    retval << ArrayOf(cls, dimsOut, op);
    return retval;
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueComplexTwoLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType len = input.getElementCount();
    const T* dp = (T*)input.getDataPointer();
    auto* ptrDpz = reinterpret_cast<std::complex<T>*>((T*)dp);

    std::vector<UniqueComplexEntry<T>> values(len);
#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < len; ++k) {
        values[k].value = ptrDpz[k];
        values[k].n = (ompIndexType)k + 1;
    }
    parallelSort(values);
    values.erase(std::unique(values.begin(), values.end()), values.end());

    ArrayOfVector retval;
    NelsonType cls(input.getDataClass());
    double* mp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, values.size());
    T* op = (T*)ArrayOf::allocateArrayOf(cls, values.size());
    auto* ptrOpz = reinterpret_cast<std::complex<T>*>((T*)op);

#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)values.size(); k++) {
        ptrOpz[k] = values[k].value;
        mp[k] = (double)values[k].n;
    }

    Dimensions dimsOut = isRowVector ? Dimensions(1, values.size()) : Dimensions(values.size(), 1);
    retval << ArrayOf(cls, dimsOut, op);
    retval << ArrayOf(NLS_DOUBLE, dimsOut, mp);

    return retval;
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueComplexThreeLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType rows = input.getRows();
    indexType cols = input.getColumns();
    rows = input.getElementCount();
    cols = 1;

    const T* dp = (const T*)input.getDataPointer();
    auto* ptrDpz = reinterpret_cast<std::complex<T>*>((T*)dp);
    ompIndexType len = input.getElementCount();
    std::vector<UniqueComplexEntry<T>> values(len);
#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < len; ++k) {
        values[k].n = k;
        values[k].value = ptrDpz[k];
    }
    parallelSort(values);
    ompIndexType cnt = len > 0 ? 1 : 0;
    for (ompIndexType i = 1; i < len; ++i) {
        if (!(values[i] == values[i - 1])) {
            cnt++;
        }
    }

    double* np = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len);
    double* mp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, cnt);
    T* op = (T*)ArrayOf::allocateArrayOf(input.getDataClass(), cnt);
    auto* ptrOpz = reinterpret_cast<std::complex<T>*>((T*)op);
    if (len > 0) {
        ptrOpz[0] = dp[values[0].n];
        cnt = 1;
        np[values[0].n] = 1;
        mp[0] = (double)(values[0].n + 1);
        for (int i = 1; i < len; i++) {
            if (!(values[i] == values[i - 1])) {
                ptrOpz[cnt] = dp[values[i].n];
                mp[cnt] = (double)(values[i].n + 1);
                cnt++;
            }
            np[values[i].n] = (double)cnt;
        }
    }
    ArrayOfVector retval;
    Dimensions dimsOut = isRowVector ? Dimensions(1, cnt) : Dimensions(cnt, 1);
    if (len == 0) {
        if (input.getRows() == input.getColumns()) {
            dimsOut = Dimensions(0, 1);
        } else {
            dimsOut = Dimensions(input.getRows() != 0 ? 1 : 0, input.getColumns() != 0 ? 1 : 0);
        }
    }
    retval << ArrayOf(input.getDataClass(), dimsOut, op);
    retval << ArrayOf(NLS_DOUBLE, Dimensions(cnt, 1), mp);
    retval << ArrayOf(NLS_DOUBLE, Dimensions(len, 1), np);
    return retval;
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueComplex(const ArrayOf& input, int nLhs)
{
    if (nLhs <= 1) {
        return UniqueComplexOneLhs<T>(input);
    }
    if (nLhs == 2) {
        return UniqueComplexTwoLhs<T>(input);
    }
    return UniqueComplexThreeLhs<T>(input);
}
//=============================================================================
}
//=============================================================================
