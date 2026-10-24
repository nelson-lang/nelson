//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <complex>
#include "nlsBuildConfig.h"
#include "Variance.hpp"
#include "VarianceOptions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
VarianceReal(const T* sp, T* dp, int planes, int planesize, int linesize, int w, bool omitNaN)
{
    ompIndexType i;
    ompIndexType j;
    ompIndexType k;
#if WITH_OPENMP
#pragma omp parallel for private(j, k) if (planes > OMP_DEFAULT_THRESHOLD)
#endif
    for (i = 0; i < (ompIndexType)planes; i++) {
        for (j = 0; j < (ompIndexType)planesize; j++) {
            if (!omitNaN) {
                // Keep the original per-term arithmetic: callers with hard-coded
                // references rely on the exact rounding.
                T accum_first = 0;
                for (k = 0; k < (ompIndexType)linesize; k++) {
                    accum_first += sp[i * planesize * linesize + j + k * planesize] / (T)linesize;
                }
                double denom = (w == 1) ? (double)linesize : (double)linesize - 1.0;
                if (denom <= 0) {
                    T v = sp[i * planesize * linesize + j] - accum_first;
                    dp[i * planesize + j] = (T)(v * v);
                    continue;
                }
                T accum_second = 0;
                for (k = 0; k < (ompIndexType)linesize; k++) {
                    T tmp = sp[i * planesize * linesize + j + k * planesize] - accum_first;
                    accum_second += (T)(tmp * tmp / denom);
                }
                dp[i * planesize + j] = (T)accum_second;
                continue;
            }
            double sum = 0;
            double count = 0;
            for (k = 0; k < (ompIndexType)linesize; k++) {
                double val = (double)sp[i * planesize * linesize + j + k * planesize];
                if (std::isnan(val)) {
                    continue;
                }
                sum += val;
                count += 1;
            }
            if (count == 0) {
                dp[i * planesize + j] = (T)std::nan("");
                continue;
            }
            double mean = sum / count;
            double accum = 0;
            for (k = 0; k < (ompIndexType)linesize; k++) {
                double val = (double)sp[i * planesize * linesize + j + k * planesize];
                if (std::isnan(val)) {
                    continue;
                }
                double tmp = val - mean;
                accum += tmp * tmp;
            }
            double denom = (w == 1) ? count : count - 1;
            if (denom <= 0) {
                // single observation: variance is 0 (or NaN if value is NaN)
                dp[i * planesize + j] = (T)accum;
            } else {
                dp[i * planesize + j] = (T)(accum / denom);
            }
        }
    }
}
//=============================================================================
template <class T>
void
VarianceComplex(
    const std::complex<T>* sp, T* dp, int planes, int planesize, int linesize, int w, bool omitNaN)
{
    ompIndexType i;
    ompIndexType j;
    ompIndexType k;
#if WITH_OPENMP
#pragma omp parallel for private(j, k) if (planes > OMP_DEFAULT_THRESHOLD)
#endif
    for (i = 0; i < (ompIndexType)planes; i++) {
        for (j = 0; j < (ompIndexType)planesize; j++) {
            std::complex<double> sum(0., 0.);
            double count = 0;
            for (k = 0; k < (ompIndexType)linesize; k++) {
                std::complex<T> z = sp[i * planesize * linesize + j + k * planesize];
                if (omitNaN && (std::isnan((double)z.real()) || std::isnan((double)z.imag()))) {
                    continue;
                }
                sum += std::complex<double>((double)z.real(), (double)z.imag());
                count += 1;
            }
            if (count == 0) {
                dp[i * planesize + j] = (T)std::nan("");
                continue;
            }
            std::complex<double> mean = sum / count;
            double accum = 0;
            for (k = 0; k < (ompIndexType)linesize; k++) {
                std::complex<T> z = sp[i * planesize * linesize + j + k * planesize];
                if (omitNaN && (std::isnan((double)z.real()) || std::isnan((double)z.imag()))) {
                    continue;
                }
                double dr = (double)z.real() - mean.real();
                double di = (double)z.imag() - mean.imag();
                // |z - mean|^2 : variance of complex data is real and >= 0
                accum += dr * dr + di * di;
            }
            double denom = (w == 1) ? count : count - 1;
            if (denom <= 0) {
                dp[i * planesize + j] = (T)accum;
            } else {
                dp[i * planesize + j] = (T)(accum / denom);
            }
        }
    }
}
//=============================================================================
template <class T>
void
VarianceWeightedReal(
    const T* sp, T* dp, int planes, int planesize, int linesize, const double* wgt, bool omitNaN)
{
    ompIndexType i;
    ompIndexType j;
    ompIndexType k;
#if WITH_OPENMP
#pragma omp parallel for private(j, k) if (planes > OMP_DEFAULT_THRESHOLD)
#endif
    for (i = 0; i < (ompIndexType)planes; i++) {
        for (j = 0; j < (ompIndexType)planesize; j++) {
            double sumWeights = 0;
            double sumWeightedValues = 0;
            for (k = 0; k < (ompIndexType)linesize; k++) {
                double val = (double)sp[i * planesize * linesize + j + k * planesize];
                if (omitNaN && std::isnan(val)) {
                    continue;
                }
                double wk = wgt[k];
                sumWeights += wk;
                sumWeightedValues += wk * val;
            }
            if (sumWeights == 0) {
                dp[i * planesize + j] = (T)std::nan("");
                continue;
            }
            double mean = sumWeightedValues / sumWeights;
            double accum = 0;
            for (k = 0; k < (ompIndexType)linesize; k++) {
                double val = (double)sp[i * planesize * linesize + j + k * planesize];
                if (omitNaN && std::isnan(val)) {
                    continue;
                }
                double tmp = val - mean;
                accum += wgt[k] * tmp * tmp;
            }
            dp[i * planesize + j] = (T)(accum / sumWeights);
        }
    }
}
//=============================================================================
template <class T>
void
VarianceWeightedComplex(const std::complex<T>* sp, T* dp, int planes, int planesize, int linesize,
    const double* wgt, bool omitNaN)
{
    ompIndexType i;
    ompIndexType j;
    ompIndexType k;
#if WITH_OPENMP
#pragma omp parallel for private(j, k) if (planes > OMP_DEFAULT_THRESHOLD)
#endif
    for (i = 0; i < (ompIndexType)planes; i++) {
        for (j = 0; j < (ompIndexType)planesize; j++) {
            std::complex<double> sum(0., 0.);
            double sumWeights = 0;
            for (k = 0; k < (ompIndexType)linesize; k++) {
                std::complex<T> z = sp[i * planesize * linesize + j + k * planesize];
                if (omitNaN && (std::isnan((double)z.real()) || std::isnan((double)z.imag()))) {
                    continue;
                }
                double wk = wgt[k];
                sum += std::complex<double>(wk * (double)z.real(), wk * (double)z.imag());
                sumWeights += wk;
            }
            if (sumWeights == 0) {
                dp[i * planesize + j] = (T)std::nan("");
                continue;
            }
            std::complex<double> mean = sum / sumWeights;
            double accum = 0;
            for (k = 0; k < (ompIndexType)linesize; k++) {
                std::complex<T> z = sp[i * planesize * linesize + j + k * planesize];
                if (omitNaN && (std::isnan((double)z.real()) || std::isnan((double)z.imag()))) {
                    continue;
                }
                double dr = (double)z.real() - mean.real();
                double di = (double)z.imag() - mean.imag();
                accum += wgt[k] * (dr * dr + di * di);
            }
            dp[i * planesize + j] = (T)(accum / sumWeights);
        }
    }
}
//=============================================================================
ArrayOf
Variance(const ArrayOf& A, int w, int dim, bool omitNaN, bool& needToOverload)
{
    needToOverload = false;
    if (w != 0 && w != 1) {
        Error(L"Nelson:statistics:invalidValue:wrongValueNArgument01",
            L"Wrong value for #%d argument: 0 or 1 expected.", 2);
    }
    ArrayOf res;
    if (dim < 0 && A.isEmpty(true)) {
        // var([]) is NaN; with a dimension, [] follows the reduction rule
        if (A.dataClass() == NLS_SINGLE || A.dataClass() == NLS_SCOMPLEX) {
            res = ArrayOf::singleScalar((single)std::nan(""));
        } else {
            res = ArrayOf::doubleScalar(std::nan(""));
        }
        return res;
    }
    Dimensions dimsA = A.dimensions();
    if (dim < 0) {
        // Bound the scan by the actual number of dimensions: the non-const
        // Dimensions::operator[] beyond length_ grows the dimension vector and
        // corrupts the output shape of all-singleton (scalar) inputs.
        int d = 0;
        int nd = (int)dimsA.length();
        while (d < nd && dimsA[d] == 1) {
            d++;
        }
        if (d == nd) {
            dim = 0;
        } else {
            dim = d;
        }
    } else {
        dim = dim - 1;
    }
    Dimensions outDim(dimsA);
    outDim[dim] = 1;
    outDim.simplify();
    int linesize = (int)dimsA[dim];
    int planesize = 1;
    for (int k = 0; k < dim; k++) {
        planesize *= (int)dimsA[k];
    }
    int planecount = 1;
    for (int k = dim + 1; k < (int)dimsA.length(); k++) {
        planecount *= (int)dimsA[k];
    }
    if (linesize == 0) {
        // No observations along the operating dimension: the variance of each
        // slice is NaN, and reading the (empty) data would be out of bounds.
        bool single = (A.dataClass() == NLS_SINGLE || A.dataClass() == NLS_SCOMPLEX);
        NelsonType outClass = single ? NLS_SINGLE : NLS_DOUBLE;
        void* ptr = ArrayOf::allocateArrayOf(outClass, outDim.elementCount());
        indexType n = outDim.elementCount();
        if (single) {
            for (indexType k = 0; k < n; ++k) {
                ((float*)ptr)[k] = (float)std::nan("");
            }
        } else {
            for (indexType k = 0; k < n; ++k) {
                ((double*)ptr)[k] = std::nan("");
            }
        }
        return ArrayOf(outClass, outDim, ptr);
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
        VarianceReal<double>(
            (const double*)A.dataPointer(), ptr, planecount, planesize, linesize, w, omitNaN);
    } break;
    case NLS_SINGLE: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, outDim.elementCount());
        res = ArrayOf(NLS_SINGLE, outDim, ptr);
        VarianceReal<single>(
            (const single*)A.dataPointer(), ptr, planecount, planesize, linesize, w, omitNaN);
    } break;
    case NLS_DCOMPLEX: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
        auto* ptrZ = reinterpret_cast<std::complex<double>*>((double*)A.dataPointer());
        VarianceComplex<double>(ptrZ, ptr, planecount, planesize, linesize, w, omitNaN);
    } break;
    case NLS_SCOMPLEX: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, outDim.elementCount());
        res = ArrayOf(NLS_SINGLE, outDim, ptr);
        auto* ptrZ = reinterpret_cast<std::complex<single>*>((single*)A.dataPointer());
        VarianceComplex<single>(ptrZ, ptr, planecount, planesize, linesize, w, omitNaN);
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    return res;
}
//=============================================================================
ArrayOf
Variance(const ArrayOf& A, const ArrayOf& weights, int dim, bool omitNaN, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf res;
    if (dim < 0 && A.isEmpty(true)) {
        if (A.dataClass() == NLS_SINGLE || A.dataClass() == NLS_SCOMPLEX) {
            res = ArrayOf::singleScalar((single)std::nan(""));
        } else {
            res = ArrayOf::doubleScalar(std::nan(""));
        }
        return res;
    }
    Dimensions dimsA = A.dimensions();
    if (dim < 0) {
        int d = 0;
        int nd = (int)dimsA.length();
        while (d < nd && dimsA[d] == 1) {
            d++;
        }
        dim = (d == nd) ? 0 : d;
    } else {
        dim = dim - 1;
    }
    int linesize = (int)dimsA[dim];
    ArrayOf w(weights);
    w.promoteType(NLS_DOUBLE);
    if ((int)w.elementCount() != linesize) {
        Error(
            L"Nelson:statistics:weightSizeMismatch", L"The length of W must be compatible with X.");
    }
    const double* wgt = (const double*)w.dataPointer();
    Dimensions outDim(dimsA);
    outDim[dim] = 1;
    int planesize = 1;
    for (int k = 0; k < dim; k++) {
        planesize *= (int)dimsA[k];
    }
    int planecount = 1;
    for (int k = dim + 1; k < (int)dimsA.length(); k++) {
        planecount *= (int)dimsA[k];
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
        VarianceWeightedReal<double>(
            (const double*)A.dataPointer(), ptr, planecount, planesize, linesize, wgt, omitNaN);
    } break;
    case NLS_SINGLE: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, outDim.elementCount());
        res = ArrayOf(NLS_SINGLE, outDim, ptr);
        VarianceWeightedReal<single>(
            (const single*)A.dataPointer(), ptr, planecount, planesize, linesize, wgt, omitNaN);
    } break;
    case NLS_DCOMPLEX: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.elementCount());
        res = ArrayOf(NLS_DOUBLE, outDim, ptr);
        auto* ptrZ = reinterpret_cast<std::complex<double>*>((double*)A.dataPointer());
        VarianceWeightedComplex<double>(ptrZ, ptr, planecount, planesize, linesize, wgt, omitNaN);
    } break;
    case NLS_SCOMPLEX: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, outDim.elementCount());
        res = ArrayOf(NLS_SINGLE, outDim, ptr);
        auto* ptrZ = reinterpret_cast<std::complex<single>*>((single*)A.dataPointer());
        VarianceWeightedComplex<single>(ptrZ, ptr, planecount, planesize, linesize, wgt, omitNaN);
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    return res;
}
//=============================================================================
ArrayOf
Variance(
    const ArrayOf& A, int w, const std::vector<indexType>& dims, bool omitNaN, bool& needToOverload)
{
    needToOverload = false;
    if (w != 0 && w != 1) {
        Error(L"Nelson:statistics:invalidValue:wrongValueNArgument01",
            L"Wrong value for #%d argument: 0 or 1 expected.", 2);
    }
    ArrayOf collapsed;
    Dimensions outDim;
    int singleDim = 0;
    if (!CollapseOperatingDimensions(A, dims, collapsed, outDim, singleDim, needToOverload)) {
        if (needToOverload) {
            return ArrayOf();
        }
        return Variance(A, w, singleDim, omitNaN, needToOverload);
    }
    // Reducing over several dimensions is the variance over all elements
    // spanning them, which is not the same as successive single-dimension
    // variances, so the elements are gathered first.
    ArrayOf res = Variance(collapsed, w, 1, omitNaN, needToOverload);
    if (needToOverload) {
        return ArrayOf();
    }
    res.reshape(outDim);
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
