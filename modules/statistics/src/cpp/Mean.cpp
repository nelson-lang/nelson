//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "Mean.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
TMeanInteger(const T* sp, double* dp, indexType planes, indexType planesize, indexType linesize)
{
    for (indexType i = 0; i < planes; i++) {
        for (indexType j = 0; j < planesize; j++) {
            double accum = 0;
            for (indexType k = 0; k < linesize; k++) {
                T val = sp[i * planesize * linesize + j + k * planesize];
                accum += (double)val;
            }
            dp[i * planesize + j] = accum / (double)linesize;
        }
    }
}
//=============================================================================
template <class T>
void
TMeanReal(const T* sp, double* dp, indexType planes, indexType planesize, indexType linesize,
    bool omitNaN)
{
    if (omitNaN) {
        for (indexType i = 0; i < planes; i++) {
            for (indexType j = 0; j < planesize; j++) {
                double accum = 0.;
                double nbNotNaN = 0;
                for (indexType k = 0; k < linesize; k++) {
                    double val = (double)sp[i * planesize * linesize + j + k * planesize];
                    if (!std::isnan(val)) {
                        accum += val;
                        nbNotNaN += 1;
                    }
                }
                dp[i * planesize + j] = accum / nbNotNaN;
            }
        }
    } else {
        for (indexType i = 0; i < planes; i++) {
            for (indexType j = 0; j < planesize; j++) {
                double accum = 0.;
                for (indexType k = 0; k < linesize; k++) {
                    double val = (double)sp[i * planesize * linesize + j + k * planesize];
                    accum += val;
                }
                dp[i * planesize + j] = accum / (double)linesize;
            }
        }
    }
}
//=============================================================================
template <class T>
void
TMeanComplex(const T* sp, double* dp, indexType planes, indexType planesize, indexType linesize,
    bool omitNaN)
{
    if (omitNaN) {
        for (indexType i = 0; i < planes; i++) {
            for (indexType j = 0; j < planesize; j++) {
                double accum_r = 0;
                double accum_i = 0;
                double nbNotNaN = 0;
                for (indexType k = 0; k < linesize; k++) {
                    double vr = (double)sp[2 * (i * planesize * linesize + j + k * planesize)];
                    double vi = (double)sp[2 * (i * planesize * linesize + j + k * planesize) + 1];
                    if (!std::isnan(vr) && !std::isnan(vi)) {
                        accum_r += vr;
                        accum_i += vi;
                        nbNotNaN += 1;
                    }
                }
                dp[2 * (i * planesize + j)] = accum_r / (double)nbNotNaN;
                dp[2 * (i * planesize + j) + 1] = accum_i / (double)nbNotNaN;
            }
        }
    } else {
        for (size_t i = 0; i < planes; i++) {
            for (size_t j = 0; j < planesize; j++) {
                double accum_r = 0;
                double accum_i = 0;
                for (size_t k = 0; k < linesize; k++) {
                    accum_r = (double)sp[2 * (i * planesize * linesize + j + k * planesize)];
                    accum_i = (double)sp[2 * (i * planesize * linesize + j + k * planesize) + 1];
                }
                dp[2 * (i * planesize + j)] = accum_r / (double)linesize;
                dp[2 * (i * planesize + j) + 1] = accum_i / (double)linesize;
            }
        }
    }
}
//=============================================================================
template <class T>
double
TMeanAllInteger(const T* spx, indexType elementCount)
{
    double sum = 0.;
#if WITH_OPENMP
#pragma omp parallel for reduction(+ : sum) if (elementCount > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType i = 0; i < (ompIndexType)elementCount; ++i) {
        sum += (double)spx[i];
    }
    return sum / (double)elementCount;
}
//=============================================================================
template <class T>
double
TMeanAllReal(const T* spx, bool omitNaN, indexType elementCount)
{
    double sum = 0.;
    double mean = 0.;
    if (omitNaN) {
        double nbNotNaN = 0.;
#if WITH_OPENMP
#pragma omp parallel for reduction(+ : sum, nbNotNaN) if (elementCount > OMP_DEFAULT_THRESHOLD)
#endif
        for (ompIndexType i = 0; i < (ompIndexType)elementCount; ++i) {
            if (!std::isnan((double)spx[i])) {
                sum += (double)spx[i];
                nbNotNaN += 1;
            }
        }
        mean = sum / nbNotNaN;
    } else {
#if WITH_OPENMP
#pragma omp parallel for reduction(+ : sum) if (elementCount > OMP_DEFAULT_THRESHOLD)
#endif
        for (ompIndexType i = 0; i < (ompIndexType)elementCount; ++i) {
            sum += (double)spx[i];
        }
        mean = sum / (double)elementCount;
    }
    return mean;
}
//=============================================================================
template <class T>
std::complex<double>
TMeanAllComplex(const T* spx, bool omitNaN, indexType elementCount)
{
    std::complex<double> mean;
    double sumImag = 0.;
    double sumReal = 0.;
    if (omitNaN) {
        double nbNotNaN = 0.;
#if WITH_OPENMP
#pragma omp parallel for reduction(+ : sumImag, sumReal, nbNotNaN) if (elementCount > OMP_DEFAULT_THRESHOLD)
#endif
        for (ompIndexType i = 0; i < (ompIndexType)elementCount * 2; i = i + 2) {
            if (!std::isnan((double)spx[i]) && !std::isnan((double)spx[i + 1])) {
                sumReal += (double)spx[i];
                sumImag += (double)spx[i + 1];
                nbNotNaN += 1;
            }
        }
        if (nbNotNaN == 0) {
            std::complex<double> m(std::nan(""), 0);
            mean = m;
        } else {
            std::complex<double> m(sumReal / (double)nbNotNaN, sumImag / (double)nbNotNaN);
            mean = m;
        }
    } else {
#if WITH_OPENMP
#pragma omp parallel for reduction(+ : sumImag, sumReal) if (elementCount > OMP_DEFAULT_THRESHOLD)
#endif

        for (ompIndexType i = 0; i < (ompIndexType)elementCount * 2; i = i + 2) {
            sumReal += (double)spx[i];
            sumImag += (double)spx[i + 1];
        }
        std::complex<double> m(sumReal / (double)elementCount, sumImag / (double)elementCount);
        mean = m;
    }
    return mean;
}
//=============================================================================
static NelsonType
getOutPutClass(const ArrayOf& A, MEAN_OUT_TYPE outType)
{
    NelsonType outClass = A.getDataClass();
    switch (outType) {
    default:
    case MEAN_OUT_TYPE::DEFAULT: {
        switch (A.getDataClass()) {
        case NLS_SINGLE: {
            outClass = NLS_SINGLE;
        } break;
        case NLS_SCOMPLEX: {
            outClass = NLS_SCOMPLEX;
        } break;
        case NLS_DCOMPLEX: {
            outClass = NLS_DCOMPLEX;
        } break;
        default: {
            outClass = NLS_DOUBLE;
        } break;
        }
    } break;
    case MEAN_OUT_TYPE::DOUBLE: {
        switch (A.getDataClass()) {
        case NLS_SCOMPLEX: {
            outClass = NLS_DCOMPLEX;
        } break;
        case NLS_DCOMPLEX: {
            outClass = NLS_DCOMPLEX;
        } break;
        default: {
            outClass = NLS_DOUBLE;
        } break;
        }
    } break;
    case MEAN_OUT_TYPE::NATIVE: {
        switch (A.getDataClass()) {
        case NLS_LOGICAL: {
            outClass = NLS_DOUBLE;
        } break;
        case NLS_CHAR: {
            Error(_("Native accumulation on char array is not supported."));
        } break;
        default: {
        } break;
        }
    } break;
    }
    return outClass;
}
//=============================================================================
ArrayOf
MeanAll(const ArrayOf& A, bool omitNaN, MEAN_OUT_TYPE outType, bool& needToOverload)
{
    ArrayOfVector retval;
    ArrayOf res;
    needToOverload = false;
    NelsonType outClass = getOutPutClass(A, outType);
    if (A.isEmpty(false)) {
        Dimensions dims(1, 0);
        res = ArrayOf::emptyConstructor(dims, A.isSparse());
        return res;
    }
    if (A.isEmpty(true)) {
        res = ArrayOf::doubleConstructor(std::nan(""));
        res.promoteType(outClass);
        return res;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return res;
    }
    if (A.isScalar()) {
        res = A;
        switch (outClass) {
        case NLS_SCOMPLEX: {
            if (res.allReal()) {
                outClass = NLS_SINGLE;
            }
        } break;
        case NLS_DCOMPLEX: {
            if (res.allReal()) {
                outClass = NLS_DOUBLE;
            }
        } break;
        default: {
        } break;
        }
        res.promoteType(outClass);
        return res;
    }
    Dimensions dimsOut(1, 1);
    indexType elementCount = A.getElementCount();
    switch (A.getDataClass()) {
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRING_ARRAY:
    default: {
        needToOverload = true;
        return res;
    } break;
    case NLS_LOGICAL: {
        double m = TMeanAllReal<logical>((logical*)A.getDataPointer(), omitNaN, elementCount);
        res = ArrayOf::doubleConstructor(m);
    } break;
    case NLS_UINT8: {
        double m = TMeanAllInteger<uint8>((uint8*)A.getDataPointer(), elementCount);
        res = ArrayOf::doubleConstructor(m);
    } break;
    case NLS_INT8: {
        double m = TMeanAllInteger<int8>((int8*)A.getDataPointer(), elementCount);
        res = ArrayOf::doubleConstructor(m);
    } break;
    case NLS_UINT16: {
        double m = TMeanAllInteger<uint16>((uint16*)A.getDataPointer(), elementCount);
        res = ArrayOf::doubleConstructor(m);
    } break;
    case NLS_INT16: {
        double m = TMeanAllInteger<int16>((int16*)A.getDataPointer(), elementCount);
        res = ArrayOf::doubleConstructor(m);
    } break;
    case NLS_UINT32: {
        double m = TMeanAllInteger<uint32>((uint32*)A.getDataPointer(), elementCount);
        res = ArrayOf::doubleConstructor(m);
    } break;
    case NLS_INT32: {
        double m = TMeanAllInteger<int32>((int32*)A.getDataPointer(), elementCount);
        res = ArrayOf::doubleConstructor(m);
    } break;
    case NLS_UINT64: {
        double m = TMeanAllInteger<uint64>((uint64*)A.getDataPointer(), elementCount);
        res = ArrayOf::doubleConstructor(m);
    } break;
    case NLS_INT64: {
        double m = TMeanAllInteger<int64>((int64*)A.getDataPointer(), elementCount);
        res = ArrayOf::doubleConstructor(m);
    } break;
    case NLS_SINGLE: {
        double m = TMeanAllReal<single>((single*)A.getDataPointer(), omitNaN, elementCount);
        res = ArrayOf::singleConstructor((single)m);
    } break;
    case NLS_DOUBLE: {
        double m = TMeanAllReal<double>((double*)A.getDataPointer(), omitNaN, elementCount);
        res = ArrayOf::doubleConstructor(m);
    } break;
    case NLS_SCOMPLEX: {
        std::complex<double> m
            = TMeanAllComplex<single>((single*)A.getDataPointer(), omitNaN, elementCount);
        res = ArrayOf::complexConstructor((single)m.real(), (single)m.imag());
    } break;
    case NLS_DCOMPLEX: {
        std::complex<double> m
            = TMeanAllComplex<double>((double*)A.getDataPointer(), omitNaN, elementCount);
        res = ArrayOf::dcomplexConstructor(m.real(), m.imag());
    } break;
    case NLS_CHAR: {
        double m = TMeanAllReal<charType>((charType*)A.getDataPointer(), omitNaN, elementCount);
        res = ArrayOf::doubleConstructor(m);
    } break;
    }

    switch (outClass) {
    case NLS_SCOMPLEX: {
        if (res.allReal()) {
            outClass = NLS_SINGLE;
        }
    } break;
    case NLS_DCOMPLEX: {
        if (res.allReal()) {
            outClass = NLS_DOUBLE;
        }
    } break;
    default: {
    } break;
    }
    res.promoteType(outClass);

    return res;
}
//=============================================================================
ArrayOf
Mean(const ArrayOf& A, indexType dim, bool omitNaN, MEAN_OUT_TYPE outType, bool& needToOverload)
{
    ArrayOf res;
    NelsonType outClass = getOutPutClass(A, outType);
    if (A.isEmpty(false)) {
        Dimensions dims(1, 0);
        res = ArrayOf::emptyConstructor(dims, A.isSparse());
        return res;
    }
    if (A.isEmpty(true)) {
        res = ArrayOf::doubleConstructor(std::nan(""));
        res.promoteType(outClass);
        return res;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return res;
    }
    if (A.isScalar()) {
        res = A;
        switch (outClass) {
        case NLS_SCOMPLEX: {
            if (res.allReal()) {
                outClass = NLS_SINGLE;
            }
        } break;
        case NLS_DCOMPLEX: {
            if (res.allReal()) {
                outClass = NLS_DOUBLE;
            }
        } break;
        default: {
        } break;
        }
        res.promoteType(outClass);
        return res;
    }

    Dimensions dimsA = A.getDimensions();
    indexType workDim;
    if (dim == 0) {
        size_t l = 0;
        while (dimsA[l] == 1) {
            l++;
        }
        workDim = l;
    } else {
        workDim = dim - 1;
    }
    Dimensions dimsRes = dimsA;
    dimsRes.setDimensionLength(workDim, 1);
    dimsRes.simplify();
    size_t planecount;
    size_t planesize = 1;
    size_t linesize = dimsA[workDim];
    for (indexType l = 0; l < workDim; l++) {
        planesize *= dimsA[l];
    }
    planecount = 1;
    for (indexType l = workDim + 1; l < dimsA.getLength(); l++) {
        planecount *= dimsA[l];
    }
    NelsonType classA = A.getDataClass();
    switch (classA) {
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRING_ARRAY:
    default: {
        needToOverload = true;
        return res;
    } break;
    case NLS_LOGICAL: {
        double* ptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount()));
        if (dimsRes.isScalar()) {
            double m = TMeanAllInteger<logical>(
                static_cast<const logical*>(A.getDataPointer()), dimsA.getElementCount());
            ptr[0] = m;
        } else {
            TMeanInteger<logical>(static_cast<const logical*>(A.getDataPointer()), ptr, planecount,
                planesize, linesize);
        }
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptr);
    } break;
    case NLS_UINT8: {
        double* ptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount()));
        if (dimsRes.isScalar()) {
            double m = TMeanAllInteger<uint8>(
                static_cast<const uint8*>(A.getDataPointer()), dimsA.getElementCount());
            ptr[0] = m;
        } else {
            TMeanInteger<uint8>(static_cast<const uint8*>(A.getDataPointer()), ptr, planecount,
                planesize, linesize);
        }
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptr);
    } break;
    case NLS_INT8: {
        double* ptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount()));
        if (dimsRes.isScalar()) {
            double m = TMeanAllInteger<int8>(
                static_cast<const int8*>(A.getDataPointer()), dimsA.getElementCount());
            ptr[0] = m;
        } else {
            TMeanInteger<int8>(
                static_cast<const int8*>(A.getDataPointer()), ptr, planecount, planesize, linesize);
        }
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptr);
    } break;
    case NLS_UINT16: {
        double* ptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount()));
        if (dimsRes.isScalar()) {
            double m = TMeanAllInteger<uint16>(
                static_cast<const uint16*>(A.getDataPointer()), dimsA.getElementCount());
            ptr[0] = m;
        } else {
            TMeanInteger<uint16>(static_cast<const uint16*>(A.getDataPointer()), ptr, planecount,
                planesize, linesize);
        }
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptr);
    } break;
    case NLS_INT16: {
        double* ptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount()));
        if (dimsRes.isScalar()) {
            double m = TMeanAllInteger<int16>(
                static_cast<const int16*>(A.getDataPointer()), dimsA.getElementCount());
            ptr[0] = m;
        } else {
            TMeanInteger<int16>(static_cast<const int16*>(A.getDataPointer()), ptr, planecount,
                planesize, linesize);
        }
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptr);
    } break;
    case NLS_UINT32: {
        double* ptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount()));
        if (dimsRes.isScalar()) {
            double m = TMeanAllInteger<uint32>(
                static_cast<const uint32*>(A.getDataPointer()), dimsA.getElementCount());
            ptr[0] = m;
        } else {
            TMeanInteger<uint32>(static_cast<const uint32*>(A.getDataPointer()), ptr, planecount,
                planesize, linesize);
        }
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptr);
    } break;
    case NLS_INT32: {
        double* ptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount()));
        if (dimsRes.isScalar()) {
            double m = TMeanAllInteger<int32>(
                static_cast<const int32*>(A.getDataPointer()), dimsA.getElementCount());
            ptr[0] = m;
        } else {
            TMeanInteger<int32>(static_cast<const int32*>(A.getDataPointer()), ptr, planecount,
                planesize, linesize);
        }
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptr);
    } break;
    case NLS_UINT64: {
        double* ptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount()));
        if (dimsRes.isScalar()) {
            double m = TMeanAllInteger<uint64>(
                static_cast<const uint64*>(A.getDataPointer()), dimsA.getElementCount());
            ptr[0] = m;
        } else {
            TMeanInteger<uint64>(static_cast<const uint64*>(A.getDataPointer()), ptr, planecount,
                planesize, linesize);
        }
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptr);
    } break;
    case NLS_INT64: {
        double* ptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount()));
        if (dimsRes.isScalar()) {
            double m = TMeanAllInteger<int64>(
                static_cast<const int64*>(A.getDataPointer()), dimsA.getElementCount());
            ptr[0] = m;
        } else {
            TMeanInteger<int64>(static_cast<const int64*>(A.getDataPointer()), ptr, planecount,
                planesize, linesize);
        }
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptr);
    } break;
    case NLS_SINGLE: {
        double* ptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount()));
        if (dimsRes.isScalar()) {
            double m = TMeanAllReal<single>(
                static_cast<const single*>(A.getDataPointer()), omitNaN, dimsA.getElementCount());
            ptr[0] = m;
        } else {
            TMeanReal<single>(static_cast<const single*>(A.getDataPointer()), ptr, planecount,
                planesize, linesize, omitNaN);
        }
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptr);
    } break;
    case NLS_DOUBLE: {
        double* ptr = static_cast<double*>(
            ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount(), stringVector(), false));
        if (dimsRes.isScalar()) {
            double m = TMeanAllReal<double>(
                static_cast<const double*>(A.getDataPointer()), omitNaN, dimsA.getElementCount());
            ptr[0] = m;
        } else {
            TMeanReal<double>(static_cast<const double*>(A.getDataPointer()), ptr, planecount,
                planesize, linesize, omitNaN);
        }
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptr);
    } break;
    case NLS_SCOMPLEX: {
        double* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, dimsRes.getElementCount(), stringVector(), false));
        if (dimsRes.isScalar()) {
            std::complex<double> m = TMeanAllComplex<single>(
                static_cast<const single*>(A.getDataPointer()), omitNaN, dimsA.getElementCount());
            ptr[0] = m.real();
            ptr[1] = m.imag();
        } else {
            TMeanComplex<single>(static_cast<const single*>(A.getDataPointer()), ptr, planecount,
                planesize, linesize, omitNaN);
        }
        res = ArrayOf(NLS_DCOMPLEX, dimsRes, ptr);
    } break;
    case NLS_DCOMPLEX: {
        double* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(
            NLS_DCOMPLEX, dimsRes.getElementCount(), stringVector(), false));
        if (dimsRes.isScalar()) {
            std::complex<double> m = TMeanAllComplex<double>(
                static_cast<const double*>(A.getDataPointer()), omitNaN, dimsA.getElementCount());
            ptr[0] = m.real();
            ptr[1] = m.imag();
        } else {
            TMeanComplex<double>(static_cast<const double*>(A.getDataPointer()), ptr, planecount,
                planesize, linesize, omitNaN);
        }
        res = ArrayOf(NLS_DCOMPLEX, dimsRes, ptr);
    } break;
    case NLS_CHAR: {
        double* ptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.getElementCount()));
        if (dimsRes.isScalar()) {
            double m = TMeanAllInteger<charType>(
                static_cast<const charType*>(A.getDataPointer()), dimsA.getElementCount());
            ptr[0] = m;
        } else {
            TMeanInteger<charType>(static_cast<const charType*>(A.getDataPointer()), ptr,
                planecount, planesize, linesize);
        }
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptr);

    } break;
    }

    switch (outClass) {
    case NLS_SCOMPLEX: {
        if (res.allReal()) {
            outClass = NLS_SINGLE;
        }
    } break;
    case NLS_DCOMPLEX: {
        if (res.allReal()) {
            outClass = NLS_DOUBLE;
        }
    } break;
    default: {
    } break;
    }
    res.promoteType(outClass);
    return res;
}
//=============================================================================
}
//=============================================================================
