//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4101)
#endif
//=============================================================================
#include <complex>
#include "nlsBuildConfig.h"
#include "Convolution2D.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Decomplexify.hpp"
#include "complex_multiply.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isSupportedShape(const std::wstring& shape)
{
    if (shape == L"full") {
        return true;
    }
    if (shape == L"same") {
        return true;
    }
    if (shape == L"valid") {
        return true;
    }
    return false;
}
//=============================================================================
static bool
isSupportedInputTypes(const ArrayOf& A)
{
    switch (A.dataClass()) {
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX: {
        return true;
    } break;
    default: {
        return false;
    } break;
    }
    return false;
}
//=============================================================================
static void
computeCommonType(
    const ArrayOf& A, const ArrayOf& B, NelsonType& intermediateClass, NelsonType& outClass)
{
    intermediateClass = NLS_DOUBLE;
    outClass = NLS_DOUBLE;
    NelsonType Ain(A.dataClass());
    NelsonType Bin(B.dataClass());
    if (Ain == NLS_SINGLE && Bin == NLS_SINGLE) {
        intermediateClass = NLS_SINGLE;
        outClass = NLS_SINGLE;
    } else if (Ain == NLS_SINGLE || Bin == NLS_SINGLE) {
        outClass = NLS_SINGLE;
    }
    if (A.isComplex() || B.isComplex()) {
        if (outClass == NLS_SINGLE) {
            outClass = NLS_SCOMPLEX;
        } else {
            outClass = NLS_DCOMPLEX;
        }
        if (intermediateClass == NLS_SINGLE) {
            intermediateClass = NLS_SCOMPLEX;
        } else {
            intermediateClass = NLS_DCOMPLEX;
        }
    }
}
//=============================================================================
template <class T>
static void
Conv2Real(T* C, const T* A, const T* B, indexType Am, indexType An, indexType Bm, indexType Bn,
    indexType Cm, indexType Cn, indexType Cm_offset, indexType Cn_offset)
{
    ompIndexType n = 0;
    ompIndexType m = 0;
    ompIndexType i = 0;
    ompIndexType j = 0;
#if WITH_OPENMP
#pragma omp parallel for private(n, m, i, j)
#endif
    for (n = 0; n < (ompIndexType)Cn; n++) {
        for (m = 0; m < (ompIndexType)Cm; m++) {
            T accum = 0;
            int64 iMin = std::max(int64(0), int64(m + Cm_offset - Bm + 1));
            int64 iMax = std::min(int64(Am - 1), int64(m + Cm_offset));
            int64 jMin = std::max(int64(0), int64(n + Cn_offset - Bn + 1));
            int64 jMax = std::min(int64(An - 1), int64(n + Cn_offset));
            for (j = jMin; j <= jMax; j++) {
                for (i = iMin; i <= iMax; i++) {
                    accum += (A[i + j * Am] == 0)
                        ? 0
                        : A[i + j * Am] * B[(m + Cm_offset - i) + (n + Cn_offset - j) * Bm];
                }
            }
            C[m + n * Cm] = accum;
        }
    }
}
//=============================================================================
template <class T>
static void
Conv2Complex(std::complex<T>* C, const std::complex<T>* A, const std::complex<T>* B, indexType Am,
    indexType An, indexType Bm, indexType Bn, indexType Cm, indexType Cn, indexType Cm_offset,
    indexType Cn_offset)
{

    ompIndexType n = 0;
    ompIndexType m = 0;
    ompIndexType i = 0;
    ompIndexType j = 0;
#if WITH_OPENMP
#pragma omp parallel for private(n, m, i, j)
#endif
    for (n = 0; n < (ompIndexType)Cn; n++) {
        for (m = 0; m < (ompIndexType)Cm; m++) {
            std::complex<double> accum = 0;
            int64 iMin = std::max(int64(0), int64(m + Cm_offset - Bm + 1));
            int64 iMax = std::min(int64(Am - 1), int64(m + Cm_offset));
            int64 jMin = std::max(int64(0), int64(n + Cn_offset - Bn + 1));
            int64 jMax = std::min(int64(An - 1), int64(n + Cn_offset));
            for (j = jMin; j <= jMax; j++) {
                for (i = iMin; i <= iMax; i++) {
                    accum += complex_multiply<T>(
                        A[i + j * Am], B[(m + Cm_offset - i) + (n + Cn_offset - j) * Bm]);
                }
            }
            C[m + n * Cm] = accum;
        }
    }
}
//=============================================================================
static ArrayOf
Conv2dDispatch(const ArrayOf& X, const ArrayOf& Y, indexType Cm, indexType Cn, indexType Cm_offset,
    indexType Cn_offset)
{
    ArrayOf res;
    Dimensions dimsRes(Cm, Cn);

    switch (X.dataClass()) {
    case NLS_DOUBLE: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.elementCount());
        res = ArrayOf(NLS_DOUBLE, dimsRes, ptr);
        Conv2Real<double>(ptr, (const double*)X.dataPointer(), (const double*)Y.dataPointer(),
            X.rows(), X.columns(), Y.rows(), Y.columns(), Cm, Cn, Cm_offset, Cn_offset);
    } break;
    case NLS_SINGLE: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, dimsRes.elementCount());
        res = ArrayOf(NLS_SINGLE, dimsRes, ptr);
        Conv2Real<single>(ptr, (const single*)X.dataPointer(), (const single*)Y.dataPointer(),
            X.rows(), X.columns(), Y.rows(), Y.columns(), Cm, Cn, Cm_offset, Cn_offset);
    } break;
    case NLS_DCOMPLEX: {
        void* ptr = ArrayOf::allocateArrayOf(NLS_DCOMPLEX, dimsRes.elementCount());
        res = ArrayOf(NLS_DCOMPLEX, dimsRes, ptr);

        auto* ptrz = reinterpret_cast<std::complex<double>*>((double*)ptr);
        auto* ptrX = reinterpret_cast<std::complex<double>*>((double*)X.dataPointer());
        auto* ptrY = reinterpret_cast<std::complex<double>*>((double*)Y.dataPointer());
        Conv2Complex<double>(ptrz, ptrX, ptrY, X.rows(), X.columns(), Y.rows(), Y.columns(), Cm, Cn,
            Cm_offset, Cn_offset);
    } break;
    case NLS_SCOMPLEX: {
        void* ptr = ArrayOf::allocateArrayOf(NLS_SCOMPLEX, dimsRes.elementCount());
        res = ArrayOf(NLS_SCOMPLEX, dimsRes, ptr);

        auto* ptrz = reinterpret_cast<std::complex<single>*>((single*)ptr);
        auto* ptrX = reinterpret_cast<std::complex<single>*>((single*)X.dataPointer());
        auto* ptrY = reinterpret_cast<std::complex<single>*>((single*)Y.dataPointer());
        Conv2Complex<single>(ptrz, ptrX, ptrY, X.rows(), X.columns(), Y.rows(), Y.columns(), Cm, Cn,
            Cm_offset, Cn_offset);
    } break;
    default: {
    } break;
    }
    return res;
}
//=============================================================================
static ArrayOf
Conv2dXYFull(const ArrayOf& A, const ArrayOf& B)
{
    indexType Cm = A.rows() + B.rows() - 1;
    indexType Cn = A.columns() + B.columns() - 1;
    indexType Cm_offset = 0;
    indexType Cn_offset = 0;
    return Conv2dDispatch(A, B, Cm, Cn, Cm_offset, Cn_offset);
}
//=============================================================================
static ArrayOf
Conv2dXYValid(const ArrayOf& A, const ArrayOf& B)
{
    indexType Cm = indexType(A.rows() - B.rows() + 1);
    indexType Cn = indexType(A.columns() - B.columns() + 1);
    if ((Cm == 0) || (Cn == 0)) {
        Dimensions dims(Cm, Cn);
        ArrayOf res = ArrayOf::empty(dims);
        res.promoteType(A.dataClass());
        return res;
    }
    indexType Cm_offset = indexType(B.rows() - 1);
    indexType Cn_offset = indexType(B.columns() - 1);
    return Conv2dDispatch(A, B, Cm, Cn, Cm_offset, Cn_offset);
}
//=============================================================================
static ArrayOf
Conv2dXYSame(const ArrayOf& A, const ArrayOf& B)
{
    indexType Cm = A.rows();
    indexType Cn = A.columns();
    indexType Cm_offset = (indexType)round(((double)(B.rows() - 1) / 2));
    indexType Cn_offset = (indexType)round(((double)(B.columns() - 1) / 2));
    return Conv2dDispatch(A, B, Cm, Cn, Cm_offset, Cn_offset);
}
//=============================================================================
ArrayOf
Convolution2D(const ArrayOf& A, const ArrayOf& B, const std::wstring& shape, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = true;
    if (!isSupportedShape(shape)) {
        Error(L"Nelson:conv2:unknownShapeParameter",
            L"shape parameter must be 'full', 'same', or 'valid'.");
    }
    if (!isSupportedInputTypes(A)) {
        Error(L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical.",
            L"First argument");
    }
    if (!isSupportedInputTypes(B)) {
        Error(L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical.",
            L"First argument");
    }
    if (!A.is2D() || !B.is2D()) {
        Error(L"Nelson:conv2:ndArrayInput", L"N-D arrays are not supported.");
    }
    if (A.isSparse() || B.isSparse()) {
        Error(L"Nelson:conv2:SparseInput", L"Sparse matrices are not supported.");
    }

    NelsonType intermediateClass;
    NelsonType outputClass;
    computeCommonType(A, B, intermediateClass, outputClass);

    if (A.isEmpty() || B.isEmpty()) {
        needToOverload = false;
        Dimensions dimsA = A.dimensions();
        Dimensions dimsB = B.dimensions();
        indexType rows = std::max(A.rows(), B.rows());
        indexType cols = std::max(A.columns(), B.columns());
        Dimensions dimsC(rows, cols);
        void* ptr
            = ArrayOf::allocateArrayOf(outputClass, dimsC.elementCount(), stringVector(), true);
        res = ArrayOf(outputClass, dimsC, ptr);
        return decomplexify(res);
    }
    ArrayOf Aintermediate(A);
    ArrayOf Bintermediate(B);
    Aintermediate.promoteType(intermediateClass);
    Bintermediate.promoteType(intermediateClass);
    needToOverload = false;
    if (shape == L"full") {
        res = Conv2dXYFull(Aintermediate, Bintermediate);
    }
    if (shape == L"same") {
        res = Conv2dXYSame(Aintermediate, Bintermediate);
    }
    if (shape == L"valid") {
        res = Conv2dXYValid(Aintermediate, Bintermediate);
    }
    res.promoteType(outputClass);
    return decomplexify(res);
}
//=============================================================================
ArrayOf
Convolution2D(const ArrayOf& u, const ArrayOf& v, const ArrayOf& A, const std::wstring& shape,
    bool& needToOverload)
{
    ArrayOf res;
    needToOverload = true;
    if (!isSupportedShape(shape)) {
        Error(L"Nelson:conv2:unknownShapeParameter",
            L"shape parameter must be 'full', 'same', or 'valid'.");
    }
    if (!isSupportedInputTypes(u)) {
        Error(L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical.",
            L"First argument");
    }
    if (!isSupportedInputTypes(v)) {
        Error(L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical.",
            L"Second argument");
    }
    if (!isSupportedInputTypes(A)) {
        Error(L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical.",
            L"Third argument");
    }
    if (!u.is2D() || !v.is2D() || !A.is2D()) {
        Error(L"Nelson:conv2:ndArrayInput", L"N-D arrays are not supported.");
    }
    if (u.isSparse() || v.isSparse() || A.isSparse()) {
        Error(L"Nelson:conv2:SparseInput", L"Sparse matrices are not supported.");
    }

    Dimensions newDimsU(u.elementCount(), 1);
    ArrayOf U(u);
    U.reshape(newDimsU);

    Dimensions newDimsV(1, v.elementCount());
    ArrayOf V(v);
    V.reshape(newDimsV);

    res = Convolution2D(A, U, shape, needToOverload);
    if (!needToOverload) {
        res = Convolution2D(res, V, shape, needToOverload);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
