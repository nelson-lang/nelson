//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <cmath>
#include <type_traits>
#include <unordered_set>
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "SparseType.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NewWithException.hpp"
//=============================================================================
// Sparse matrices to and from (I, J, V) triplets.
//=============================================================================
template <class T>
void*
Eigen_SparseToIJV(const void* cp, indexType*& I, indexType*& J, int& nnz)
{
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<T, 0, signedIndexType>*)cp;
    T* pV = nullptr;
    if (spMat) {
        indexType q = 0;
        pV = new_with_exception<T>(spMat->nonZeros(), false);
        for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
            for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spMat, k);
                it; ++it) {
                I[q] = it.row() + 1;
                J[q] = it.col() + 1;
                pV[q] = it.value();
                q++;
            }
        }
        nnz = (int)spMat->nonZeros();
    } else {
        nnz = 0;
    }
    return (void*)pV;
}
//=============================================================================
void*
Eigen_SparseToIJV(NelsonType dclass, indexType rows, indexType cols, const void* cp, indexType*& I,
    indexType*& J, int& nnz)
{
    switch (dclass) {
    case NLS_LOGICAL: {
        return Eigen_SparseToIJV<logical>(cp, I, J, nnz);
    } break;
    case NLS_DOUBLE: {
        return Eigen_SparseToIJV<double>(cp, I, J, nnz);
    } break;
    case NLS_SINGLE: {
        return Eigen_SparseToIJV<single>(cp, I, J, nnz);
    } break;
    case NLS_DCOMPLEX: {
        Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMat
            = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)cp;
        double* pV = nullptr;
        if (spMat) {
            indexType q = 0;
            indexType k = 0;
            indexType l = 0;
            pV = new_with_exception<double>(spMat->nonZeros() * 2, false);
            for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
                for (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>::InnerIterator it(
                         *spMat, k);
                    it; ++it) {
                    I[q] = it.row() + 1;
                    J[q] = it.col() + 1;
                    pV[l] = it.value().real();
                    pV[l + 1] = it.value().imag();
                    l = l + 2;
                    q++;
                }
            }
            nnz = (int)spMat->nonZeros();
        } else {
            nnz = 0;
        }
        return (void*)pV;
    } break;
    case NLS_SCOMPLEX: {
        Eigen::SparseMatrix<singlecomplex, 0, signedIndexType>* spMat
            = (Eigen::SparseMatrix<singlecomplex, 0, signedIndexType>*)cp;
        single* pV = nullptr;
        if (spMat) {
            indexType q = 0;
            indexType l = 0;
            pV = new_with_exception<single>(spMat->nonZeros() * 2, false);
            for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
                for (Eigen::SparseMatrix<singlecomplex, 0, signedIndexType>::InnerIterator it(
                         *spMat, k);
                    it; ++it) {
                    I[q] = it.row() + 1;
                    J[q] = it.col() + 1;
                    pV[l] = it.value().real();
                    pV[l + 1] = it.value().imag();
                    l = l + 2;
                    q++;
                }
            }
            nnz = (int)spMat->nonZeros();
        } else {
            nnz = 0;
        }
        return (void*)pV;
    } break;
    default: {
        Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.", L"SparseToIJV");
    }
    }
    return nullptr;
}
//=============================================================================
template <class T>
void*
Eigen_makeSparseFromIJVInternal(indexType rows, indexType cols, indexType nnz, indexType* I,
    int istride, indexType* J, int jstride, const void* cp, bool bScalarV)
{
    using Triplet = Eigen::Triplet<T, signedIndexType>;
    std::vector<Triplet> tripletList;
    tripletList.reserve(nnz);
    T* pV = (T*)cp;
    // Push every non-zero entry; Eigen's setFromTriplets sums duplicate (i, j)
    // pairs internally in O(nnz log nnz), so no manual coalescing is needed.
    // A scalar subscript has stride 0, so it is broadcast to every entry
    // instead of being indexed out of bounds.
    for (indexType k = 0; k < nnz; k++) {
        T value = bScalarV ? pV[0] : pV[k];
        if (value != T(0)) {
            tripletList.emplace_back(I[(size_t)k * istride] - 1, J[(size_t)k * jstride] - 1, value);
        }
    }
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMat = nullptr;
    try {
        spMat = new Eigen::SparseMatrix<T, 0, signedIndexType>(rows, cols);
    } catch (const std::bad_alloc&) {
        spMat = nullptr;
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    spMat->setFromTriplets(tripletList.begin(), tripletList.end());
    spMat->makeCompressed();
    return (void*)spMat;
}
//=============================================================================
void*
Eigen_makeSparseFromIJVLogical(indexType rows, indexType cols, indexType nnz, indexType* I,
    int istride, indexType* J, int jstride, const void* cp, bool bScalarV)
{
    using Triplet = Eigen::Triplet<logical, signedIndexType>;
    std::vector<Triplet> tripletList;
    tripletList.reserve(nnz);
    auto* pV = (logical*)cp;
    // Repeated indices are rejected for logical matrices; detect them in O(nnz)
    // with a hashed (row, col) key instead of a quadratic linear scan.
    std::unordered_set<uint64_t> seen;
    seen.reserve(nnz * 2);
    uint64_t ncols = (uint64_t)cols;
    for (indexType k = 0; k < nnz; k++) {
        logical value = bScalarV ? pV[0] : pV[k];
        if (value) {
            indexType r = I[(size_t)k * istride] - 1;
            indexType c = J[(size_t)k * jstride] - 1;
            uint64_t key = (uint64_t)r * ncols + (uint64_t)c;
            if (!seen.insert(key).second) {
                Error(L"Nelson:sparse:repeatedIndicesNotSupported",
                    L"Repeated indices are not supported for sparse logical matrices.");
            }
            tripletList.emplace_back(r, c, value);
        }
    }
    Eigen::SparseMatrix<logical, 0, signedIndexType>* spMat = nullptr;
    try {
        spMat = new Eigen::SparseMatrix<logical, 0, signedIndexType>(rows, cols);
    } catch (const std::bad_alloc&) {
        spMat = nullptr;
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    spMat->setFromTriplets(tripletList.begin(), tripletList.end());
    spMat->makeCompressed();
    return (void*)spMat;
}
//=============================================================================
void*
Eigen_makeSparseFromIJVComplex(indexType rows, indexType cols, indexType nnz, indexType* I,
    int istride, indexType* J, int jstride, const void* cp, bool bScalarV)
{
    using T = Eigen::Triplet<std::complex<double>, signedIndexType>;
    std::vector<T> tripletList;
    tripletList.reserve(nnz);
    auto* pV = (double*)cp;
    indexType q = 0;
    for (indexType k = 0; k < nnz; k++) {
        indexType r = I[(size_t)k * istride] - 1;
        indexType c = J[(size_t)k * jstride] - 1;
        /* Currently , for compatibility, complex values are not cumulative */
        if (bScalarV) {
            if (pV[0] || pV[1]) {
                doublecomplex v(pV[0], pV[1]);
                tripletList.emplace_back(r, c, v);
            }
            q = q + 2;
        } else {
            if (pV[q] || pV[q + 1]) {
                doublecomplex v(pV[q], pV[q + 1]);
                tripletList.emplace_back(r, c, v);
            }
        }
        q = q + 2;
    }
    Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMat = nullptr;
    try {
        spMat = new Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>(rows, cols);
    } catch (const std::bad_alloc&) {
        spMat = nullptr;
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    spMat->setFromTriplets(tripletList.begin(), tripletList.end());
    spMat->makeCompressed();
    return (void*)spMat;
}
//=============================================================================
void*
Eigen_makeSparseFromIJV(NelsonType dclass, indexType rows, indexType cols, indexType nnz,
    indexType* I, int istride, indexType* J, int jstride, const void* cp, int cpstride,
    bool bScalarV)
{
    switch (dclass) {
    case NLS_LOGICAL: {
        return Eigen_makeSparseFromIJVLogical(
            rows, cols, nnz, I, istride, J, jstride, cp, bScalarV);
    } break;
    case NLS_DOUBLE: {
        return Eigen_makeSparseFromIJVInternal<double>(
            rows, cols, nnz, I, istride, J, jstride, cp, bScalarV);
    } break;
    case NLS_SINGLE: {
        return Eigen_makeSparseFromIJVInternal<single>(
            rows, cols, nnz, I, istride, J, jstride, cp, bScalarV);
    } break;
    case NLS_DCOMPLEX: {
        return Eigen_makeSparseFromIJVComplex(
            rows, cols, nnz, I, istride, J, jstride, cp, bScalarV);
    } break;
    case NLS_SCOMPLEX: {
        return Eigen_makeSparseFromIJVInternal<singlecomplex>(
            rows, cols, nnz, I, istride, J, jstride, cp, bScalarV);
    } break;
    default: {
        Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.", L"SparseToIJV");
    }
    }
    return nullptr;
}
//=============================================================================
