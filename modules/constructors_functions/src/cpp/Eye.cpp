//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "omp_for_loop.hpp"
#include <algorithm>
#include "Eye.hpp"
#include "SparseType.hpp"
#include "NelsonTypeVisitor.hpp"
#include "NelsonComplexStorage.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Fill a freshly zero-allocated buffer with 1 on the main diagonal (real
// scalar classes); returns the payload pointer.
template <class T>
static void*
eyeRealDiagonal(NelsonType classDest, indexType n, indexType m)
{
    T* mat = static_cast<T*>(ArrayOf::allocateArrayOf(classDest, m * n, stringVector(), true));
    ompIndexType elementCount = (ompIndexType)std::min(m, n);
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (ompIndexType i = 0; i < elementCount; i++) {
        mat[i * n + i] = (T)1;
    }
    return (void*)mat;
}
//=============================================================================
// Dense complex identity via Eigen setIdentity; T is the real element type.
template <class T>
static void*
eyeComplexIdentity(NelsonType classDest, indexType n, indexType m)
{
    T* mat = static_cast<T*>(ArrayOf::allocateArrayOf(classDest, m * n, stringVector(), true));
    auto* pzMat = TypeTraits::asComplex<T>(mat);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matEye(pzMat, n, m);
    matEye.setIdentity();
    return (void*)pzMat;
}
//=============================================================================
ArrayOf
Eye(indexType n, indexType m, NelsonType classDest, bool bIsSparse)
{
    Dimensions dimMat(n, m);
    void* vmat = nullptr;
    if (m * n != 0) {
        switch (classDest) {
        case NLS_DCOMPLEX: {
            vmat = bIsSparse ? Eigen_EyeSparseMatrixConstructor(NLS_DCOMPLEX, n, m)
                             : eyeComplexIdentity<double>(classDest, n, m);
        } break;
        case NLS_SCOMPLEX: {
            if (bIsSparse) {
                Error(L"Nelson:constructors_functions:sparseSupported", L"Sparse not supported.");
            }
            vmat = eyeComplexIdentity<single>(classDest, n, m);
        } break;
        case NLS_DOUBLE: {
            vmat = bIsSparse ? Eigen_EyeSparseMatrixConstructor(NLS_DOUBLE, n, m)
                             : eyeRealDiagonal<double>(classDest, n, m);
        } break;
        case NLS_SINGLE: {
            if (bIsSparse) {
                Error(L"Nelson:constructors_functions:sparseSupported", L"Sparse not supported.");
            }
            vmat = eyeRealDiagonal<single>(classDest, n, m);
        } break;
        case NLS_LOGICAL: {
            vmat = bIsSparse ? Eigen_EyeSparseMatrixConstructor(NLS_LOGICAL, n, m)
                             : eyeRealDiagonal<logical>(classDest, n, m);
        } break;
        case NLS_INT8:
        case NLS_INT16:
        case NLS_INT32:
        case NLS_INT64:
        case NLS_UINT8:
        case NLS_UINT16:
        case NLS_UINT32:
        case NLS_UINT64: {
            if (bIsSparse) {
                Error(L"Nelson:constructors_functions:sparseSupported", L"Sparse not supported.");
            }
            vmat = TypeTraits::visitReal(classDest,
                [&](auto tag) { return eyeRealDiagonal<decltype(tag)>(classDest, n, m); });
        } break;
        default: {
            Error(L"Nelson:constructors_functions:inputFollowingLikeNumericArray",
                L"Input following 'like' is not a numeric array.");
        } break;
        }
    } else {
        switch (classDest) {
        case NLS_DOUBLE:
        case NLS_LOGICAL: {
            vmat = nullptr;
        } break;
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_SINGLE:
        case NLS_INT8:
        case NLS_INT16:
        case NLS_INT32:
        case NLS_INT64:
        case NLS_UINT8:
        case NLS_UINT16:
        case NLS_UINT32:
        case NLS_UINT64: {
            if (bIsSparse) {
                Error(L"Nelson:constructors_functions:sparseSupported", L"Sparse not supported.");
            }
            vmat = nullptr;
        } break;
        default: {
            Error(L"Nelson:constructors_functions:inputFollowingLikeNumericArray",
                L"Input following 'like' is not a numeric array.");
        } break;
        }
    }
    return ArrayOf(classDest, dimMat, vmat, bIsSparse);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
