//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "Eye.hpp"
#include "SparseDynamicFunctions.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Eye(indexType n, indexType m, NelsonType classDest, bool bIsSparse)
{
    Dimensions dimMat(n, m);
    void* vmat = nullptr;
    if (m * n != 0) {
        switch (classDest) {
        case NLS_DCOMPLEX: {
            if (bIsSparse) {
                vmat = EyeSparseMatrixConstructorDynamicFunction(NLS_DCOMPLEX, n, m);
            } else {
                double* mat = static_cast<double*>(
                    ArrayOf::allocateArrayOf(NLS_DCOMPLEX, m * n, stringVector(), true));
                auto* pzMat = reinterpret_cast<doublecomplex*>(mat);
                Eigen::Map<Eigen::Matrix<doublecomplex, Eigen::Dynamic, Eigen::Dynamic>> matEye(
                    pzMat, n, m);
                matEye.setIdentity();
                vmat = (void*)pzMat;
            }
        } break;
        case NLS_DOUBLE: {
            if (bIsSparse) {
                vmat = EyeSparseMatrixConstructorDynamicFunction(NLS_DOUBLE, n, m);
            } else {
                double* mat = static_cast<double*>(
                    ArrayOf::allocateArrayOf(NLS_DOUBLE, m * n, stringVector(), true));
#if WITH_OPENMP
#pragma omp parallel for
#endif
                for (ompIndexType i = 0; i < (ompIndexType)std::min(m, n); i++) {
                    mat[i * n + i] = 1;
                }
                vmat = (void*)mat;
            }
        } break;
        case NLS_SCOMPLEX: {
            if (bIsSparse) {
                Error(_W("sparse not supported."));
            }
            single* mat = static_cast<single*>(
                ArrayOf::allocateArrayOf(NLS_SCOMPLEX, m * n, stringVector(), true));
            auto* pzMat = reinterpret_cast<singlecomplex*>(mat);
            Eigen::Map<Eigen::Matrix<singlecomplex, Eigen::Dynamic, Eigen::Dynamic>> matEye(
                pzMat, n, m);
            matEye.setIdentity();
            vmat = (void*)mat;
        } break;
        case NLS_SINGLE: {
            if (bIsSparse) {
                Error(_W("sparse not supported."));
            }
            single* mat = static_cast<single*>(
                ArrayOf::allocateArrayOf(NLS_SINGLE, m * n, stringVector(), true));
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType i = 0; i < (ompIndexType)std::min(m, n); i++) {
                mat[i * n + i] = (single)1;
            }
            vmat = (void*)mat;
        } break;
        case NLS_LOGICAL: {
            if (bIsSparse) {
                vmat = EyeSparseMatrixConstructorDynamicFunction(NLS_LOGICAL, n, m);
            } else {
                logical* mat = static_cast<logical*>(
                    ArrayOf::allocateArrayOf(NLS_LOGICAL, m * n, stringVector(), true));
#if WITH_OPENMP
#pragma omp parallel for
#endif
                for (ompIndexType i = 0; i < (ompIndexType)std::min(m, n); i++) {
                    mat[i * n + i] = (logical)1;
                }
                vmat = (void*)mat;
            }
        } break;
        case NLS_INT8: {
            if (bIsSparse) {
                Error(_W("sparse not supported."));
            }
            int8* mat = static_cast<int8*>(
                ArrayOf::allocateArrayOf(NLS_INT8, m * n, stringVector(), true));
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType i = 0; i < (ompIndexType)std::min(m, n); i++) {
                mat[i * n + i] = (int8)1;
            }
            vmat = (void*)mat;
        } break;
        case NLS_INT16: {
            if (bIsSparse) {
                Error(_W("sparse not supported."));
            }
            int16* mat = static_cast<int16*>(
                ArrayOf::allocateArrayOf(NLS_INT16, m * n, stringVector(), true));
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType i = 0; i < (ompIndexType)std::min(m, n); i++) {
                mat[i * n + i] = (int16)1;
            }
            vmat = (void*)mat;
        } break;
        case NLS_INT32: {
            if (bIsSparse) {
                Error(_W("sparse not supported."));
            }
            int32* mat = static_cast<int32*>(
                ArrayOf::allocateArrayOf(NLS_INT32, m * n, stringVector(), true));
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType i = 0; i < (ompIndexType)std::min(m, n); i++) {
                mat[i * n + i] = (int32)1;
            }
            vmat = (void*)mat;
        } break;
        case NLS_INT64: {
            if (bIsSparse) {
                Error(_W("sparse not supported."));
            }
            int64* mat = static_cast<int64*>(
                ArrayOf::allocateArrayOf(NLS_INT64, m * n, stringVector(), true));
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType i = 0; i < (ompIndexType)std::min(m, n); i++) {
                mat[i * n + i] = (int64)1;
            }
            vmat = (void*)mat;
        } break;
        case NLS_UINT8: {
            if (bIsSparse) {
                Error(_W("sparse not supported."));
            }
            uint8* mat = static_cast<uint8*>(
                ArrayOf::allocateArrayOf(NLS_UINT8, m * n, stringVector(), true));
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType i = 0; i < (ompIndexType)std::min(m, n); i++) {
                mat[i * n + i] = (uint8)1;
            }
            vmat = (void*)mat;
        } break;
        case NLS_UINT16: {
            if (bIsSparse) {
                Error(_W("sparse not supported."));
            }
            uint16* mat = static_cast<uint16*>(
                ArrayOf::allocateArrayOf(NLS_UINT16, m * n, stringVector(), true));
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType i = 0; i < (ompIndexType)std::min(m, n); i++) {
                mat[i * n + i] = (uint16)1;
            }
            vmat = (void*)mat;
        } break;
        case NLS_UINT32: {
            if (bIsSparse) {
                Error(_W("sparse not supported."));
            }
            uint32* mat = static_cast<uint32*>(
                ArrayOf::allocateArrayOf(NLS_UINT32, m * n, stringVector(), true));
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType i = 0; i < (ompIndexType)std::min(m, n); i++) {
                mat[i * n + i] = (uint32)1;
            }
            vmat = (void*)mat;
        } break;
        case NLS_UINT64: {
            if (bIsSparse) {
                Error(_W("sparse not supported."));
            }
            uint64* mat = static_cast<uint64*>(
                ArrayOf::allocateArrayOf(NLS_UINT64, m * n, stringVector(), true));
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType i = 0; i < (ompIndexType)std::min(m, n); i++) {
                mat[i * n + i] = (int8)1;
            }
            vmat = (void*)mat;
        } break;
        default: {
            Error(_W("Input following \'like'' is not a numeric array."));
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
                Error(_W("sparse not supported."));
            }
            vmat = nullptr;
        } break;
        default: {
            Error(_W("Input following \'like\' is not a numeric array."));
        } break;
        }
    }
    return ArrayOf(classDest, dimMat, vmat, bIsSparse);
}
//=============================================================================
}
// namespace Nelson
//=============================================================================
