//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/Dense>
#include "Ones.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
ArrayOf
Ones(NelsonType cl)
{
    Dimensions dims(1, 1);
    return Ones(dims, cl);
}
//=============================================================================
ArrayOf
Ones(Dimensions& dims, NelsonType cl)
{
    dims.simplify();
    if (dims.isEmpty(false)) {
        ArrayOf res = ArrayOf::emptyConstructor(dims);
        res.promoteType(cl);
        return res;
    }
    switch (cl) {
    case NLS_LOGICAL: {
        indexType nbElements = dims.getElementCount();
        logical* mat = nullptr;
        if (nbElements != 0) {
            mat = static_cast<logical*>(
                ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)nbElements; ++k) {
                mat[k] = (logical)1;
            }
        }
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_INT8: {
        indexType nbElements = dims.getElementCount();
        int8* mat = nullptr;
        if (nbElements != 0) {
            mat = static_cast<int8*>(
                ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)nbElements; ++k) {
                mat[k] = (int8)1;
            }
        }
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_UINT8: {
        indexType nbElements = dims.getElementCount();
        uint8* mat = nullptr;
        if (nbElements != 0) {
            mat = static_cast<uint8*>(
                ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)nbElements; ++k) {
                mat[k] = (uint8)1;
            }
        }
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_INT16: {
        indexType nbElements = dims.getElementCount();
        int16* mat = nullptr;
        if (nbElements != 0) {
            mat = static_cast<int16*>(
                ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)nbElements; ++k) {
                mat[k] = (int16)1;
            }
        }
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_UINT16: {
        indexType nbElements = dims.getElementCount();
        uint16* mat = nullptr;
        if (nbElements != 0) {
            mat = static_cast<uint16*>(
                ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)nbElements; ++k) {
                mat[k] = (uint16)1;
            }
        }
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_INT32: {
        indexType nbElements = dims.getElementCount();
        int32* mat = nullptr;
        if (nbElements != 0) {
            mat = static_cast<int32*>(
                ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)nbElements; ++k) {
                mat[k] = (int32)1;
            }
        }
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_UINT32: {
        indexType nbElements = dims.getElementCount();
        uint32* mat = nullptr;
        if (nbElements != 0) {
            mat = static_cast<uint32*>(
                ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)nbElements; ++k) {
                mat[k] = (uint32)1;
            }
        }
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_INT64: {
        indexType nbElements = dims.getElementCount();
        int64* mat = nullptr;
        if (nbElements != 0) {
            mat = static_cast<int64*>(
                ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)nbElements; ++k) {
                mat[k] = (int64)1;
            }
        }
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_UINT64: {
        indexType nbElements = dims.getElementCount();
        uint64* mat = nullptr;
        if (nbElements != 0) {
            mat = static_cast<uint64*>(
                ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)nbElements; ++k) {
                mat[k] = (uint64)1;
            }
        }
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_SINGLE: {
        indexType nbElements = dims.getElementCount();
        single* mat = nullptr;
        if (nbElements != 0) {
            mat = static_cast<single*>(
                ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)nbElements; ++k) {
                mat[k] = (single)1;
            }
        }
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_DOUBLE: {
        indexType nbElements = dims.getElementCount();
        double* mat = nullptr;
        if (nbElements != 0) {
            mat = static_cast<double*>(
                ArrayOf::allocateArrayOf(cl, nbElements, Nelson::stringVector(), false));
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)nbElements; ++k) {
                mat[k] = (double)1;
            }
        }
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_SCOMPLEX: {
        indexType nbElements = dims.getElementCount();
        single* mat = nullptr;
        if (nbElements != 0) {
            mat = static_cast<single*>(
                ArrayOf::allocateArrayOf(cl, nbElements * 2, Nelson::stringVector(), false));
            auto* Cz = reinterpret_cast<singlecomplex*>(mat);
            Eigen::Map<Eigen::Matrix<singlecomplex, Eigen::Dynamic, Eigen::Dynamic>> matEigen(
                (singlecomplex*)Cz, 1, dims.getElementCount());
            matEigen.setOnes();
        }
        return ArrayOf(cl, dims, mat, false);
    } break;
    case NLS_DCOMPLEX: {
        indexType nbElements = dims.getElementCount();
        double* mat = nullptr;
        if (nbElements != 0) {
            mat = static_cast<double*>(
                ArrayOf::allocateArrayOf(cl, nbElements * 2, Nelson::stringVector(), false));
            auto* Cz = reinterpret_cast<doublecomplex*>(mat);
            Eigen::Map<Eigen::Matrix<doublecomplex, Eigen::Dynamic, Eigen::Dynamic>> matEigen(
                (doublecomplex*)Cz, 1, dims.getElementCount());
            matEigen.setOnes();
        }
        return ArrayOf(cl, dims, mat, false);
    } break;
    default:
        Error(ERROR_TYPE_NOT_SUPPORTED);
    }
    return ArrayOf();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
