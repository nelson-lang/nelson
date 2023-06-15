//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "UnaryMinus.hpp"
#include "OverloadHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
uminusReal(const ArrayOf& A)
{
    indexType nbElements = A.getElementCount();
    T* pRes = static_cast<T*>(
        ArrayOf::allocateArrayOf(A.getDataClass(), nbElements, Nelson::stringVector(), false));
    T* ptrA = static_cast<T*>(const_cast<void*>(A.getDataPointer()));
    if (nbElements == 1) {
        pRes[0] = -ptrA[0];
    } else {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
        for (ompIndexType k = 0; k < (ompIndexType)nbElements; k++) {
            pRes[k] = -ptrA[k];
        }
#else
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, 1>> matRes(pRes, nbElements, 1);
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, 1>> matA(ptrA, nbElements, 1);
        matRes = -matA.array();
#endif
    }
    return ArrayOf(A.getDataClass(), A.getDimensions(), pRes);
}
//=============================================================================
template <class T>
ArrayOf
uminusComplex(const ArrayOf& A)
{
    indexType nbElements = A.getElementCount();
    void* pRes
        = ArrayOf::allocateArrayOf(A.getDataClass(), nbElements, Nelson::stringVector(), false);
    std::complex<T>* pResz = reinterpret_cast<std::complex<T>*>(pRes);
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());

    if (nbElements == 1) {
        pResz[0] = -Az[0];
    } else {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
        for (ompIndexType k = 0; k < (ompIndexType)nbElements; k++) {
            pResz[k] = -Az[k];
        }
#else
        Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, 1>> matRes(pResz, nbElements, 1);
        Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, 1>> matA(Az, nbElements, 1);
        matRes = -matA.array();
#endif
    }
    return ArrayOf(A.getDataClass(), A.getDimensions(), pRes);
}
//=============================================================================
ArrayOf
UnaryMinus(const ArrayOf& A)
{
    ArrayOf res;
    switch (A.getDataClass()) {
    case NLS_LOGICAL:
    case NLS_CHAR: {
        res = A;
        res.ensureSingleOwner();
        res.promoteType(NLS_DOUBLE);
        return uminusReal<double>(res);
    } break;
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64: {
        void* Cp
            = ArrayOf::allocateArrayOf(A.getDataClass(), A.getElementCount(), stringVector(), true);
        return ArrayOf(A.getDataClass(), A.getDimensions(), Cp);
    } break;
    case NLS_INT8: {
        return uminusReal<int8>(A);
    } break;
    case NLS_INT16: {
        return uminusReal<int16>(A);
    } break;
    case NLS_INT32: {
        return uminusReal<int32>(A);
    } break;
    case NLS_INT64: {
        return uminusReal<int64>(A);
    } break;
    case NLS_SINGLE: {
        return uminusReal<single>(A);
    } break;
    case NLS_DOUBLE: {
        return uminusReal<double>(A);
    } break;
    case NLS_SCOMPLEX: {
        return uminusComplex<single>(A);
    } break;
    case NLS_DCOMPLEX: {
        return uminusComplex<double>(A);
    } break;
    default: {
        OverloadRequired("uminus");
    } break;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
