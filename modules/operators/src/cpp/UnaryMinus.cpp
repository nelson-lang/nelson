//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <limits>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "UnaryMinus.hpp"
#include "UminusSparse.hpp"
#include "Decomplexify.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
uminusReal(const ArrayOf& A)
{
    indexType nbElements = A.elementCount();
    T* pRes
        = (T*)ArrayOf::allocateArrayOf(A.dataClass(), nbElements, Nelson::stringVector(), false);
    switch (nbElements) {
    case 0: {
    } break;
    case 1: {
        pRes[0] = -static_cast<T*>(const_cast<void*>(static_cast<const void*>(A.dataPointer())))[0];
    } break;
    default: {
        T* pSrc = static_cast<T*>(const_cast<void*>(static_cast<const void*>(A.dataPointer())));
#if WITH_OPENMP
        OMP_PARALLEL_FOR_LOOP(nbElements)
        for (ompIndexType k = 0; k < (ompIndexType)nbElements; ++k) {
            pRes[k] = -pSrc[k];
        }
#else
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, 1>> matRes(pRes, nbElements, 1);
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, 1>> matA(pSrc, nbElements, 1);
        matRes = -matA.array();
#endif
    } break;
    }
    return ArrayOf(A.dataClass(), A.dimensions(), pRes);
}
//=============================================================================
template <class T>
ArrayOf
uminusSignedInteger(const ArrayOf& A)
{
    indexType nbElements = A.elementCount();
    T* pRes
        = (T*)ArrayOf::allocateArrayOf(A.dataClass(), nbElements, Nelson::stringVector(), false);
    const T* pSrc = static_cast<const T*>(A.dataPointer());
    OMP_PARALLEL_FOR_LOOP(nbElements)
    for (ompIndexType k = 0; k < (ompIndexType)nbElements; ++k) {
        T v = pSrc[k];
        // -intmin is not representable: saturate to intmax (and avoid the
        // undefined behavior of negating intmin).
        pRes[k] = (v == std::numeric_limits<T>::min()) ? std::numeric_limits<T>::max()
                                                       : static_cast<T>(-v);
    }
    return ArrayOf(A.dataClass(), A.dimensions(), pRes);
}
//=============================================================================
template <class T>
ArrayOf
uminusComplex(const ArrayOf& A)
{
    indexType nbElements = A.elementCount();
    if (nbElements == 0) {
        // negating no elements gives an empty real array
        return decomplexify(A);
    }
    void* pRes = ArrayOf::allocateArrayOf(A.dataClass(), nbElements, Nelson::stringVector(), false);
    std::complex<T>* pResz = reinterpret_cast<std::complex<T>*>(pRes);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, 1>> matRes(pResz, nbElements, 1);
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.dataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, 1>> matA(Az, nbElements, 1);
    matRes = -matA.array();
    return ArrayOf(A.dataClass(), A.dimensions(), pRes);
}
//=============================================================================
ArrayOf
UnaryMinus(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf res;
    switch (A.dataClass()) {
    case NLS_LOGICAL: {
        if (A.isSparse()) {
            return sparselogical_uminus(A);
        } else {
            res = A;
            res.ensureSingleOwner();
            res.promoteType(NLS_DOUBLE);
            res = uminusReal<double>(res);
        }
    } break;
    case NLS_CHAR: {
        res = A;
        res.ensureSingleOwner();
        res.promoteType(NLS_DOUBLE);
        res = uminusReal<double>(res);
    } break;
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64: {
        void* Cp = ArrayOf::allocateArrayOf(A.dataClass(), A.elementCount(), stringVector(), true);
        res = ArrayOf(A.dataClass(), A.dimensions(), Cp);
    } break;
    case NLS_INT8: {
        res = uminusSignedInteger<int8>(A);
    } break;
    case NLS_INT16: {
        res = uminusSignedInteger<int16>(A);
    } break;
    case NLS_INT32: {
        res = uminusSignedInteger<int32>(A);
    } break;
    case NLS_INT64: {
        res = uminusSignedInteger<int64>(A);
    } break;
    case NLS_SINGLE: {
        if (A.isSparse()) {
            res = sparsedouble_uminus(A);
        } else {
            res = uminusReal<single>(A);
        }
    } break;
    case NLS_DOUBLE: {
        if (A.isSparse()) {
            res = sparsedouble_uminus(A);
        } else {
            res = uminusReal<double>(A);
        }
    } break;
    case NLS_SCOMPLEX: {
        if (A.isSparse()) {
            res = A.isEmpty() ? decomplexify(A) : sparsedouble_uminus(A);
        } else {
            res = uminusComplex<single>(A);
        }
    } break;
    case NLS_DCOMPLEX: {
        if (A.isSparse()) {
            res = A.isEmpty() ? decomplexify(A) : sparsedouble_uminus(A);
        } else {
            res = uminusComplex<double>(A);
        }
    } break;
    default: {
        needToOverload = true;
        return {};
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
