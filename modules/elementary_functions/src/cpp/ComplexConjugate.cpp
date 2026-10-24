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
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#if defined(_NLS_WITH_VML)
#include <mkl_vml.h>
#endif
#include "ComplexConjugate.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "SparseType.hpp"
#include "SparseArrayOf.hpp"
#include "i18n.hpp"
#include "Decomplexify.hpp"
#include <Eigen/Sparse>
#include <memory>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static ArrayOf
SparseComplexConjugateTyped(const ArrayOf& A, NelsonType dataClass)
{
    try {
        indexType rows = A.rows();
        indexType cols = A.columns();
        auto spmatDST = std::make_unique<Eigen::SparseMatrix<T, 0, signedIndexType>>(rows, cols);
        const auto* spmatSRC
            = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(A.sparseDataPointer());
        std::vector<Eigen::Triplet<T, signedIndexType>> triplets;
        triplets.reserve(spmatSRC->nonZeros());
        for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
            for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(
                     *spmatSRC, k);
                it; ++it) {
                T value = std::conj(it.value());
                if (value != T(0., 0.)) {
                    triplets.emplace_back(it.row(), it.col(), value);
                }
            }
        }
        spmatDST->setFromTriplets(triplets.begin(), triplets.end());
        return makeSparseResult(dataClass, A.dimensions(), std::move(spmatDST));
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    return {};
}
//=============================================================================
static ArrayOf
SparseComplexConjugate(const ArrayOf& A)
{
    switch (A.dataClass()) {
    case NLS_DOUBLE: {
        ArrayOf C(A);
        C.ensureSingleOwner();
        return C;
    } break;
    case NLS_DCOMPLEX: {
        return SparseComplexConjugateTyped<doublecomplex>(A, NLS_DCOMPLEX);
    } break;
    case NLS_SINGLE: {
        ArrayOf C(A);
        C.ensureSingleOwner();
        return C;
    } break;
    case NLS_SCOMPLEX: {
        return SparseComplexConjugateTyped<singlecomplex>(A, NLS_SCOMPLEX);
    } break;
    default: {
    } break;
    }
    Error("Nelson:UndefinedFunction:undefinedFunctionValueInputArgumentsType",
        "Undefined function '%s' for input arguments of type '%s'.", "conj", ClassName(A));
    return {};
}
//=============================================================================
ArrayOf
ComplexConjugate(const ArrayOf& A)
{
    NelsonType classA = A.dataClass();
    if (A.isComplex() && A.isEmpty()) {
        // the conjugate of no elements is an empty real array
        return decomplexify(A);
    }
    if (A.isSparse()) {
        return SparseComplexConjugate(A);
    }
    if (classA > NLS_LOGICAL) {
        Error("Nelson:UndefinedFunction:undefinedFunctionValueInputArgumentsType",
            "Undefined function '%s' for input arguments of type '%s'.", "conj", ClassName(A));
    }
    Dimensions dimsA = A.dimensions();
    ArrayOf C;
    if (A.isEmpty()) {
        C = A;
        C.ensureSingleOwner();
        return C;
    }
    switch (classA) {
    case NLS_SCOMPLEX: {
        if (!A.isEmpty()) {
            auto* psingleA = (single*)A.dataPointer();
            singlecomplex* ptrC
                = (singlecomplex*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, dimsA.elementCount());
            C = ArrayOf(NLS_SCOMPLEX, dimsA, ptrC);
            ompIndexType N = (ompIndexType)dimsA.elementCount();
#if defined(_NLS_WITH_VML)
            MKL_Complex8* ptrAz = reinterpret_cast<MKL_Complex8*>(psingleA);
            MKL_Complex8* ptrCz = reinterpret_cast<MKL_Complex8*>(ptrC);
            vcConj((MKL_INT)A.elementCount(), ptrAz, ptrCz);
#else
            auto* Az = reinterpret_cast<singlecomplex*>(psingleA);
            OMP_PARALLEL_FOR_LOOP(N)
            for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
                ptrC[i] = std::conj(Az[i]);
            }
#endif
        }
    } break;
    case NLS_DCOMPLEX: {
        auto* pdoubleA = (double*)A.dataPointer();
        doublecomplex* ptrC
            = (doublecomplex*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, dimsA.elementCount());
        C = ArrayOf(NLS_DCOMPLEX, dimsA, ptrC);
        ompIndexType N = (ompIndexType)dimsA.elementCount();
#if defined(_NLS_WITH_VML)
        MKL_Complex16* ptrAz = reinterpret_cast<MKL_Complex16*>(pdoubleA);
        MKL_Complex16* ptrCz = reinterpret_cast<MKL_Complex16*>(ptrC);
        vzConj((MKL_INT)A.elementCount(), ptrAz, ptrCz);
#else
        auto* Az = reinterpret_cast<doublecomplex*>(pdoubleA);
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            ptrC[i] = std::conj(Az[i]);
        }
#endif
    } break;
    case NLS_DOUBLE:
    case NLS_SINGLE:
    case NLS_INT8:
    case NLS_UINT8:
    case NLS_INT16:
    case NLS_UINT16:
    case NLS_INT32:
    case NLS_UINT32:
    case NLS_INT64:
    case NLS_UINT64: {
        C = A;
        C.ensureSingleOwner();
    } break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    default: {
        Error("Nelson:UndefinedFunction:undefinedFunctionValueInputArgumentsType",
            "Undefined function '%s' for input arguments of type '%s'.", "conj", ClassName(A));
    } break;
    }
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
