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
#include "IsNaN.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "SparseType.hpp"
#include "SparseArrayOf.hpp"
#include "i18n.hpp"
#include <Eigen/Sparse>
#include <memory>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
boolean_isnan(indexType N, logical* C, const T* A)
{
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        C[i] = std::isnan(A[i]);
    }
}
//=============================================================================
template <class T>
void
boolean_isnan_cplx(indexType N, logical* C, const T* A)
{
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        C[i] = std::isnan(A[i].real()) || std::isnan(A[i].imag());
    }
}
//=============================================================================
ArrayOf
IsNaN(const ArrayOf& A)
{
    if (A.isSparse()) {
        auto spmatDST = std::make_unique<Eigen::SparseMatrix<logical, 0, signedIndexType>>(
            A.rows(), A.columns());
        std::vector<Eigen::Triplet<logical, signedIndexType>> triplets;
        switch (A.dataClass()) {
        case NLS_DOUBLE: {
            const auto* spmatSRC
                = static_cast<const Eigen::SparseMatrix<double, 0, signedIndexType>*>(
                    A.sparseDataPointer());
            triplets.reserve(spmatSRC->nonZeros());
            for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
                for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(
                         *spmatSRC, k);
                    it; ++it) {
                    if (std::isnan(it.value())) {
                        triplets.emplace_back(it.row(), it.col(), 1);
                    }
                }
            }
        } break;
        case NLS_DCOMPLEX: {
            const auto* spmatSRC
                = static_cast<const Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*>(
                    A.sparseDataPointer());
            triplets.reserve(spmatSRC->nonZeros());
            for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
                for (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>::InnerIterator it(
                         *spmatSRC, k);
                    it; ++it) {
                    if (std::isnan(it.value().real()) || std::isnan(it.value().imag())) {
                        triplets.emplace_back(it.row(), it.col(), 1);
                    }
                }
            }
        } break;
        case NLS_LOGICAL: {
        } break;
        default: {
            Error("Nelson:UndefinedFunction:undefinedFunctionValueInputArgumentsType",
                "Undefined function '%s' for input arguments of type '%s'.", "isnan", ClassName(A));
        } break;
        }
        spmatDST->setFromTriplets(triplets.begin(), triplets.end());
        ArrayOf C = makeSparseResult(NLS_LOGICAL, A.dimensions(), std::move(spmatDST));
        Dimensions dims = A.dimensions();
        C.resize(dims);
        return C;
    }
    ArrayOf C;
    switch (A.dataClass()) {
    case NLS_DOUBLE: {
        C = ArrayOf(NLS_LOGICAL, A.dimensions(), nullptr);
        void* Cp = Nelson::ArrayOf::allocateArrayOf(
            NLS_LOGICAL, A.elementCount(), stringVector(), false);
        boolean_isnan<double>(A.elementCount(), (logical*)Cp, (const double*)A.dataPointer());
        C.setDataPointer(Cp);
    } break;
    case NLS_SINGLE: {
        C = ArrayOf(NLS_LOGICAL, A.dimensions(), nullptr);
        void* Cp = Nelson::ArrayOf::allocateArrayOf(
            NLS_LOGICAL, A.elementCount(), stringVector(), false);
        boolean_isnan<single>(A.elementCount(), (logical*)Cp, (const single*)A.dataPointer());
        C.setDataPointer(Cp);
    } break;
    case NLS_SCOMPLEX: {
        C = ArrayOf(NLS_LOGICAL, A.dimensions(), nullptr);
        void* Cp = Nelson::ArrayOf::allocateArrayOf(
            NLS_LOGICAL, A.elementCount(), stringVector(), false);
        auto* pValueA = (single*)A.dataPointer();
        auto* cplx = reinterpret_cast<singlecomplex*>(pValueA);
        boolean_isnan_cplx<singlecomplex>(A.elementCount(), (logical*)Cp, cplx);
        C.setDataPointer(Cp);
    } break;
    case NLS_DCOMPLEX: {
        C = ArrayOf(NLS_LOGICAL, A.dimensions(), nullptr);
        void* Cp = Nelson::ArrayOf::allocateArrayOf(
            NLS_LOGICAL, A.elementCount(), stringVector(), false);
        auto* pValueA = (double*)A.dataPointer();
        auto* cplx = reinterpret_cast<doublecomplex*>(pValueA);
        boolean_isnan_cplx<doublecomplex>(A.elementCount(), (logical*)Cp, cplx);
        C.setDataPointer(Cp);
    } break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    case NLS_INT8:
    case NLS_UINT8:
    case NLS_INT16:
    case NLS_UINT16:
    case NLS_INT32:
    case NLS_UINT32:
    case NLS_INT64:
    case NLS_UINT64: {
        C = ArrayOf(NLS_LOGICAL, A.dimensions(), nullptr);
        void* Cp = Nelson::ArrayOf::allocateArrayOf(
            NLS_LOGICAL, A.elementCount(), stringVector(), false);
        auto* CpLogical = static_cast<logical*>(Cp);
        ompIndexType N = (ompIndexType)A.elementCount();
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < N; i++) {
            CpLogical[i] = static_cast<logical>(0);
        }
        C.setDataPointer(Cp);
    } break;
    default: {
        Error("Nelson:UndefinedFunction:undefinedFunctionValueInputArgumentsType",
            "Undefined function '%s' for input arguments of type '%s'.", "isnan", ClassName(A));
    } break;
    }
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
