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
#include "IsFinite.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "SparseType.hpp"
#include "i18n.hpp"
#include <Eigen/Sparse>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
boolean_isfinite(indexType N, logical* C, const T* A)
{
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        C[i] = std::isfinite(A[i]);
    }
}
//=============================================================================
template <class T>
void
boolean_isfinite_cplx(indexType N, logical* C, const T* A)
{
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        C[i] = std::isfinite(A[i].real()) && std::isfinite(A[i].imag());
    }
}
//=============================================================================
ArrayOf
IsFinite(const ArrayOf& A)
{
    if (A.isSparse()) {
        void* Cp = Nelson::ArrayOf::allocateArrayOf(
            NLS_LOGICAL, A.elementCount(), stringVector(), false);
        auto* out = static_cast<logical*>(Cp);
        OMP_PARALLEL_FOR_LOOP(A.elementCount())
        for (ompIndexType i = 0; i < (ompIndexType)A.elementCount(); i++) {
            out[i] = static_cast<logical>(1);
        }
        switch (A.dataClass()) {
        case NLS_DOUBLE: {
            const auto* spmatSRC
                = static_cast<const Eigen::SparseMatrix<double, 0, signedIndexType>*>(
                    A.sparseDataPointer());
            for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
                for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(
                         *spmatSRC, k);
                    it; ++it) {
                    out[it.row() + it.col() * A.rows()]
                        = static_cast<logical>(std::isfinite(it.value()));
                }
            }
        } break;
        case NLS_DCOMPLEX: {
            const auto* spmatSRC
                = static_cast<const Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*>(
                    A.sparseDataPointer());
            for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
                for (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>::InnerIterator it(
                         *spmatSRC, k);
                    it; ++it) {
                    out[it.row() + it.col() * A.rows()] = static_cast<logical>(
                        std::isfinite(it.value().real()) && std::isfinite(it.value().imag()));
                }
            }
        } break;
        case NLS_LOGICAL: {
        } break;
        default: {
            Error("Nelson:UndefinedFunction:undefinedFunctionValueInputArgumentsType",
                "Undefined function '%s' for input arguments of type '%s'.", "isfinite",
                ClassName(A));
        } break;
        }
        return ArrayOf(NLS_LOGICAL, A.dimensions(), Cp);
    }
    ArrayOf C;
    switch (A.dataClass()) {
    case NLS_DOUBLE: {
        C = ArrayOf(NLS_LOGICAL, A.dimensions(), nullptr);
        void* Cp = Nelson::ArrayOf::allocateArrayOf(
            NLS_LOGICAL, A.elementCount(), stringVector(), false);
        boolean_isfinite<double>(A.elementCount(), (logical*)Cp, (const double*)A.dataPointer());
        C.setDataPointer(Cp);
    } break;
    case NLS_SINGLE: {
        C = ArrayOf(NLS_LOGICAL, A.dimensions(), nullptr);
        void* Cp = Nelson::ArrayOf::allocateArrayOf(
            NLS_LOGICAL, A.elementCount(), stringVector(), false);
        boolean_isfinite<single>(A.elementCount(), (logical*)Cp, (const single*)A.dataPointer());
        C.setDataPointer(Cp);
    } break;
    case NLS_SCOMPLEX: {
        C = ArrayOf(NLS_LOGICAL, A.dimensions(), nullptr);
        void* Cp = Nelson::ArrayOf::allocateArrayOf(
            NLS_LOGICAL, A.elementCount(), stringVector(), false);
        auto* pValueA = (single*)A.dataPointer();
        auto* cplx = reinterpret_cast<singlecomplex*>(pValueA);
        boolean_isfinite_cplx<singlecomplex>(A.elementCount(), (logical*)Cp, cplx);
        C.setDataPointer(Cp);
    } break;
    case NLS_DCOMPLEX: {
        C = ArrayOf(NLS_LOGICAL, A.dimensions(), nullptr);
        void* Cp = Nelson::ArrayOf::allocateArrayOf(
            NLS_LOGICAL, A.elementCount(), stringVector(), false);
        auto* pValueA = (double*)A.dataPointer();
        auto* cplx = reinterpret_cast<doublecomplex*>(pValueA);
        boolean_isfinite_cplx<doublecomplex>(A.elementCount(), (logical*)Cp, cplx);
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
            CpLogical[i] = static_cast<logical>(1);
        }
        C.setDataPointer(Cp);
    } break;
    default: {
        Error("Nelson:UndefinedFunction:undefinedFunctionValueInputArgumentsType",
            "Undefined function '%s' for input arguments of type '%s'.", "isfinite", ClassName(A));
    } break;
    }
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
