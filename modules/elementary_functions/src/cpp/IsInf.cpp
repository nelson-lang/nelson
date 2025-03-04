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
#include "omp_for_loop.hpp"
#include "IsInf.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
boolean_isinf(indexType N, logical* C, const T* A)
{
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        C[i] = std::isinf(A[i]);
    }
}
//=============================================================================
template <class T>
void
boolean_isinf_cplx(indexType N, logical* C, const T* A)
{
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        C[i] = std::isinf(A[i].real()) || std::isinf(A[i].imag());
    }
}
//=============================================================================
ArrayOf
IsInf(const ArrayOf& A)
{
    ArrayOf C;
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        C = ArrayOf(NLS_LOGICAL, A.getDimensions(), nullptr);
        void* Cp = Nelson::ArrayOf::allocateArrayOf(
            NLS_LOGICAL, A.getElementCount(), stringVector(), false);
        boolean_isinf<double>(A.getElementCount(), (logical*)Cp, (const double*)A.getDataPointer());
        C.setDataPointer(Cp);
    } break;
    case NLS_SINGLE: {
        C = ArrayOf(NLS_LOGICAL, A.getDimensions(), nullptr);
        void* Cp = Nelson::ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getElementCount());
        boolean_isinf<single>(A.getElementCount(), (logical*)Cp, (const single*)A.getDataPointer());
        C.setDataPointer(Cp);
    } break;
    case NLS_SCOMPLEX: {
        C = ArrayOf(NLS_LOGICAL, A.getDimensions(), nullptr);
        void* Cp = Nelson::ArrayOf::allocateArrayOf(
            NLS_LOGICAL, A.getElementCount(), stringVector(), false);
        auto* pValueA = (single*)A.getDataPointer();
        auto* cplx = reinterpret_cast<singlecomplex*>(pValueA);
        boolean_isinf_cplx<singlecomplex>(A.getElementCount(), (logical*)Cp, cplx);
        C.setDataPointer(Cp);
    } break;
    case NLS_DCOMPLEX: {
        C = ArrayOf(NLS_LOGICAL, A.getDimensions(), nullptr);
        void* Cp = Nelson::ArrayOf::allocateArrayOf(
            NLS_LOGICAL, A.getElementCount(), stringVector(), false);
        auto* pValueA = (double*)A.getDataPointer();
        auto* cplx = reinterpret_cast<doublecomplex*>(pValueA);
        boolean_isinf_cplx<doublecomplex>(A.getElementCount(), (logical*)Cp, cplx);
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
        C = ArrayOf(NLS_LOGICAL, A.getDimensions(), nullptr);
        void* Cp = Nelson::ArrayOf::allocateArrayOf(
            NLS_LOGICAL, A.getElementCount(), stringVector(), false);
        auto* CpLogical = static_cast<logical*>(Cp);
        ompIndexType elementCount = A.getElementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType i = 0; i < elementCount; i++) {
            CpLogical[i] = static_cast<logical>(0);
        }
        C.setDataPointer(Cp);
    } break;
    default: {
        Error(_("Undefined function 'isinf' for input arguments of type") + " '" + ClassName(A)
            + "'.");
    } break;
    }
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
