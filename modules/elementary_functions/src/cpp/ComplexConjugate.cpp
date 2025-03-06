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
#include "lapack_eigen_config.hpp"
#if defined(_NLS_WITH_VML)
#include <mkl_vml.h>
#endif
#include "ComplexConjugate.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ComplexConjugate(const ArrayOf& A)
{
    NelsonType classA = A.getDataClass();
    if (classA > NLS_LOGICAL || A.isSparse()) {
        Error(_("Undefined function 'conj' for input arguments of type") + " '" + ClassName(A)
            + "'.");
    }
    Dimensions dimsA = A.getDimensions();
    ArrayOf C;
    if (A.isEmpty()) {
        C = A;
        C.ensureSingleOwner();
        return C;
    }
    switch (classA) {
    case NLS_SCOMPLEX: {
        if (!A.isEmpty()) {
            auto* psingleA = (single*)A.getDataPointer();
            singlecomplex* ptrC
                = (singlecomplex*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, dimsA.getElementCount());
            C = ArrayOf(NLS_SCOMPLEX, dimsA, ptrC);
            ompIndexType N = (ompIndexType)dimsA.getElementCount();
#if defined(_NLS_WITH_VML)
            MKL_Complex8* ptrAz = reinterpret_cast<MKL_Complex8*>(psingleA);
            MKL_Complex8* ptrCz = reinterpret_cast<MKL_Complex8*>(ptrC);
            vcConj((MKL_INT)A.getElementCount(), ptrAz, ptrCz);
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
        auto* pdoubleA = (double*)A.getDataPointer();
        doublecomplex* ptrC
            = (doublecomplex*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, dimsA.getElementCount());
        C = ArrayOf(NLS_DCOMPLEX, dimsA, ptrC);
        ompIndexType N = (ompIndexType)dimsA.getElementCount();
#if defined(_NLS_WITH_VML)
        MKL_Complex16* ptrAz = reinterpret_cast<MKL_Complex16*>(pdoubleA);
        MKL_Complex16* ptrCz = reinterpret_cast<MKL_Complex16*>(ptrC);
        vzConj((MKL_INT)A.getElementCount(), ptrAz, ptrCz);
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
        // returns same value
        C = A;
        C.ensureSingleOwner();
    } break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    default: {
        Error(_("Undefined function 'conj' for input arguments of type") + " '" + ClassName(A)
            + "'.");
    } break;
    }
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
