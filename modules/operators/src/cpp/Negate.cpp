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
#include "Negate.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
negate(indexType N, T* C, const T* A)
{
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        C[i] = -A[i];
    }
}
//=============================================================================
ArrayOf
Negate(ArrayOf A)
{
    ArrayOf C;
    NelsonType Aclass;
    if (A.isReferenceType()) {
        Error(L"Cannot negate non-numeric types.");
    }
    Aclass = A.getDataClass();
    if (A.isRowVectorCharacterArray()) {
        Aclass = NLS_DOUBLE;
    } else if (Aclass < NLS_INT32) {
        Aclass = NLS_INT32;
    }
    A.promoteType(Aclass);
    C = ArrayOf(Aclass, A.getDimensions(), nullptr);
    void* Cp = Nelson::ArrayOf::allocateArrayOf(Aclass, A.getElementCount(), stringVector(), false);
    switch (Aclass) {
    case NLS_INT32:
        negate<int32>(A.getElementCount(), static_cast<int32*>(Cp), (int32*)A.getDataPointer());
        break;
    case NLS_SINGLE:
        negate<float>(A.getElementCount(), static_cast<float*>(Cp), (float*)A.getDataPointer());
        break;
    case NLS_DOUBLE:
        negate<double>(A.getElementCount(), static_cast<double*>(Cp), (double*)A.getDataPointer());
        break;
    case NLS_SCOMPLEX:
        negate<float>(2 * A.getElementCount(), static_cast<float*>(Cp), (float*)A.getDataPointer());
        break;
    case NLS_DCOMPLEX:
        negate<double>(
            2 * A.getElementCount(), static_cast<double*>(Cp), (double*)A.getDataPointer());
        break;
    default: {
    } break;
    }
    C.setDataPointer(Cp);
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
