//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Not.hpp"
#include "Exception.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
boolean_not(indexType N, logical* C, const logical* A)
{
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        C[i] = static_cast<Nelson::logical>(static_cast<Nelson::logical>(A[i]) == 0u);
    }
}
//=============================================================================
ArrayOf
Not(const ArrayOf& A, bool& needToOverload)
{
    ArrayOf C;
    needToOverload = false;
    if (A.getDataClass() == NLS_SCOMPLEX || A.getDataClass() == NLS_DCOMPLEX) {
        Error(_W("Input argument must be real."));
    }
    ArrayOf AA = A;
    try {
        AA.promoteType(NLS_LOGICAL);
    } catch (Exception&) {
        needToOverload = true;
        return {};
    }
    logical* Cp = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getElementCount(), stringVector(), false));
    boolean_not(A.getElementCount(), Cp, static_cast<const logical*>(AA.getDataPointer()));
    C = ArrayOf(NLS_LOGICAL, A.getDimensions(), Cp);
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
