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
#include "Evaluator.hpp"
#include "Operators.hpp"
#include "OverloadHelpers.hpp"
#include "BinaryOperatorHelpers.hpp"
#include "OverloadRequired.hpp"
#include "Error.hpp"
#include "BytecodeVMInternal.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
zeroBasedToOrdinal(ArrayOf res);
//=============================================================================
ArrayOfVector
Evaluator::subsindexOperator(const ArrayOfVector& m)
{
    ArrayOfVector n;
    n.reserve(m.size());
    for (const auto& k : m) {
        bool wasFound = false;
        ArrayOf res = callOperatorOverload(this, k, SUBSINDEX_OPERATOR_STR, wasFound);
        if (wasFound) {
            n.push_back(res);
        } else {
            OverloadRequired(SUBSINDEX_OPERATOR_STR);
        }
    }
    return n;
}
//=============================================================================
// A class object or handle used to index a built-in value is replaced by the
// result of its subsindex method. Without such a method the index is kept and
// the indexing reports its usual error. Other index types are never looked up.
void
bcvm_detail::resolveObjectIndices(Evaluator* eval, ArrayOfVector& indices)
{
    for (auto& index : indices) {
        const NelsonType type = index.dataClass();
        if (type != NLS_CLASS_ARRAY && type != NLS_CLASSDEF_ARRAY && type != NLS_HANDLE) {
            continue;
        }
        bool wasFound = false;
        ArrayOf res = callOperatorOverload(eval, index, SUBSINDEX_OPERATOR_STR, wasFound);
        if (wasFound) {
            index = zeroBasedToOrdinal(res);
        }
    }
}
//=============================================================================
// The values stay exact: a negative, fractional or NaN zero-based value then
// fails as an invalid index instead of being rounded to a valid position.
ArrayOf
zeroBasedToOrdinal(ArrayOf res)
{
    if (!(res.isNumeric() || res.isLogical()) || res.isComplex() || res.isSparse()) {
        Error(L"Nelson:types:invalidIndex:indexEitherRealPositiveIntegersLogicals",
            L"index must either be real positive integers or logicals.");
    }
    res.promoteType(NLS_DOUBLE);
    ompIndexType len = (ompIndexType)res.elementCount();
    double* dp = static_cast<double*>(res.readWriteDataPointer());
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType k = 0; k < len; ++k) {
        dp[k]++;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
