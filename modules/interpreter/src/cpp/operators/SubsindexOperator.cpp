//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Evaluator.hpp"
#include "Operators.hpp"
#include "OverloadUnaryOperator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
Evaluator::subsindexOperator(const ArrayOfVector& m)
{
    ArrayOfVector n;
    n.reserve(m.size());
    for (const auto& k : m) {
        ArrayOf t = OverloadUnaryOperator(this, k, SUBSINDEX_OPERATOR_STR);
        t.promoteType(NLS_UINT32);
        indexType len = t.getElementCount();
        uint32* dp = (uint32*)t.getReadWriteDataPointer();
        for (indexType j = 0; j < len; j++) {
            dp[j]++;
        }
        n.push_back(t);
    }
    return n;
}
//=============================================================================
}
//=============================================================================
