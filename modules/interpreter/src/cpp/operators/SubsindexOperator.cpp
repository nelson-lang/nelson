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
#include "OverloadRequired.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
Evaluator::subsindexOperator(const ArrayOfVector& m)
{
    ArrayOfVector n;
    n.reserve(m.size());
    for (const auto& k : m) {
        bool wasFound = false;
        ArrayOf res = callOverloadedFunction(this,
            NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), k,
            SUBSINDEX_OPERATOR_STR, ClassName(k), k.getDataClass(), wasFound);
        if (wasFound) {
            ompIndexType len = (ompIndexType)res.getElementCount();
#ifdef NLS_INDEX_TYPE_64
            res.promoteType(NLS_UINT64);
            uint64* dp = static_cast<uint64*>(res.getReadWriteDataPointer());
#else
            res.promoteType(NLS_UINT32);
            uint32* dp = static_cast<uint32*>(res.getReadWriteDataPointer());
#endif
            OMP_PARALLEL_FOR_LOOP(len)
            for (ompIndexType k = 0; k < len; ++k) {
                dp[k]++;
            }
            n.push_back(res);
        } else {
            OverloadRequired(SUBSINDEX_OPERATOR_STR);
        }
    }
    return n;
}
//=============================================================================
}
//=============================================================================
