//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "GOFixedVectorProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOFixedVectorProperty::set(ArrayOf num)
{
    GOGenericProperty::set(num);
    num.promoteType(NLS_DOUBLE);
    const double* dp = (const double*)num.getDataPointer();
    _data.clear();
    indexType minLen = std::min((indexType)m_len, num.getElementCount());
    _data.reserve(minLen);
    for (indexType i = 0; i < minLen; i++) {
        _data.push_back(dp[i]);
    }
}
//=============================================================================
}
//=============================================================================
