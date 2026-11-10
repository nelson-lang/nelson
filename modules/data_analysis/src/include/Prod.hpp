//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "nlsData_analysis_exports.h"
//=============================================================================
namespace Nelson {
/* if d == 0 --> Prod(X) */
/* if d != 0 --> Prod(X, n) */
/* if d == static_cast<indexType>(-1) --> Prod(X, 'all') */
/* lastDim > d: dimensions d..lastDim are reduced as one line (dense arrays only) */
NLSDATA_ANALYSIS_IMPEXP ArrayOf
Prod(ArrayOf A, indexType d, const std::wstring& strtype, bool withnan, indexType lastDim = 0);
//=============================================================================
} // namespace Nelson
//=============================================================================
