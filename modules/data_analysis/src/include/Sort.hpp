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
//=============================================================================
enum MISSING_PLACEMENT
{
    AUTO_PLACEMENT,
    FIRST_PLACEMENT,
    LAST_PLACEMENT
};
//=============================================================================
enum COMPARISON_METHOD
{
    AUTO_METHOD,
    REAL_METHOD,
    ABS_METHOD
};
//=============================================================================
NLSDATA_ANALYSIS_IMPEXP ArrayOfVector
Sort(const ArrayOf& arrayIn, size_t nargin, bool withIndex, indexType dim, bool ascend,
    MISSING_PLACEMENT placement, COMPARISON_METHOD comparisonMethod, bool& needToOverload);
}
//=============================================================================
