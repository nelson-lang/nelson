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
/**
 * min operation.
 */
//=============================================================================
// C = min(A, B);
NLSDATA_ANALYSIS_IMPEXP ArrayOf
Minimum(bool omitNaN, const ArrayOf& A, const ArrayOf& B, bool& needToOverload);
//=============================================================================
// [M, i] = min(A)
NLSDATA_ANALYSIS_IMPEXP ArrayOfVector
Minimum(bool omitNaN, const ArrayOf& A, int nLhs, bool& needToOverload);
//=============================================================================
// [M, i] = min(A, [], dim)
NLSDATA_ANALYSIS_IMPEXP ArrayOfVector
Minimum(bool omitNaN, const ArrayOf& A, indexType dim, int nLhs, bool& needToOverload);
//=============================================================================
NLSDATA_ANALYSIS_IMPEXP ArrayOf
MinimumAll(bool omitNaN, const ArrayOf& A, bool& needToOverload);
//=============================================================================
} // namespace Nelson
//=============================================================================
