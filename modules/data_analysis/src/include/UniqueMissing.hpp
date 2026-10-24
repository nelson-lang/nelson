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
#include "nlsData_analysis_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Merge the missing values (NaN, <missing>) of a unique result {C, ia, ic}
// (first occurrence indices) so that repeated missing values count as
// duplicates. With asRows, rows are duplicates when they have missing values
// in the same columns and equal nonmissing values in the other columns.
NLSDATA_ANALYSIS_IMPEXP ArrayOfVector
UniqueMergeMissing(const ArrayOfVector& base, bool asRows);
//=============================================================================
} // namespace Nelson
//=============================================================================
