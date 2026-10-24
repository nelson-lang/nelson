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
// Element-wise membership within tolerance.
//   lia(i)  = true if some B(j) is within tol*DataScale of A(i)
//   locb(i) = lowest index j in B within tolerance (0 if none); computed only
//             when computeLocb is true.
// tol < 0 selects the default (1e-12 for double, 1e-6 for single inputs).
NLSDATA_ANALYSIS_IMPEXP void
IsMemberTol(const ArrayOf& A, const ArrayOf& B, double tol, bool computeLocb, ArrayOf& lia,
    ArrayOf& locb, bool& needToOverload);
//=============================================================================
} // namespace Nelson
//=============================================================================
