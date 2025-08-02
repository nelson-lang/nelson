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
#include "mex.h"
#include "ArrayOf.hpp"
#include "nlsMex_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * convert ArrayOf to mxArray
 * internal function
 */
NLSMEX_IMPEXP
mxArray*
ArrayOfToMxArray(const ArrayOf& nlsArrayOf, bool interleavedComplex);
//=============================================================================
/**
 * convert mxArray to ArrayOf
 * internal function
 */
NLSMEX_IMPEXP
ArrayOf
MxArrayToArrayOf(const mxArray* mtlbArray);
//=============================================================================
}
//=============================================================================
