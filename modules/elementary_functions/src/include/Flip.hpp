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
#include "nlsElementary_functions_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSELEMENTARY_FUNCTIONS_IMPEXP ArrayOf
Fliplr(const ArrayOf& arrayIn, bool& needToOverload);
//=============================================================================
NLSELEMENTARY_FUNCTIONS_IMPEXP ArrayOf
Flipud(const ArrayOf& arrayIn, bool& needToOverload);
//=============================================================================
// General N-D flip along a 1-based dimension for dense numeric / char /
// logical / complex arrays. Sets needToOverload for unsupported inputs.
NLSELEMENTARY_FUNCTIONS_IMPEXP ArrayOf
Flip(const ArrayOf& arrayIn, indexType dim, bool& needToOverload);
//=============================================================================
} // namespace Nelson
//=============================================================================
