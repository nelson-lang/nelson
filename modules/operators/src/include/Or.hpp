//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "nlsOperators_exports.h"
//=============================================================================
namespace Nelson {
/**
 * Element-wise or of two arrays: C = A or B.
 * C = A || B
 */
NLSOPERATORS_IMPEXP ArrayOf
Or(ArrayOf A, ArrayOf B);
} // namespace Nelson
//=============================================================================
