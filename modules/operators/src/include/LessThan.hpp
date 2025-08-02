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
#include "nlsOperators_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
/**
 * Element-wise compare (lt) of two arrays.
 */
NLSOPERATORS_IMPEXP ArrayOf
LessThan(const ArrayOf& A, const ArrayOf& B, bool& needToOverload);
}
//=============================================================================
