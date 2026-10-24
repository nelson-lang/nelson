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
// Fused rotate by k * 90 degrees in the plane of the first two dimensions
// (each higher-dimension page is rotated independently). Dense numeric / char
// / logical / complex arrays only; sets needToOverload otherwise.
NLSELEMENTARY_FUNCTIONS_IMPEXP ArrayOf
Rotate90(const ArrayOf& arrayIn, int nbRotations, bool& needToOverload);
//=============================================================================
} // namespace Nelson
//=============================================================================
