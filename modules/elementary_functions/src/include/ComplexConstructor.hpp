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
NLSELEMENTARY_FUNCTIONS_IMPEXP ArrayOf
ComplexConstructor(ArrayOf arrayA, ArrayOf arrayB);
NLSELEMENTARY_FUNCTIONS_IMPEXP ArrayOf
ComplexConstructor(const ArrayOf& arrayA);
} // namespace Nelson
  //=============================================================================
