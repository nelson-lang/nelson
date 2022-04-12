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
#include "Evaluator.hpp"
#include "nlsConstructors_functions_exports.h"
//=============================================================================
namespace Nelson {
NLSCONSTRUCTORS_FUNCTIONS_IMPEXP ArrayOf
Zeros(NelsonType cl);
NLSCONSTRUCTORS_FUNCTIONS_IMPEXP ArrayOf
Zeros(Dimensions& dims, NelsonType cl);
} // namespace Nelson
//=============================================================================
