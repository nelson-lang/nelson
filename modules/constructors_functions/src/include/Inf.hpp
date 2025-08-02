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
#include "nlsConstructors_functions_exports.h"
//=============================================================================
namespace Nelson {
NLSCONSTRUCTORS_FUNCTIONS_IMPEXP ArrayOf
Inf();
NLSCONSTRUCTORS_FUNCTIONS_IMPEXP ArrayOf
Inf(indexType m, indexType n = 1);
NLSCONSTRUCTORS_FUNCTIONS_IMPEXP ArrayOf
Inf(Dimensions& dims);
//=============================================================================
} // namespace Nelson
//=============================================================================
