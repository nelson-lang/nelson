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
#include "nlsTrigonometric_functions_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSTRIGONOMETRIC_FUNCTIONS_IMPEXP ArrayOf
Cos(const ArrayOf& A, bool& needToOverload);
//=============================================================================
NLSTRIGONOMETRIC_FUNCTIONS_IMPEXP ArrayOf
Sin(const ArrayOf& A, bool& needToOverload);
//=============================================================================
NLSTRIGONOMETRIC_FUNCTIONS_IMPEXP ArrayOf
Tan(const ArrayOf& A, bool& needToOverload);
//=============================================================================
NLSTRIGONOMETRIC_FUNCTIONS_IMPEXP ArrayOf
Acos(const ArrayOf& A, bool& needToOverload);
//=============================================================================
NLSTRIGONOMETRIC_FUNCTIONS_IMPEXP ArrayOf
Asin(const ArrayOf& A, bool& needToOverload);
//=============================================================================
NLSTRIGONOMETRIC_FUNCTIONS_IMPEXP ArrayOf
Atan(const ArrayOf& A, bool& needToOverload);
//=============================================================================
NLSTRIGONOMETRIC_FUNCTIONS_IMPEXP ArrayOf
Cosh(const ArrayOf& A, bool& needToOverload);
//=============================================================================
NLSTRIGONOMETRIC_FUNCTIONS_IMPEXP ArrayOf
Sinh(const ArrayOf& A, bool& needToOverload);
//=============================================================================
NLSTRIGONOMETRIC_FUNCTIONS_IMPEXP ArrayOf
Tanh(const ArrayOf& A, bool& needToOverload);
//=============================================================================
NLSTRIGONOMETRIC_FUNCTIONS_IMPEXP ArrayOf
Atanh(const ArrayOf& A, bool& needToOverload);
//=============================================================================
} // namespace Nelson
//=============================================================================
