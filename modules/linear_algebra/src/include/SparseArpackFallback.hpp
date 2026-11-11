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
#include "nlsLinear_algebra_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
EigsSparseDenseFallback(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
SvdsSparseDenseFallback(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP ArrayOfVector
SvdsDense(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP indexType
SvdsRequestedK(const ArrayOfVector& argIn, indexType limit);
//=============================================================================
} // namespace Nelson
//=============================================================================
