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
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
// get/set accept numeric graphics handles: 0 is the root
// (groot) and a positive value is the figure with that number.
ArrayOfVector
double_getBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
double_setBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
}; // namespace Nelson
//=============================================================================
