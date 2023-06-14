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
//=============================================================================
namespace Nelson::SpecialFunctionsGateway {
ArrayOfVector
gcdBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
double_gcdBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
single_gcdBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
logical_gcdBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
uint8_gcdBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
uint16_gcdBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
uint32_gcdBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
uint64_gcdBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
int8_gcdBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
int16_gcdBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
int32_gcdBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
int64_gcdBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
char_gcdBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson
//=============================================================================
