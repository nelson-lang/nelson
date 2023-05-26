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
#include "Evaluator.hpp"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson::OperatorsGateway {
//=============================================================================
ArrayOfVector
generic_colonBuiltin(NelsonType nlsType, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
double_colonBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
single_colonBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int8_colonBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int16_colonBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int32_colonBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int64_colonBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint8_colonBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint16_colonBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint32_colonBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint64_colonBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
char_colonBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson
//=============================================================================
