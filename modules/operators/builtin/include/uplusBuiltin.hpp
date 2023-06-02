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
namespace Nelson::OperatorsGateway {
//=============================================================================
ArrayOfVector
uplusBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
logical_uplusBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
double_uplusBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
single_uplusBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int8_uplusBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int16_uplusBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int32_uplusBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
int64_uplusBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint8_uplusBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint16_uplusBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint32_uplusBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uint64_uplusBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
char_uplusBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson
//=============================================================================
