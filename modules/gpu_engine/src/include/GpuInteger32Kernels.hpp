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
#include <string>
#include <vector>
#include "ArrayOf.hpp"
#include "Dimensions.hpp"
#include "GpuArrayObject.hpp"
#include "nlsGpu_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// int32 / uint32 support. Unlike int8/16 (which reuse the f32 kernels), 32-bit
// integers cannot be represented exactly in f32, so their buffer holds the raw
// integer bits and dedicated kernels bitcast<i32>/<u32> to operate on them with
// exact saturating arithmetic. Only value-interpreting ops need this path;
// byte-moving ops (reshape/transpose/concat/indexing) work unchanged.
//=============================================================================
/**
 * Result integer class ("int32"/"uint32") of a binary op where at least one
 * operand is a 32-bit-integer gpuArray. Two 32-bit integers must share the same
 * class; mixing with a small integer class (int8/16) is rejected; a real
 * (single/logical) operand is allowed and converted. Returns "" if neither
 * operand is a 32-bit integer.
 */
NLSGPU_ENGINE_IMPEXP std::string
GpuInt32ResultType(GpuArrayObject* a, GpuArrayObject* b);
//=============================================================================
/** Saturating int32/uint32 binary op: op in {plus,minus,times,max,min}. */
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuInt32Binary(GpuArrayObject* a, GpuArrayObject* b, const std::string& op);
//=============================================================================
/** Saturating int32/uint32 unary op: op in {uminus,abs}. */
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuInt32Unary(GpuArrayObject* a, const std::string& op);
//=============================================================================
/** Saturating int32/uint32 power (base .^ exp), keeping the integer class. */
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuInt32Power(GpuArrayObject* a, GpuArrayObject* b);
//=============================================================================
/**
 * Raise an error when the exponent gpuArray holds any negative value: integer
 * power is only defined for non-negative integral exponents (host semantics).
 */
NLSGPU_ENGINE_IMPEXP void
GpuRequireNonNegativeExponent(GpuArrayObject* b);
//=============================================================================
/** int32/uint32 comparison (logical result): cmp in {eq,ne,lt,le,gt,ge}. */
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuInt32Compare(GpuArrayObject* a, GpuArrayObject* b, const std::string& cmp);
//=============================================================================
/**
 * Saturating int32/uint32 reduction along a dimension: op in {sum,prod,max,min}.
 * sum/prod saturate to the class range; max/min keep the class. Result stays
 * device resident with the same integer class.
 */
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuInt32Reduce(GpuArrayObject* a, int dim, const std::string& op);
//=============================================================================
} // namespace Nelson
//=============================================================================
