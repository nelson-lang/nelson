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
#include "Types.hpp"
#include "nlsGpu_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class AnonymousMacroFunctionDef;
//=============================================================================
/**
 * Result of translating an anonymous function body into a WGSL expression.
 * Each parameter is referenced in `wgslExpr` as p0, p1, ... following the
 * order of the function's argument list.
 */
struct GpuExprCompileResult
{
    bool ok = false;
    std::string wgslExpr; // e.g. "((p0 * p1) + sin(p0))"
    size_t parameterCount = 0; // number of @(...) parameters referenced by the kernel
    bool logicalResult = false; // true when the top-level op yields a logical (0/1)
    std::string failureReason; // populated when !ok (for diagnostics / fallback)
};
//=============================================================================
/**
 * Translate the body of a capture-simple anonymous function into a scalar WGSL
 * expression suitable for an elementwise kernel. Returns ok=false (with a
 * reason) for any unsupported construct so the caller can fall back to CPU.
 */
NLSGPU_ENGINE_IMPEXP GpuExprCompileResult
GpuCompileAnonymousBody(AnonymousMacroFunctionDef* def);
//=============================================================================
/**
 * Translate the body into an i32 WGSL expression that saturates every step to
 * [lo, hi] (the range of a small integer class: int8/uint8/int16/uint16). Only
 * a safe subset is accepted - +, -, .*, unary minus, relational, logical, min,
 * max, abs - so the running values always fit i32; any other construct (or a
 * non-integer literal) returns ok=false for a host fallback. wgslExpr is i32.
 */
NLSGPU_ENGINE_IMPEXP GpuExprCompileResult
GpuCompileAnonymousBodyInteger(AnonymousMacroFunctionDef* def, long lo, long hi);
//=============================================================================
} // namespace Nelson
//=============================================================================
