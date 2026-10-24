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
// Complex support. Arrays store interleaved (re, im) f32 pairs. Real operands
// are promoted to complex (im = 0) as needed. Result stays device resident.
//=============================================================================
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexAdd(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexSub(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexMul(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexDiv(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexPower(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexEq(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexNe(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexNegate(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexExp(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexLog(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexSqrt(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexSin(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexCos(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexTan(GpuArrayObject* a);
//=============================================================================
/**
 * Promote a real gpuArray to a new complex gpuArray (im = 0). The caller owns
 * the returned object and must delete it. If `a` is already complex, returns a
 * clone. Used to align operands for mixed real/complex operations.
 */
NLSGPU_ENGINE_IMPEXP GpuArrayObject*
GpuPromoteToComplex(GpuArrayObject* a);
//=============================================================================
// Introspection / extraction (accept real or complex input)
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuReal(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuImag(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuConj(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexAbs(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuAngle(GpuArrayObject* a);
//=============================================================================
// Complex rounding / sign / predicates (per-component; sign(z) = z/|z|).
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexFloor(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexCeil(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexRound(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexFix(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexSign(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexIsNaN(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexIsInf(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuComplexIsFinite(GpuArrayObject* a);
//=============================================================================
} // namespace Nelson
//=============================================================================
