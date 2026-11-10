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
#include <vector>
#include "ArrayOf.hpp"
#include "GpuArrayObject.hpp"
#include "nlsGpu_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// discrete Fourier transform of length `length` along dim (dim <= 0: first dimension not of
// length 1), zero padded or truncated; any length (power of two or not); complex result
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuFftAlong(GpuArrayObject* a, indexType length, int dim, bool inverse);
//=============================================================================
// complex array equal to the conjugate of its mirror image across the 1-based dims
NLSGPU_ENGINE_IMPEXP bool
GpuIsConjugateSymmetric(GpuArrayObject* a, const std::vector<int>& dims);
//=============================================================================
// makes the spectrum of real data exactly conjugate symmetric across the dims (in place)
NLSGPU_ENGINE_IMPEXP void
GpuMakeConjugateSymmetric(GpuArrayObject* a, const std::vector<int>& dims);
//=============================================================================
} // namespace Nelson
//=============================================================================
