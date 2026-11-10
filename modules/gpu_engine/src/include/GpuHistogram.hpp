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
// each function returns false when the call is outside what the device covers
namespace Nelson {
//=============================================================================
// counts in [e(k), e(k+1)), the last bin closed; ascending edges
NLSGPU_ENGINE_IMPEXP bool
GpuHistcounts(GpuArrayObject* x, GpuArrayObject* edges, ArrayOf& result);
//=============================================================================
// bin number of every value, NaN outside the edges
NLSGPU_ENGINE_IMPEXP bool
GpuDiscretize(GpuArrayObject* x, GpuArrayObject* edges, ArrayOf& result);
//=============================================================================
// subs: one row of 1-based subscripts per value; op 0 sum, 1 max, 2 min; empty slots = fill
NLSGPU_ENGINE_IMPEXP bool
GpuAccumarray(GpuArrayObject* subs, GpuArrayObject* vals, const std::vector<indexType>& outDims,
    int op, float fill, ArrayOf& result);
//=============================================================================
} // namespace Nelson
//=============================================================================
