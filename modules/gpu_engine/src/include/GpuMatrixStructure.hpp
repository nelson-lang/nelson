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
#include "GpuArrayObject.hpp"
#include "nlsGpu_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// shape facts of a 2-D matrix, read back from one device pass
struct GpuStructureInfo
{
    indexType rows = 0;
    indexType columns = 0;
    // farthest nonzero below / above the main diagonal (0 when none)
    indexType lowerBandwidth = 0;
    indexType upperBandwidth = 0;
    bool symmetric = false;
    bool skewSymmetric = false;
    bool hermitian = false;
    bool skewHermitian = false;
};
//=============================================================================
// false for an N-D or integer array (the host answers instead)
NLSGPU_ENGINE_IMPEXP bool
GpuMatrixStructure(GpuArrayObject* a, GpuStructureInfo& info);
//=============================================================================
enum class GpuStructuredKind
{
    Toeplitz = 0,
    Hankel = 1,
    Vander = 2,
    DiagonalMatrix = 3,
    Diagonal = 4,
    Companion = 5
};
//=============================================================================
// rows x columns real matrix built from u (and w for Toeplitz/Hankel); k is the diagonal
// index and uRows the row count of u when a diagonal is extracted from it
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuStructuredMatrix(GpuStructuredKind kind, GpuArrayObject* u, GpuArrayObject* w, indexType rows,
    indexType columns, int k = 0, indexType uRows = 0);
//=============================================================================
} // namespace Nelson
//=============================================================================
