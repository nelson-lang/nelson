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
#include "nlsBuildConfig.h"
#include "Types.hpp"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Calls scanLine(first, step) once per line of an array seen as
// planes x linesize x planesize: first is the linear index of the first
// element visited (the last one when reverse is true) and step the signed
// distance to the next one. Lines are independent, so they are distributed
// over threads; each line is scanned sequentially by a single thread, which
// keeps results identical to the serial order. A single line never spawns a
// thread team, and small arrays stay below the runtime OpenMP threshold.
template <class ScanLine>
void
forEachCumulativeLine(ompIndexType planes, ompIndexType planesize, ompIndexType linesize,
    bool reverse, ScanLine scanLine)
{
    const ompIndexType lineCount = planes * planesize;
    const ompIndexType parallelWork = lineCount > 1 ? lineCount * linesize : 0;
    const ompIndexType step = reverse ? -planesize : planesize;
    const ompIndexType lastOffset = linesize > 0 ? (linesize - 1) * planesize : 0;
    OMP_PARALLEL_FOR_LOOP(parallelWork)
    for (ompIndexType line = 0; line < lineCount; line++) {
        const ompIndexType first = (line / planesize) * planesize * linesize + line % planesize;
        scanLine(reverse ? first + lastOffset : first, step);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
