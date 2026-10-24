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
#include <cstdint>
//=============================================================================
class FigureFrameSample
{
    int warmup;
    int target;
    int skipped = 0;
    int measured = 0;
    int64_t start = 0;
    int64_t end = 0;

public:
    FigureFrameSample(int warmupFrames, int measuredFrames)
        : warmup(warmupFrames), target(measuredFrames)
    {
    }

    bool
    frame(int64_t timestamp)
    {
        if (complete()) {
            return true;
        }
        if (skipped < warmup) {
            ++skipped;
            start = timestamp;
            return false;
        }
        ++measured;
        end = timestamp;
        return complete();
    }

    bool
    complete() const
    {
        return measured == target;
    }
    int
    frames() const
    {
        return measured;
    }
    int64_t
    duration() const
    {
        return end > start ? end - start : 0;
    }
    double
    fps() const
    {
        return duration() > 0 ? measured * 1.0e9 / duration() : 0;
    }
};
//=============================================================================
