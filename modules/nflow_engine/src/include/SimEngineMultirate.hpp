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
#include <optional>
#include <string>
#include <vector>
#include "SimEngineTypes.hpp"
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
// Multi-rate helpers of SimEngine::runSimulation.
//=============================================================================
// Non-integer sample times: if any block's generic SampleTime is not an integer
// multiple of the base step, refine the base to the GCD so every rate becomes
// integer, adjusting the sample count to match. A model with only integer
// multiples keeps its dt/samples. Mutates dt and samples in place.
void
refineBaseStepForSampleTimes(const std::vector<Block>& blocks, double& dt, int& samples);
//=============================================================================
// Multi-rate updateRate table (opt-in): a block whose SampleTime is slower than
// the base step updates only every round(Ts/dt) samples, holding its output
// between hits. Fills `updateRate` (caller pre-sizes it to blocks.size(), all 1)
// and returns std::nullopt on success. Returns a result-JSON error string when a
// SampleTime is not an integer sub-rate, belongs to a self-scheduling block, has
// nowhere to hold its output, or is nested inside a subsystem; `time` (empty at
// this point), blocks, blockState and inputIds feed those error payloads.
std::optional<std::string>
computeUpdateRates(const std::vector<Block>& blocks, double dt, const std::vector<int>& updateIdx,
    const std::vector<int>& algIdx, std::vector<int>& updateRate, const std::vector<double>& time,
    const std::vector<BlockState>& blockState, const std::vector<std::vector<int>>& inputIds);
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
