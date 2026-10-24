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
#include <unordered_map>
#include <vector>
#include <nlohmann/json.hpp>
#include "SimEngineTypes.hpp"
#include "SimEngineBusTypes.hpp"
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
// The model-topology build step of SimEngine::runSimulation. From the parsed
// diagram it builds the block list (with numeric ids), the variables map, the
// bus-type registry, the sample step / count, the resolved connection list, the
// label-sink map and the signal layout, then derives each block's output-slot
// base and its per-port input source slots. Missing sinks, an invalid bus
// declaration or a failed signal layout are reported.
//
// The caller keeps the JSON parse and ensureBuiltinsRegistered() (the signal
// layout needs the builtin metadata).
struct ModelTopology
{
    std::vector<Block> blocks;
    std::unordered_map<std::string, BlockId> nameToId;
    std::unordered_map<std::string, double> variables;
    BusRegistry busRegistry;
    double dt = 0.01;
    int samples = 0;
    std::unordered_map<std::string, std::string> labelSinks;
    SignalLayout sigLayout;
    std::vector<int> outputBase;
    std::vector<std::vector<int>> inputIds;
    int totalSlots = 0;
};
//=============================================================================
// Fills `out` and returns std::nullopt on success; returns a result-JSON error
// string when the diagram has no sink, an invalid bus declaration or a signal
// layout error.
std::optional<std::string>
buildModelTopology(const nlohmann::json& diagram, ModelTopology& out);
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
