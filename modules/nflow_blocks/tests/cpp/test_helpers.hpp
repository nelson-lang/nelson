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
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "SimEngineTypes.hpp"
#include "doctest.h"
#include <limits>
#include <string>
#include <unordered_map>
#include <vector>
//=============================================================================
namespace nflow_tests {
using Nelson::NFlow::Block;
using Nelson::NFlow::BlockHandlerFn;
using Nelson::NFlow::BlockId;
using Nelson::NFlow::BlockRegistry;
using Nelson::NFlow::BlockState;
using Nelson::NFlow::json;
using Nelson::NFlow::Phase;
using Nelson::NFlow::SimCtx;
using Nelson::NFlow::SimDiagnostics;
constexpr double kEpsilon = 1e-9;
void
ensureHandlersRegistered();
struct Harness
{
    std::vector<Block> blocks;
    std::vector<std::vector<int>> inputIds;
    std::vector<int> outputBase;
    std::unordered_map<std::string, std::string> labelSinks;
    std::unordered_map<std::string, BlockId> nameToId;
    std::vector<BlockState> blockState;
    std::vector<double> outputs;
    std::unordered_map<std::string, double> variables;
    double dt = 0.1;
    double t = 0.0;
    double relTol = 1e-6; // wired into SimCtx::relTol by context()
    // Optional global continuous-state / zero-crossing vectors. When non-empty,
    // context() wires them into SimCtx so DERIVATIVE / ZERO_CROSSING handlers
    // scatter/gather through them (see blockX / blockXdot / blockG).
    std::vector<double> x;
    std::vector<double> xdot;
    std::vector<double> g;
    // Read back from SimCtx after run(): a block (e.g. an FMU returning
    // terminateRequested) may set ctx.stopRequested to ask for an early stop.
    bool stopRequested = false;
    SimDiagnostics diagnostics;

    BlockId
    addBlock(
        const std::string& id, const std::string& type, int inputs, int outputsCount, json params);
    BlockId
    addInput(double value);
    void
    connect(BlockId from, BlockId to, int toPort = 0, int fromPort = 0);
    void
    setOutputValue(BlockId nid, double value, int port = 0);
    double
    output(BlockId nid, int port = 0) const;
    SimCtx
    context();
    BlockHandlerFn
    handler(const std::string& type) const;
    bool
    run(BlockId nid, Phase phase);
};
//=============================================================================
Harness
oneBlock(const std::string& type, json params = json::object(), int inputs = 0, int outputs = 1);
void
checkClose(double actual, double expected, double epsilon = kEpsilon);
//=============================================================================
} // namespace nflow_tests
//=============================================================================
