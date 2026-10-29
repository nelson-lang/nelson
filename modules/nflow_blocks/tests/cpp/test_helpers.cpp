//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "test_helpers.hpp"
#include <algorithm>
#include <cmath>
#include <iostream>
#include <stdexcept>
//=============================================================================
namespace nflow_tests {
void
ensureHandlersRegistered()
{
    static bool done = false;
    if (!done) {
        Nelson::NFlow::registerAllBuiltinHandlers();
        done = true;
    }
}
//=============================================================================
BlockId
Harness::addBlock(
    const std::string& id, const std::string& type, int inputs, int outputsCount, json params)
{
    Block block;
    block.id = id;
    block.nid = static_cast<BlockId>(blocks.size());
    block.type = type;
    block.inputs = inputs;
    block.outputs = outputsCount;
    asJson(block.params) = std::move(params);

    nameToId[block.id] = block.nid;
    outputBase.push_back(static_cast<int>(outputs.size()));
    inputIds.push_back(std::vector<int>(inputs, -1));
    blockState.emplace_back();
    outputs.resize(outputs.size() + static_cast<size_t>(std::max(1, outputsCount)), 0.0);
    blocks.push_back(std::move(block));
    return static_cast<BlockId>(blocks.size() - 1);
}
//=============================================================================
BlockId
Harness::addInput(double value)
{
    BlockId id
        = addBlock("__input" + std::to_string(blocks.size()), "__input", 0, 1, json::object());
    setOutputValue(id, value);
    return id;
}
//=============================================================================
void
Harness::connect(BlockId from, BlockId to, int toPort, int fromPort)
{
    if (toPort >= static_cast<int>(inputIds[to].size())) {
        inputIds[to].resize(static_cast<size_t>(toPort + 1), -1);
        blocks[to].inputs = toPort + 1;
    }
    inputIds[to][toPort] = outputBase[from] + fromPort;
}
//=============================================================================
void
Harness::setOutputValue(BlockId nid, double value, int port)
{
    outputs[outputBase[nid] + port] = value;
}
//=============================================================================
double
Harness::output(BlockId nid, int port) const
{
    return outputs[outputBase[nid] + port];
}
//=============================================================================
SimCtx
Harness::context()
{
    SimCtx ctx { blocks, inputIds, outputBase, labelSinks, nameToId, blockState, outputs, variables,
        dt, t };
    ctx.diag = &diagnostics;
    ctx.relTol = relTol;
    if (!x.empty()) {
        ctx.x = x.data();
        ctx.nx = static_cast<int>(x.size());
    }
    if (!xdot.empty()) {
        ctx.xdot = xdot.data();
    }
    if (!g.empty()) {
        ctx.g = g.data();
    }
    return ctx;
}
//=============================================================================
BlockHandlerFn
Harness::handler(const std::string& type) const
{
    ensureHandlersRegistered();
    return BlockRegistry::instance().getHandler(type);
}
//=============================================================================
bool
Harness::run(BlockId nid, Phase phase)
{
    auto fn = handler(blocks[nid].type);
    if (!fn) {
        throw std::runtime_error("missing handler for " + blocks[nid].type);
    }
    SimCtx ctx = context();
    bool ok = fn(ctx, blocks[nid], phase);
    stopRequested = ctx.stopRequested;
    return ok;
}
//=============================================================================
Harness
oneBlock(const std::string& type, json params, int inputs, int outputs)
{
    Harness h;
    h.variables[nflow::kPi] = M_PI;
    h.variables[nflow::kE] = M_E;
    h.addBlock("b", type, inputs, outputs, std::move(params));
    return h;
}
//=============================================================================
void
checkClose(double actual, double expected, double epsilon)
{
    if (!(actual == doctest::Approx(expected).epsilon(epsilon))) {
        std::cerr << "expected " << expected << ", got " << actual << std::endl;
    }
    CHECK(actual == doctest::Approx(expected).epsilon(epsilon));
}
//=============================================================================
} // namespace nflow_tests
//=============================================================================
