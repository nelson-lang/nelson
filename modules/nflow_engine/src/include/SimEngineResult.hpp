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
#include "SimEngineTypes.hpp"
#include "SimEngineBusTypes.hpp"
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
// JSON string escaping for values the result payload carries verbatim (block ids,
// bus member names). Diagram editors wrap long labels, so a block id routinely
// holds a newline; emitted raw it made the reader reject the whole result.
// Escapes the two structural characters and every control character (short
// escape where JSON has one, \u00XX otherwise).
std::string
escapeJsonString(const std::string& s);
//=============================================================================
// Result-JSON assembly helpers extracted from SimEngine::runSimulation.
//
// buildSignalBusJson returns the ",\"signalBus\":{...}" fragment describing the
// (possibly nested, offset-annotated) bus layout of every bus-typed output port,
// or an empty string when the diagram carries no bus ports. Appended to the
// result payload.
std::string
buildSignalBusJson(const std::vector<Block>& blocks, const SignalLayout& sigLayout,
    const BusRegistry& busRegistry);
//=============================================================================
// buildSignalPayloadJson assembles the per-port final-value payload of the
// result JSON: "signals", "signalDims", "signalTypes" and, when the diagram
// carries complex signals, "signalsImag" and "signalComplex". Reads the final
// output buffers (double lane, imaginary lane, exact-64 lane).
std::string
buildSignalPayloadJson(const std::vector<Block>& blocks, const SignalLayout& sigLayout,
    const std::vector<double>& outputs, const std::vector<double>& outputsImag,
    const std::vector<long long>& outputsI64);
//=============================================================================
// buildFinalResult assembles the terminal result JSON of runSimulation: on an
// unconverged algebraic loop, an "error" listing the offending loop's blocks;
// otherwise the "done" result with the signal payload + bus descriptors.
std::string
buildFinalResult(bool algLoopFailed, double algLoopTime, const std::vector<int>& algFailBlocks,
    const std::vector<int>& algIdx, const std::vector<Block>& blocks,
    const std::vector<double>& time, const std::vector<BlockState>& blockState,
    const std::vector<std::vector<int>>& inputIds, const SignalLayout& sigLayout,
    const std::vector<double>& outputs, const std::vector<double>& outputsImag,
    const std::vector<long long>& outputsI64, const BusRegistry& busRegistry);
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
