//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <nlohmann/json.hpp>
#include <string>
#include <unordered_map>
#include <vector>
#include "nlsNflow_engine_exports.h"
//=============================================================================
namespace nflow {
using json = nlohmann::json;
class NLSNFLOW_ENGINE_IMPEXP DiagramFlattener
{
public:
    json
    flatten(const json& diagram);
    // Fail-closed variant: when the pass guard runs out before every subsystem is
    // flattened (more than 1000 subsystem instances), `error` receives a message and
    // the caller must reject the result. The returned diagram is only partly flattened.
    json
    flatten(const json& diagram, std::string* error);

private:
    static std::string
    connKey(const json& conn);
    static json
    normalizeConnection(const json& conn);
    json
    flattenOneSubsystem(const json& diagram, const json& subsystemBlock);
    json
    flattenMuxDemux(const json& diagram);
    // Rewrites a for-each subsystem (params.forEach) into an ordinary subsystem
    // whose inner diagram unrolls the body N times. A per-iteration `selector` takes
    // the i-th width-w slice of each partitioned input (width from inWidths), an
    // unpartitioned input is broadcast, and a `mux` concatenates the outputs.
    // Each unrolled copy carries its own state.
    static json
    expandForEachSubsystem(const json& subsystemBlock,
        const std::unordered_map<std::string, std::vector<int>>& inWidths);
};
//=============================================================================
} // namespace nflow
//=============================================================================
