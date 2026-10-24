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
// Emission bookkeeping shared by the C and Rust generators (the zero-crossing
// inventory, the conditional-lowering guard chains and their execution edges),
// so a fix lands once.
//=============================================================================
#include "nlsNflow_engine_exports.h"
#include <list>
#include <nlohmann/json.hpp>
#include <string>
#include <unordered_map>
#include <vector>
//=============================================================================
namespace nflow {
//=============================================================================
// Zero-crossing inventory of a flattened/expanded diagram for one target:
// blocks whose template declares crossingCount AND the RK4 seam, with their
// packed surface offsets in diagram order. paramsStore owns the params json
// the retained BlockCodegenArgs pointers reference.
struct CodegenCrossScan
{
    struct Entry
    {
        std::string rawId;
        std::string type;
        const nlohmann::json* block;
        int offset;
    };
    std::vector<Entry> list;
    std::list<nlohmann::json> paramsStore;
    std::vector<const nlohmann::json*> params; // parallel to list
    int total = 0;
};
//=============================================================================
NLSNFLOW_ENGINE_IMPEXP CodegenCrossScan
buildCodegenCrossScan(const std::vector<nlohmann::json>& blocks, const char* lang,
    const std::unordered_map<std::string, double>& variables);
//=============================================================================
// Sanitized gate-id chain of a conditional-subsystem member
// (params.__nfgGates); empty for an untagged block.
NLSNFLOW_ENGINE_IMPEXP std::vector<std::string>
condGatesOf(const nlohmann::json& block);
//=============================================================================
// Execution-order edges of the conditional lowering: every tagged member
// runs after its gates, and a trigger's condLatch runs after its gate. Also
// orders a data store's writes before its reads, which share no wire.
NLSNFLOW_ENGINE_IMPEXP void
appendCondExecutionEdges(
    const std::vector<nlohmann::json>& blocks, std::vector<nlohmann::json>& extraEdges);
//=============================================================================
} // namespace nflow
//=============================================================================
