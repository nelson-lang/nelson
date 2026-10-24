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
#include <string>
#include <utility>
#include <vector>
#include <nlohmann/json.hpp>
#include "nlsNflow_engine_exports.h"
//=============================================================================
namespace nflow {
using json = nlohmann::json;
struct CodegenOptions
{
    double sampleTime = 0.01;
    bool includeMain = true;
    bool build = false;
    std::string target = "C";
};
//=============================================================================
// One structured finding raised while generating code. Errors mean the emitted
// source is NOT faithful to the simulation (callers must not ship it);
// warnings mean generation succeeded with a documented deviation the user
// should know about (e.g. a requested RK4 solver demoted to fused Euler).
struct CodegenDiagnostic
{
    enum class Severity
    {
        Warning,
        Error
    };
    Severity severity = Severity::Error;
    std::string gate; // stable id: "algebraicLoop", "typedPort", "solverDowngrade", ...
    std::string blockId; // offending block, empty for a model-level finding
    std::string blockType;
    std::string message; // user-facing text (English; callers translate/wrap)
    std::string detail; // gate-specific parameter (solver name, type list, ...)
};
//=============================================================================
struct CodegenCResult
{
    std::string modelSource; // full model C source (no runner)
    std::string mainSource; // optional runner (main.c)
    std::string cmake; // CMakeLists.txt content
    // block types that fell back to the default "unknown" emission
    std::vector<std::string> unknownTypes;
    // structured findings (errors and warnings) raised during generation
    std::vector<CodegenDiagnostic> diagnostics;
    // External interface, in generation order, as (field name, FMI type name):
    // the exact ModelInput / ModelOutput field names and their FMI type
    // ("Float64" | "Int64" | "UInt64", or "" for a bus/struct field that has no
    // scalar FMI type). Consumed by the FMU exporter to map value references.
    std::vector<std::pair<std::string, std::string>> fmuInputs;
    std::vector<std::pair<std::string, std::string>> fmuOutputs;
    std::string
    combined() const
    {
        if (mainSource.empty()) {
            return modelSource;
        }
        return modelSource + "\n\n/* --- main.c --- */\n\n" + mainSource;
    }
};
//=============================================================================
NLSNFLOW_ENGINE_IMPEXP CodegenCResult
generateC(const json& diagram, const CodegenOptions& opts = {});
// Shared by the C and Rust generators: the error for an unresolved cycle in the
// execution order (a genuine algebraic loop). The simulator iterates such loops
// to a fixed point (SimEngineAlgebraic); straight-line generated code cannot, so
// generation fails instead of shipping numerically wrong code.
inline CodegenDiagnostic
makeAlgebraicLoopDiagnostic(const std::vector<std::string>& cycleIds)
{
    CodegenDiagnostic d;
    d.severity = CodegenDiagnostic::Severity::Error;
    d.gate = "algebraicLoop";
    d.blockId = cycleIds.empty() ? std::string {} : cycleIds.front();
    std::string list;
    size_t shown = cycleIds.size() < 5 ? cycleIds.size() : 5;
    for (size_t i = 0; i < shown; ++i) {
        if (!list.empty()) {
            list += "', '";
        }
        list += cycleIds[i];
    }
    d.message = "cannot solve the algebraic loop through or fed by blocks '" + list + "'"
        + (cycleIds.size() > shown ? ", ..." : "")
        + ": the simulator iterates it to a fixed point but generated code is straight-line. "
          "Break the loop with a unitDelay or memory block, or simulate this model instead.";
    return d;
}
//=============================================================================
} // namespace nflow
//=============================================================================
