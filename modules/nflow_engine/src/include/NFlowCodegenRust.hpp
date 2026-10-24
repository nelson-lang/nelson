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
#include "NFlowCodegenHelpers.hpp" // CodegenOptions, VarMap, json
#include "NFlowCodegenC.hpp"
#include <string>
#include <vector>
#include "nlsNflow_engine_exports.h"
//=============================================================================
namespace nflow {
struct CodegenRustResult
{
    std::string rustSource; ///< Full contents of src/lib.rs
    std::string mainSource; ///< Contents of src/main.rs (empty when includeMain=false)
    std::string cargoToml; ///< Minimal Cargo.toml snippet
    // block types that fell back to the default "unknown" emission
    std::vector<std::string> unknownTypes;
    // structured findings (errors and warnings) raised during generation
    std::vector<CodegenDiagnostic> diagnostics;
};
//=============================================================================
/// Generate a Rust no_std crate from the given (already-flattened) diagram.
/// The returned crate exposes:
///   pub struct ModelState  - all continuous / discrete state
///   pub struct ModelInput  - external input labels
///   pub struct ModelOutput - external output labels (label sinks)
///   pub fn nflow_init(s: &mut ModelState)
///   pub fn nflow_step(s: &mut ModelState, inp: &ModelInput,
///                     out: &mut ModelOutput, t: f64)
NLSNFLOW_ENGINE_IMPEXP CodegenRustResult
generateRust(const json& diagram, const CodegenOptions& opts);
//=============================================================================
} // namespace nflow
//=============================================================================
