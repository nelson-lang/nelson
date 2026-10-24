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
#include <functional>
#include <string>
#include <vector>
#include "NFlowCodegenC.hpp"
#include "NFlowCodegenRust.hpp"
#include "nlsNflow_engine_exports.h"
//=============================================================================
namespace nflow {
// ---------------------------------------------------------------------------
// Shared code-generation pipeline.
//
// Every code-generation entry point (the nflow_codegenerate builtin, the FMU
// exporter and the GUI RPC/preview paths) runs the same gate sequence:
// workspace baking -> modelica/acausal expansion -> linear-island reduction ->
// parse -> model sampleTime -> solver gate -> conditional gate -> flatten ->
// non-generatable gate -> multi-rate gate -> complex lowering -> vector
// expansion -> generateC/generateRust -> unknownTypes/diagnostics check.
// The pipeline owns the decisions; callers own the wording (the builtin keeps its
// translated segmented messages, composeCodegenMessageEnglish gives the English
// text for the RPC/preview envelopes).
// ---------------------------------------------------------------------------
// Interpreter-backed stages, injected because the pipeline itself must stay
// callable from threads without an evaluator (GUI webview). An EMPTY hook is
// legal: when the model does not need the stage it is skipped; when it does
// (substring fast-path detection), the pipeline emits a typed error instead
// of silently skipping the pass.
struct CodegenPipelineHooks
{
    // Bake fromWorkspace VariableName data into the diagram text (in place).
    // Returns false + err only on a hard failure; an unresolvable variable
    // leaves the diagram unbaked (the block then emits its 0.0 marker).
    std::function<bool(std::string& diagramJson, std::string& err)> resolveWorkspaceInputs;
    // Expand modelica blocks via the interpreter (FMU compilation path).
    std::function<std::string(const std::string& diagramJson, std::string& err)> expandModelica;
    // Expand acausal physicalWires documents into physicalIsland blocks.
    std::function<std::string(const std::string& diagramJson, std::string& err)> expandAcausal;
    // Linear single-I/O islands -> stateSpace (NFlow.internal.reduceLinearIslands);
    // on failure returns the input unchanged (the non-generatable gate rejects).
    std::function<std::string(const std::string& diagramJson, std::string& err)>
        reduceLinearIslands;
};
//=============================================================================
struct CodegenPipelineRequest
{
    std::string diagramJson; // raw .nflow text (hooks operate on text)
    CodegenOptions options; // target "C"|"rust", includeMain, sampleTime, build
    bool useModelSampleTime = true; // diagram sampleTime overrides options
    bool failOnWarning = false; // promote warnings to errors
};
//=============================================================================
// Diagnostic gates emitted by the pipeline (besides the generator-produced
// gates algebraicLoop / typedPort / expressionRejected / solverDowngrade):
//   "modelica"        blockType="modelica"
//   "workspace"       message = bridge failure detail
//   "expandModelica"  message = expansion failure detail
//   "expandAcausal"   message = expansion failure detail
//   "hookMissing"     detail = stage name ("modelica"|"acausal"), model needs
//                     an interpreter pass this caller cannot run
//   "parse"           message = parse failure detail
//   "solver"          detail = rejected solver name
//   "conditional"     blockId + blockType (construct name)
//   "flatten"         message = non-convergence detail
//   "nonGeneratable"  blockId + blockType (fmu/fmuMe/nelsonFunction/...)
//   "multirate"       blockId
//   "complexLower"    message (+ blockId when known)
//   "vectorExpand"    message (+ blockId when known)
//   "unknownTypes"    detail = comma-joined type list
struct CodegenPipelineResult
{
    bool ok = false;
    CodegenCResult c; // filled when options.target == "C"
    CodegenRustResult rust; // filled when options.target == "rust"
    std::vector<CodegenDiagnostic> diagnostics; // gates + generator findings
    bool
    hasError() const
    {
        for (const auto& d : diagnostics) {
            if (d.severity == CodegenDiagnostic::Severity::Error) {
                return true;
            }
        }
        return false;
    }
    const CodegenDiagnostic*
    firstError() const
    {
        for (const auto& d : diagnostics) {
            if (d.severity == CodegenDiagnostic::Severity::Error) {
                return &d;
            }
        }
        return nullptr;
    }
};
//=============================================================================
NLSNFLOW_ENGINE_IMPEXP CodegenPipelineResult
runCodegenPipeline(const CodegenPipelineRequest& request, const CodegenPipelineHooks& hooks);
// Canonical English text for a pipeline/generator diagnostic. Byte-equal to the
// nflow_codegenerate builtin's English output for the same condition (the builtin
// composes its own translated segments; this is for the RPC/preview envelopes and
// logs).
NLSNFLOW_ENGINE_IMPEXP std::string
composeCodegenMessageEnglish(const CodegenDiagnostic& d);
// True when the generators lower this block type through a NATIVE emitter
// branch or a pre-generation pass (subsystem flattening, complex lowering,
// vector expansion, dashboard plan) rather than a registered template.
// Complements BlockRegistry::getCodegen for support reporting
// (__nflow_codegen_support__ and the help-page stamper).
NLSNFLOW_ENGINE_IMPEXP bool
hasNativeCodegenPath(const std::string& type);
//=============================================================================
} // namespace nflow
//=============================================================================
