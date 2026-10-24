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
#include "NFlowCodegenPipeline.hpp"
#include "nlsNflow_engine_builtin_exports.h"
//=============================================================================
namespace Nelson {
class Evaluator;
//=============================================================================
namespace NFlowGateway {
    //=============================================================================
    // Installs the NelsonWorkspaceBridge callbacks (function call, workspace
    // read/write) capturing the given Evaluator and its owning thread.
    // Idempotent; callbacks invoked from another thread marshal the work to
    // the interpreter thread through MainThreadTaskQueue and block until done.
    // Exported so the nflow GUI launcher can install the bridge too.
    NLSNFLOW_ENGINE_BUILTIN_IMPEXP void
    ensureNelsonWorkspaceBridge(Evaluator* eval);
    NLSNFLOW_ENGINE_BUILTIN_IMPEXP void
    clearNelsonWorkspaceBridge();
    //=============================================================================
    // Import an SSP archive into a runnable .nflow by invoking `NFlow.sspImport` on the
    // interpreter thread through the installed bridge. Called from the editor's RPC
    // layer on the WebUI thread, so the work is marshalled to the interpreter thread.
    // Returns the produced .nflow path, or an empty string with `errorMessage` set.
    // `ensureNelsonWorkspaceBridge` must have run.
    NLSNFLOW_ENGINE_BUILTIN_IMPEXP std::string
    nflowSspImportViaInterpreter(
        const std::string& sspPath, const std::string& destinationDir, std::string& errorMessage);
    //=============================================================================
    // Export a diagram (given as its .nflow JSON content) to an SSP at the target
    // path by invoking `NFlow.internal.sspExportContent` (which wraps NFlow.sspExportDiagram) on
    // the interpreter thread. The inverse of nflowSspImportViaInterpreter. Returns
    // the produced .ssp path, or an empty string with `errorMessage` set.
    NLSNFLOW_ENGINE_BUILTIN_IMPEXP std::string
    nflowSspExportDiagramViaInterpreter(
        const std::string& nflowContent, const std::string& sspPath, std::string& errorMessage);
    //=============================================================================
    // Export a diagram (given as its .nflow JSON content) to an FMI 3.0 CS FMU at
    // the target path by invoking `NFlow.internal.exportFmuContent` (which wraps NFlow.exportfmu)
    // on the interpreter thread. Returns the produced .fmu path, or an empty
    // string with `errorMessage` set.
    NLSNFLOW_ENGINE_BUILTIN_IMPEXP std::string
    nflowExportFmuViaInterpreter(
        const std::string& nflowContent, const std::string& fmuPath, std::string& errorMessage);
    //=============================================================================
    // Return modelicaInfo() as JSON on the interpreter thread for the GUI status.
    NLSNFLOW_ENGINE_BUILTIN_IMPEXP std::string
    nflowModelicaInfoViaInterpreter(std::string& errorMessage);
    //=============================================================================
    // Evaluate dashboard callback code on the interpreter thread. Callback
    // failures are returned to the editor and never unwind through the engine.
    NLSNFLOW_ENGINE_BUILTIN_IMPEXP bool
    nflowEvaluateCallbackViaInterpreter(
        const std::string& code, const std::string& blockId, std::string& errorMessage);
    //=============================================================================
    // Open an indexed documentation topic on the interpreter thread. The caller
    // validates the topic before invoking this bridge.
    NLSNFLOW_ENGINE_BUILTIN_IMPEXP bool
    nflowOpenHelpViaInterpreter(
        const std::string& topic, const std::string& page, std::string& errorMessage);
    //=============================================================================
    // Expand every `modelica` block in a model's JSON into an `fmu` block by invoking
    // `NFlow.internal.expandModelicaDoc` (OpenModelica compile) on the interpreter
    // thread, so the GUI Run path does not reach the engine with a raw `modelica`
    // block (unsupported_block). Returns the input unchanged, with no thread hop, when
    // the model has no modelica block. On a build failure returns an empty string with
    // `errorMessage` set (the caller reports it and blocks the run).
    // `ensureNelsonWorkspaceBridge` must have run.
    NLSNFLOW_ENGINE_BUILTIN_IMPEXP std::string
    nflowExpandModelicaViaInterpreter(const std::string& diagramJson, std::string& errorMessage);
    //=============================================================================
    // Fire a model's StopFcn once a GUI Run stops: rebuild the `out` struct and execute
    // the callback text in the base workspace by invoking
    // `NFlow.internal.runStopFcnForRun(documentJson, resultJson)` on the interpreter
    // thread. Returns false with no thread hop when the model has no `stopFcn` field.
    // A callback failure is reported through `errorMessage` and must not fail the
    // already-completed run.
    NLSNFLOW_ENGINE_BUILTIN_IMPEXP bool
    nflowRunStopFcnViaInterpreter(
        const std::string& documentJson, const std::string& resultJson, std::string& errorMessage);
    //=============================================================================
    // Lower every acausal (physical-pin) / planar component of a model's JSON into the
    // engine's native island blocks by invoking `NFlow.internal.expandAcausalDoc`
    // (NFlow.internal.acausalExpand then NFlow.internal.planarExpand) on the
    // interpreter thread, so the GUI Run path does not reach the engine with a raw
    // catalog block such as `Resistor` or `ConstantVoltage` (unsupported_block).
    // Returns the input unchanged, with no thread hop, when the model has no physical
    // wires. On a lowering failure returns an empty string with `errorMessage` set
    // (the caller reports it and blocks the run).
    NLSNFLOW_ENGINE_BUILTIN_IMPEXP std::string
    nflowExpandAcausalViaInterpreter(const std::string& diagramJson, std::string& errorMessage);
    //=============================================================================
    // Acausal code generation: replace every LINEAR single-I/O electrical / thermal
    // physicalIsland by its reduced state-space realisation by invoking
    // `NFlow.internal.reduceLinearIslands` on the interpreter thread. Reduction is
    // opportunistic: any failure (or an undeliverable interpreter call) returns the
    // input unchanged with no error. A non-reducible island is either generatable on
    // its own (embedded Newton) or rejected by the non-generatable gate downstream.
    NLSNFLOW_ENGINE_BUILTIN_IMPEXP std::string
    nflowReduceLinearIslandsViaInterpreter(
        const std::string& diagramJson, std::string& errorMessage);
    //=============================================================================
    // Interpreter-backed hooks for nflow::runCodegenPipeline. The same bundle
    // serves the nflow_codegenerate builtin (runs inline, already on the
    // interpreter thread) and the GUI RPC layer (marshalled through
    // MainThreadTaskQueue). `ensureNelsonWorkspaceBridge` must have run.
    NLSNFLOW_ENGINE_BUILTIN_IMPEXP nflow::CodegenPipelineHooks
    makeInterpreterCodegenHooks();
    //=============================================================================
} // namespace NFlowGateway
//=============================================================================
} // namespace Nelson
//=============================================================================
