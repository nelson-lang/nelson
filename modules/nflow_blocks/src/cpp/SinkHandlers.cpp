//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "sink/sink_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    void
    registerSinkHandlers(BlockRegistry& reg)
    {
        reg.registerHandler(
            "scope", handleScope, { Phase::INIT, Phase::AFTER_STEP }, "builtin.sink");
        reg.registerHandler(
            "xyScope", handleXyScope, { Phase::INIT, Phase::AFTER_STEP }, "builtin.sink");
        reg.registerHandler(
            "xyzScope", handleXyzScope, { Phase::INIT, Phase::AFTER_STEP }, "builtin.sink");
        reg.registerHandler(nflow::kLabelSink, handleLabelSink, {}, "builtin.sink");
        reg.registerHandler("terminator", handleTerminator, {}, "builtin.sink");
        reg.registerHandler(
            "display", handleDisplay, { Phase::INIT, Phase::AFTER_STEP }, "builtin.sink");
        reg.registerHandler(
            "toWorkspace", handleToWorkspace, { Phase::INIT, Phase::AFTER_STEP }, "builtin.sink");
        reg.registerHandler(
            "stopSimulation", handleStopSimulation, { Phase::AFTER_STEP }, "builtin.sink");
        reg.registerHandler(
            "fileSink", handleFileSink, { Phase::INIT, Phase::AFTER_STEP }, "builtin.sink");

        // Code-generation templates (other blocks keep native emission paths).
        reg.registerCodegen("scope", "c", getCodeGenCScope());
        reg.registerCodegen("scope", "rust", getCodeGenRustScope());
        reg.registerCodegen("xyScope", "c", getCodeGenCXyScope());
        reg.registerCodegen("xyScope", "rust", getCodeGenRustXyScope());
        reg.registerCodegen("xyzScope", "c", getCodeGenCXyzScope());
        reg.registerCodegen("xyzScope", "rust", getCodeGenRustXyzScope());
        reg.registerCodegen("display", "c", getCodeGenCDisplay());
        reg.registerCodegen("display", "rust", getCodeGenRustDisplay());
        reg.registerCodegen("toWorkspace", "c", getCodeGenCToWorkspace());
        reg.registerCodegen("toWorkspace", "rust", getCodeGenRustToWorkspace());
        // fileSink has no template: the codegen pipeline rewrites it into an external
        // labelSink (a CSV column of the runner).

        reg.registerCodegen("labelSink", "c", getCodeGenCLabelSink());
        reg.registerCodegen("labelSink", "rust", getCodeGenRustLabelSink());
        reg.registerCodegen("terminator", "c", getCodeGenCTerminator());
        reg.registerCodegen("terminator", "rust", getCodeGenRustTerminator());
        reg.registerCodegen("stopSimulation",
            BlockRegistry::BlockCodegenPair {
                getCodeGenCStopSimulation(), getCodeGenRustStopSimulation() });
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
