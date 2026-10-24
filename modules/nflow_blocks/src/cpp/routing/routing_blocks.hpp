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
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Signal-routing blocks over vector/matrix signals: mux (concatenate to a
    // 1-D vector), demux (split), selector (index extraction), reshape,
    // concatenate (2-D aware) and convert (explicit type cast).
    //=============================================================================
    bool
    handleMux(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleDemux(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleSelector(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleReshape(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleConcatenate(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleMerge(SimCtx& ctx, const Block& b, Phase phase);
    BlockCodegenTemplate
    getCodeGenCMerge();
    BlockCodegenTemplate
    getCodeGenRustMerge();
    bool
    handleConvert(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleInitialCondition(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleWidth(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleSignalConversion(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleAssignment(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleDataStoreMemory(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleDataStoreWrite(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleDataStoreRead(SimCtx& ctx, const Block& b, Phase phase);
    // Iterator-subsystem helper blocks (see docs/iterator_subsystems.md):
    // iteratorNumber outputs the current 1-based iteration index inside a
    // For/While iterator body; iteratorCondition is a pass-through probe whose
    // input is the While loop's continue predicate (the engine reads it).
    bool
    handleIteratorNumber(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleIteratorCondition(SimCtx& ctx, const Block& b, Phase phase);
    // Bus grouping (busCreator / busSelector); the bus-aware dimension rules
    // need the registry, hence the ResolveBusDimsFn signature.
    bool
    handleBusCreator(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleBusSelector(SimCtx& ctx, const Block& b, Phase phase);
    bool
    handleBusAssignment(SimCtx& ctx, const Block& b, Phase phase);
    bool
    resolveBusAssignmentDims(const Block& b, const ValMap& vars, const std::vector<PortSig>& inSigs,
        std::vector<PortSig>& outSigs, BusRegistry& busReg, std::string& err);
    bool
    resolveBusCreatorDims(const Block& b, const ValMap& vars, const std::vector<PortSig>& inSigs,
        std::vector<PortSig>& outSigs, BusRegistry& busReg, std::string& err);
    bool
    resolveBusSelectorDims(const Block& b, const ValMap& vars, const std::vector<PortSig>& inSigs,
        std::vector<PortSig>& outSigs, BusRegistry& busReg, std::string& err);
    //=============================================================================
    // Code generation (scalar path). convert is fully generatable; the
    // vector-shape routing blocks are simulation-only pending vector codegen.
    BlockCodegenTemplate
    getCodeGenCConvert();
    BlockCodegenTemplate
    getCodeGenRustConvert();
    BlockCodegenTemplate
    getCodeGenCDataStoreMemory();
    BlockCodegenTemplate
    getCodeGenRustDataStoreMemory();
    BlockCodegenTemplate
    getCodeGenCDataStoreWrite();
    BlockCodegenTemplate
    getCodeGenRustDataStoreWrite();
    BlockCodegenTemplate
    getCodeGenCDataStoreRead();
    BlockCodegenTemplate
    getCodeGenRustDataStoreRead();
    BlockCodegenTemplate
    getCodeGenCInitialCondition();
    BlockCodegenTemplate
    getCodeGenRustInitialCondition();
    BlockCodegenTemplate
    getCodeGenCSignalConversion();
    BlockCodegenTemplate
    getCodeGenRustSignalConversion();
    //=============================================================================
    // multiportSwitch: route 1-of-N data inputs by a control index.
    bool
    handleMultiportSwitch(SimCtx& ctx, const Block& b, Phase phase);
    void
    registerMultiportSwitch(BlockRegistry& reg);
    BlockCodegenTemplate
    getCodeGenCMultiportSwitch();
    BlockCodegenTemplate
    getCodeGenRustMultiportSwitch();
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
