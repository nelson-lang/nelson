//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Data Store Memory / Read / Write: a named scalar memory shared between blocks
// of a model without a wire (Data Store Memory declares it with an initial
// value; Data Store Write updates it; Data Store Read outputs it). The store is
// a per-thread map keyed by DataStoreName, so it persists across steps within a
// run and is re-seeded each run by the Memory block's INIT. Read observes the
// value as of the previous step's Write (Read runs in OUTPUT, Write in UPDATE),
// i.e. one-step latency, matching a unit delay. Scalar only; native (the
// cross-block store is outside the code-generation model).
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "NFlowCodegenLang.hpp"
#include <cctype>
#include <string>
#include <unordered_map>
#include "routing_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Per-thread named store. A sim run is synchronous on one thread, so this
    // persists across that run's steps; concurrent runs on other threads get
    // their own map. Each entry is (re)seeded by the owning Memory block's INIT.
    static std::unordered_map<std::string, double>&
    dataStoreMap()
    {
        static thread_local std::unordered_map<std::string, double> store;
        return store;
    }
    //=============================================================================
    static std::string
    dataStoreName(const Block& b, const SimCtx& ctx)
    {
        nflow::BlockDescriptor bd(b, ctx.variables);
        return bd.paramStr("DataStoreName", "A");
    }
    //=============================================================================
    bool
    handleDataStoreMemory(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::INIT) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const std::string name = bd.paramStr("DataStoreName", "A");
        dataStoreMap()[name] = bd.paramDouble("InitialValue", 0.0);
        return false;
    }
    //=============================================================================
    bool
    handleDataStoreWrite(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase == Phase::INIT) {
            // Seed the entry if no Memory block declared it, so a lone
            // Write/Read pair still starts from a defined value.
            const std::string name = dataStoreName(b, ctx);
            dataStoreMap().emplace(name, 0.0);
            return false;
        }
        // The write lands in the topologically ordered phase, not at the end
        // of the step: a read placed after it then sees the value written this
        // step, which is what an execution order means.
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        if (!hasInput(ctx, b.nid, 0)) {
            return false;
        }
        dataStoreMap()[dataStoreName(b, ctx)] = getInput(ctx, b.nid, 0, 0.0);
        return false;
    }
    //=============================================================================
    bool
    handleDataStoreRead(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        const std::string name = dataStoreName(b, ctx);
        const auto& store = dataStoreMap();
        auto it = store.find(name);
        setOutput(ctx, b.nid, (it != store.end()) ? it->second : 0.0);
        return false;
    }
    //=============================================================================
    // The named store becomes ONE shared ModelState field ds_<name> per DataStoreName.
    // A Read has no wired input, so the topological sort emits it in the first wave,
    // before any Write. That gives the simulator's one-step latency without synthetic
    // edges. The field is declared once (seeded 0.0); a Memory block appends its
    // InitialValue init after the seed, so it wins whatever the block order.
    //=============================================================================
    static std::string
    dataStoreField(const BlockCodegenStateArgs& a)
    {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const std::string name = bd.paramStr("DataStoreName", "A");
        std::string field = "ds_";
        for (char c : name) {
            field += (std::isalnum(static_cast<unsigned char>(c)) != 0) ? c : '_';
        }
        return field;
    }
    //=============================================================================
    static std::string
    dataStoreFieldStep(const BlockCodegenArgs& a)
    {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        const std::string name = bd.paramStr("DataStoreName", "A");
        std::string field = "ds_";
        for (char c : name) {
            field += (std::isalnum(static_cast<unsigned char>(c)) != 0) ? c : '_';
        }
        return field;
    }
    //=============================================================================
    static void
    ensureDataStoreField(const BlockCodegenStateArgs& a, CodegenLang L)
    {
        const std::string field = dataStoreField(a);
        if (!a.once("dsdecl:" + field)) {
            return;
        }
        if (L.rust) {
            a.declState("    pub " + field + ": f64,");
            a.addInit("    s." + field + " = 0.0;");
        } else {
            a.declState("double " + field + ";");
            a.addInit("  s->" + field + " = 0.0;");
        }
    }
    //=============================================================================
    static BlockCodegenTemplate
    makeDataStoreMemoryCodegen(CodegenLang L)
    {
        BlockCodegenTemplate t;
        t.emitState = [L](const BlockCodegenStateArgs& a) {
            ensureDataStoreField(a, L);
            nflow::BlockDescriptor bd(*a.block, *a.variables);
            const std::string field = dataStoreField(a);
            const std::string init = a.fmt(bd.paramDouble("InitialValue", 0.0));
            if (L.rust) {
                a.addInit("    s." + field + " = " + init + ";");
            } else {
                a.addInit("  s->" + field + " = " + init + ";");
            }
        };
        t.emitStep = [L](const BlockCodegenArgs& a) {
            a.line("out_" + a.id + " = " + L.sref(dataStoreFieldStep(a)) + ";");
        };
        return t;
    }
    //=============================================================================
    static BlockCodegenTemplate
    makeDataStoreWriteCodegen(CodegenLang L)
    {
        BlockCodegenTemplate t;
        t.emitState = [L](const BlockCodegenStateArgs& a) { ensureDataStoreField(a, L); };
        t.emitStep = [L](const BlockCodegenArgs& a) {
            a.line("out_" + a.id + " = " + a.in[0] + ";");
            a.line(L.sref(dataStoreFieldStep(a)) + " = out_" + a.id + ";");
        };
        return t;
    }
    //=============================================================================
    static BlockCodegenTemplate
    makeDataStoreReadCodegen(CodegenLang L)
    {
        BlockCodegenTemplate t;
        t.emitState = [L](const BlockCodegenStateArgs& a) { ensureDataStoreField(a, L); };
        t.emitStep = [L](const BlockCodegenArgs& a) {
            a.line("out_" + a.id + " = " + L.sref(dataStoreFieldStep(a)) + ";");
        };
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCDataStoreMemory()
    {
        return makeDataStoreMemoryCodegen({ false });
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustDataStoreMemory()
    {
        return makeDataStoreMemoryCodegen({ true });
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCDataStoreWrite()
    {
        return makeDataStoreWriteCodegen({ false });
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustDataStoreWrite()
    {
        return makeDataStoreWriteCodegen({ true });
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCDataStoreRead()
    {
        return makeDataStoreReadCodegen({ false });
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustDataStoreRead()
    {
        return makeDataStoreReadCodegen({ true });
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
