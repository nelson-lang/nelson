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
#include "BlockPlugin.h"
#include <string>
#include <vector>
#include <unordered_map>
#include <set>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Declarative code-generation template for one block type and one target
    // language ("c" or "rust"). The engine generators expand the placeholders:
    //   {id}              sanitized block id
    //   {inN}             input expression for port N (any N >= 0)
    //   {inNw}            parenthesized input expression for port N
    //   {param:name}      numeric parameter, generator-formatted (default 0.0)
    //   {param:name:dflt} numeric parameter with explicit default
    //   {state:name}      state field accessor (s->name_<id> in C, s.name_<id> in Rust)
    //   {dt} {t}          time step / current time
    // Multi-line `step` bodies are indented by the generator per line.
    //=============================================================================
    // Context handed to a native codegen emitter (BlockCodegenTemplate::emitStep)
    // for one block instance. Everything is pre-resolved by the generator with
    // its own language conventions (formatting, wrapping, input expressions).
    //=============================================================================
    struct BlockCodegenArgs
    {
        std::string id; // sanitized block id
        std::vector<std::string> in; // input expressions, one per port (>= 3 entries)
        std::vector<std::string> inputIds; // raw source block ids per port ("" = unconnected)
        // Signal type names resolved by the layout pass ("double", "int8",
        // "int64", "uint64", ...): the block's output port 0 and each input
        // port's source. Emitters branch on these for the exact 64-bit
        // paths; empty/absent means double.
        std::string outType;
        std::vector<std::string> inTypes;
        const json* block = nullptr; // full block JSON
        const json* params = nullptr; // block params JSON (never null when invoked)
        const std::unordered_map<std::string, double>* variables = nullptr; // diagram variables
        double dt = 0.0; // sample time
        std::function<std::string(double)> fmt; // numeric literal formatter
        std::function<std::string(const std::string&)> wrap; // parenthesize an expression
        std::function<std::string(const std::string&)> sanitize; // sanitize a block id
        std::function<void(const std::string&)> line; // emit one step line (indent added)
        // RK4 codegen. The generator sets these before invoking emitRk4 on a
        // continuous block: stateOffset is the
        // block's base index in the packed global continuous-state vector, rk4Op
        // selects the operation (see Rk4Op), and rk4Arr names the packed array the
        // op reads or writes.
        int stateOffset = 0;
        int rk4Op = 0;
        std::string rk4Arr;
        // Zero-crossing lowering: the block's base index in the
        // packed surface vector when rk4Op == Rk4Crossing.
        int crossingOffset = 0;
    };
    //=============================================================================
    using BlockCodegenEmitFn = std::function<void(const BlockCodegenArgs&)>;
    // Operation requested of a continuous block's emitRk4.
    enum Rk4Op
    {
        Rk4Gather = 1, // rk4Arr[stateOffset + i] = <block state i>
        Rk4Scatter = 2, // <block state i> = rk4Arr[stateOffset + i]
        Rk4Output = 3, // out_<id> = <output from the (scattered) state + input>
        Rk4Deriv = 4, // rk4Arr[stateOffset + i] = <derivative i from state + input>
        Rk4Clamp = 5, // clamp <block state i> to its bounds after the step (no-op if
                      // unbounded); lets a saturated integrator hold at its limit
        Rk4Crossing = 6 // rk4Arr[crossingOffset + i] = <surface i>, evaluated at the
                        // current scattered state / input expressions (emitted in the
                        // rhs tail; the stepper bisects on a sign change)
    };
    //=============================================================================
    // Context handed to a native state emitter (BlockCodegenTemplate::emitState)
    // during the generator's state-declaration pass. Raw sinks append verbatim
    // lines so the emitter fully controls the generated text.
    //=============================================================================
    struct BlockCodegenStateArgs
    {
        std::string id; // sanitized block id
        const json* block = nullptr; // full block JSON
        const json* params = nullptr; // block params JSON
        const std::unordered_map<std::string, double>* variables = nullptr; // diagram variables
        double dt = 0.0; // sample time
        std::function<std::string(double)> fmt; // numeric literal formatter
        // Scalar state field ("" type = language default double/f64).
        std::function<void(
            const std::string& name, const std::string& init, const std::string& type)>
            addState;
        std::function<void(const std::string&)> declState; // raw state struct declaration
        std::function<void(const std::string&)> addInit; // raw init-function line
        std::function<void(const std::string&)> addConst; // raw module-level const line
        std::function<void(const std::string&)> addHelper; // raw shared helper-function line
        std::function<bool(const std::string&)> once; // true the first time a key is seen
        const std::vector<json>* allBlocks = nullptr; // every block (for cross-block maxima)
    };
    //=============================================================================
    using BlockCodegenStateFn = std::function<void(const BlockCodegenStateArgs&)>;
    //=============================================================================
    struct BlockCodegenTemplate
    {
        struct StateVar
        {
            std::string name; // field prefix; "_<blockId>" is appended by the generator
            std::string init; // init expression (template, expanded)
        };
        std::vector<StateVar> state;
        std::string step; // step body template; empty = no codegen for this language
        std::string inputDefault; // unconnected-input default ("" = generator default)
        // Native emitter for blocks whose emission needs logic (loops, parameter
        // conditionals). Takes precedence over `step` when set. Built-in blocks
        // only - the plugin JSON ABI carries declarative templates.
        BlockCodegenEmitFn emitStep;
        // A continuous block that implements these joins the global rhs()+RK4
        // scheme the generator emits for solver 'ode4', instead of the per-block
        // fused forward Euler in emitStep. continuousWidth returns how many packed
        // continuous states the block contributes (0 = none / not RK4-eligible,
        // e.g. a bounded integrator whose saturation needs zero-crossing
        // handling); emitRk4 emits one Rk4Op step (gather / scatter / output /
        // derivative) using a.stateOffset and a.rk4Arr. If any stateful block
        // lacks these (or returns 0), the generator falls back to the Euler
        // emission.
        std::function<int(const BlockCodegenArgs&)> continuousWidth;
        BlockCodegenEmitFn emitRk4;
        // Pure "output from latched state" emission (no state change, no input
        // read). The generators pre-load out_<id> with it for a non-feedthrough
        // stateful block whose consumers are emitted earlier in topological
        // order (a feedback cycle broken at this block would otherwise read the
        // 0.0 declaration default). Continuous seam blocks fall back to
        // emitRk4(Rk4Output) when this is unset.
        BlockCodegenEmitFn emitOutput;
        // Exact-64 emitter: invoked INSTEAD of emitStep/step when the block's
        // output or an input carries an int64/uint64 signal (a.outType /
        // a.inTypes). Blocks without it keep the double path; the generators
        // only route exact-64 diagrams through blocks that declare one, so
        // the all-double output stays byte-identical.
        BlockCodegenEmitFn emitStepTyped;
        // Native state emitter; takes precedence over `state` when set.
        BlockCodegenStateFn emitState;
        // Native emitter for shared/grouped sections (typedefs, const tables and
        // helper functions), run in a deferred pass AFTER the state loop so their
        // byte order is grouped by type rather than interleaved by block. Blocks
        // are visited in ascending sharedPriority, then diagram order; use once()
        // for genuinely-shared lines (a typedef emitted for the whole type).
        BlockCodegenStateFn emitShared;
        int sharedPriority = 0; // deferred-pass ordering key (lower = earlier)
        // Explicit "no codegen for this language": the block declares the
        // functions (uniform authoring surface) but the generators keep their
        // native/default emission for it.
        bool notImplemented = false;

        // The emitter honors a per-block SampleTime slower than the base step
        // (it gates its state advance on the major-step index, see
        // codegenSampleRate). The multi-rate codegen gate consults this flag
        // instead of a hardcoded type list.
        bool multirateCapable = false;

        // Zero-crossing lowering: number of scalar surfaces this
        // block contributes under the RK4/DP45 seam (0 / unset = none). A
        // block with surfaces emits them via emitRk4(Rk4Crossing) into the
        // packed __g array at a.crossingOffset; the generated stepper bisects
        // to the crossing instant on a sign change (fixed RK4 trial steps,
        // mirroring the simulator's localizeRoot).
        std::function<int(const BlockCodegenArgs&)> crossingCount;

        static BlockCodegenTemplate
        unimplemented()
        {
            BlockCodegenTemplate t;
            t.notImplemented = true;
            return t;
        }
    };
    //=============================================================================
    // Central registry for block handlers.
    // Thread-safe for reads after initial registration.
    //=============================================================================
    class NLSNFLOW_BLOCKS_IMPEXP BlockRegistry
    {
    public:
        static BlockRegistry&
        instance();

        // Both language templates of one block, registered together so a
        // C/Rust asymmetry cannot slip in (the historical fileSink bug class).
        struct BlockCodegenPair
        {
            BlockCodegenTemplate c;
            BlockCodegenTemplate rust;
        };

        void
        registerHandler(
            const std::string& type, BlockHandlerFn handler, const std::set<Phase>& phases);

        // Look up a handler by type. Returns nullptr if not found.
        BlockHandlerFn
        getHandler(const std::string& type) const;

        bool
        hasPhase(const std::string& type, Phase phase) const;

        void
        unregisterLibrary(const std::string& libraryId);

        void
        registerHandler(const std::string& type, BlockHandlerFn handler,
            const std::set<Phase>& phases, const std::string& libraryId);

        // Register a handler with explicit numerical metadata (continuous/discrete
        // state counts, zero-crossing surfaces, feedthrough, sample-time class).
        void
        registerHandler(const std::string& type, BlockHandlerFn handler,
            const std::set<Phase>& phases, const std::string& libraryId, const BlockMetadata& meta);

        // Return the metadata declared for a block type. When the type is unknown
        // or registered without explicit metadata, returns defaults with
        // directFeedthrough set from ALGEBRAIC-phase membership (legacy semantics).
        BlockMetadata
        metadataOf(const std::string& type) const;

        // Patch the stored metadata of an already-registered type with its
        // dimension-propagation capabilities: whether the runtime handler is
        // width-generalized (vector-capable) and an optional custom
        // output-dimension rule. Works for entries registered with or without
        // explicit metadata.
        void
        setDimsMetadata(
            const std::string& type, bool acceptsVectorInputs, ResolveDimsFn resolveDims = nullptr);

        // Patch the stored metadata of an already-registered type with its
        // type-propagation capabilities (mixed-input tolerance, forced output
        // SigType as int, -1 = inherit, double-only inputs).
        void
        setTypeMetadata(const std::string& type, bool allowMixedInputTypes, int fixedOutSigType,
            bool requiresDoubleInputs);

        // Declare that the runtime handler of an already-registered type has
        // an exact int64/uint64 path (64-bit lane, integer-space
        // arithmetic). Without it, an exact-64 signal wired to the block is
        // a compile error.
        void
        setExact64Support(const std::string& type, bool supportsExact64 = true);

        // Declare that this direct-feedthrough block must recompute its output
        // from the current input on EVERY base step, even under a slower
        // SampleTime (only its UPDATE state advance is rate-gated). Exempts the
        // block from the sample-and-hold evalAlgebraicBlockMultirate applies to
        // an ordinary sub-rate feedthrough block, so its ALGEBRAIC output stays
        // byte-for-byte with generated code (difference, the detect* family).
        void
        setAlgebraicOutputEveryStep(const std::string& type, bool everyStep = true);

        // Declare complex-signal capabilities: whether the handler reads the
        // imaginary lane, and the output complexity override (-1 =
        // OR-propagate, 0 = always real, 1 = always complex).
        void
        setComplexSupport(
            const std::string& type, bool acceptsComplexInputs, int forcesComplexOut = -1);

        // Declare bus capabilities: whether the block accepts bus inputs and
        // an optional bus-aware dimension rule (registry access).
        void
        setBusMetadata(const std::string& type, bool acceptsBusInputs,
            ResolveBusDimsFn resolveBusDims = nullptr);

        // Scan a library directory for library.json and load native plugin if declared.
        bool
        scanDirectory(const std::string& dirPath);

        int
        scanSearchPaths(const std::vector<std::string>& paths);

        // Replace the ordered search paths and rebuild external discoveries.
        int
        setSearchPaths(const std::vector<std::string>& paths);

        std::vector<std::string>
        searchPaths() const;

        bool
        loadNativeLibrary(const std::string& path, const std::string& libraryId);

        std::vector<std::string>
        registeredTypes() const;

        std::vector<std::string>
        discoveredManifests() const;

        std::vector<std::string>
        discoveryMessages() const;

        void
        registerCodegen(
            const std::string& type, const std::string& lang, const BlockCodegenTemplate& tmpl);

        // Register both language templates of a block in one call (see
        // BlockCodegenPair). Preferred for new blocks: one line, no asymmetry.
        void
        registerCodegen(const std::string& type, const BlockCodegenPair& pair);

        // Register a plugin's programmatic emitter for (type, lang). Builds a
        // BlockCodegenTemplate whose emitStep/emitState bridge the internal
        // BlockCodegenArgs to a pure-C NfCodegenContext and call fn. Used by the
        // plugin loader for nfEmitBlockCodegen; takes precedence over any JSON
        // template registered for the same (type, lang).
        void
        registerCodegenEmitter(
            const std::string& type, const std::string& lang, NfEmitBlockCodegenFn fn);

        // Debug aid: block types registered for exactly ONE language (a pair
        // asymmetry). Empty when every type carries both templates.
        std::vector<std::string>
        codegenLanguageAsymmetries() const;

        // Look up a code-generation template. Returns false if none registered.
        bool
        getCodegen(
            const std::string& type, const std::string& lang, BlockCodegenTemplate& out) const;

        void
        clear();

    private:
        BlockRegistry();
        ~BlockRegistry();
        BlockRegistry(const BlockRegistry&) = delete;
        BlockRegistry&
        operator=(const BlockRegistry&)
            = delete;

        struct Impl;
        Impl* impl_;
    };
    //=============================================================================
    // Built-in handler registration functions (called at startup).
    // Each is defined in the corresponding *Handlers.cpp file.
    //=============================================================================
    NLSNFLOW_BLOCKS_IMPEXP void
    registerSourceHandlers(BlockRegistry& reg);
    NLSNFLOW_BLOCKS_IMPEXP void
    registerMathHandlers(BlockRegistry& reg);
    NLSNFLOW_BLOCKS_IMPEXP void
    registerLookupHandlers(BlockRegistry& reg);
    NLSNFLOW_BLOCKS_IMPEXP void
    registerNonlinearHandlers(BlockRegistry& reg);
    NLSNFLOW_BLOCKS_IMPEXP void
    registerLogicHandlers(BlockRegistry& reg);
    NLSNFLOW_BLOCKS_IMPEXP void
    registerContinuousHandlers(BlockRegistry& reg);
    NLSNFLOW_BLOCKS_IMPEXP void
    registerDiscreteHandlers(BlockRegistry& reg);
    NLSNFLOW_BLOCKS_IMPEXP void
    registerSinkHandlers(BlockRegistry& reg);
    void
    registerDashboardHandlers(BlockRegistry& reg);
    NLSNFLOW_BLOCKS_IMPEXP void
    registerSubsystemHandlers(BlockRegistry& reg);
    NLSNFLOW_BLOCKS_IMPEXP void
    registerCondLoweringHandlers(BlockRegistry& reg);
    NLSNFLOW_BLOCKS_IMPEXP void
    registerFmuBlock(BlockRegistry& reg);
    NLSNFLOW_BLOCKS_IMPEXP void
    registerFmuMeBlock(BlockRegistry& reg);
    NLSNFLOW_BLOCKS_IMPEXP void
    registerRoutingHandlers(BlockRegistry& reg);
    NLSNFLOW_BLOCKS_IMPEXP void
    registerMatrixHandlers(BlockRegistry& reg);
    NLSNFLOW_BLOCKS_IMPEXP void
    registerComplexHandlers(BlockRegistry& reg);
    NLSNFLOW_BLOCKS_IMPEXP void
    registerUserDefinedHandlers(BlockRegistry& reg);
    // Patches the dimension-propagation capabilities (vector support, custom
    // dims rules) onto the already-registered built-ins.
    NLSNFLOW_BLOCKS_IMPEXP void
    registerVectorCapabilities(BlockRegistry& reg);
    NLSNFLOW_BLOCKS_IMPEXP void
    registerAllBuiltinHandlers();
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
