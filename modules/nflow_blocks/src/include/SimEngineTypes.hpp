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
#define _CRT_SECURE_NO_WARNINGS
#define _USE_MATH_DEFINES
//=============================================================================
#include <nlohmann/json.hpp>
#include <cmath>
#include <algorithm>
#include <vector>
#include <string>
#include <map>
#include <unordered_map>
#include <functional>
#include <limits>
#include <cstdio>
#include <stdexcept>
#include <memory>
#include <set>
#include <complex>
#include "nlsNflow_blocks_exports.h"
#include "SimDiagnostic.hpp"
//=============================================================================
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#ifndef M_E
#define M_E 2.71828182845904523536
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    using json = nlohmann::json;
    using ValMap = std::unordered_map<std::string, double>;
    //=============================================================================
    // Numeric block ID: index into the blocks/blockState/outputBase vectors.
    // INVALID_ID means "not yet assigned" or "not found".
    using BlockId = int;
    static constexpr BlockId INVALID_ID = -1;
    //=============================================================================
    // Signal data type carried by a port. Storage is double-backed; the tag drives
    // rounding, saturation, comparison and display. Ids are stable: append only.
    enum class SigType : int
    {
        Double = 0,
        Single = 1,
        Int8 = 2,
        UInt8 = 3,
        Int16 = 4,
        UInt16 = 5,
        Int32 = 6,
        UInt32 = 7,
        Boolean = 8,
        // Exact 64-bit integers live in the 64-bit lane (SimCtx::outputsI64); the
        // double lane holds a lossy projection for display. UInt64 is stored as its
        // raw bit pattern in the signed lane.
        Int64 = 9,
        UInt64 = 10
    };
    //=============================================================================
    // Defined in SimEngineBusTypes.hpp.
    class BusRegistry;
    //=============================================================================
    static constexpr int kMaxSignalDims = 32;
    //=============================================================================
    // Per-output-port signal descriptor, resolved by buildSignalLayout and indexed by
    // the global port index gpi = portBase[nid] + port.
    //
    // A port of width w owns w contiguous slots in the flat outputs buffer starting
    // at `offset` (column-major, first dimension fastest). `dims` is the shape: never
    // empty, every entry >= 1, at most kMaxSignalDims entries:
    //   dims.size() == 1 -> unoriented vector [n]
    //   dims.size() == 2 -> oriented matrix [rows x cols]
    //   dims.size() >= 3 -> N-D array [d1 x d2 x ...]
    // Shapes are frozen for the whole run. Trailing singleton dimensions beyond the
    // second are dropped ([2x3x1] == [2x3]) so shape equality is canonical. [3]
    // (unoriented) and [3x1] (oriented column) stay distinct.
    struct PortSig
    {
        int offset = -1; // first slot in the outputs buffer
        std::vector<int> dims { 1 }; // authoritative shape (see above)
        SigType type = SigType::Double;
        // Complexity is independent of the numeric type. A complex port owns the same
        // [offset, offset+width) span in the imaginary lane (SimCtx::outputsImag).
        bool isComplex = false;
        // -1 = scalar/array leaf; >= 0 = the port carries bus #busTypeId. A bus port's
        // packed footprint is dims = {packedWidth}; the member layout lives in the registry.
        int busTypeId = -1;
        int
        rows() const
        {
            return dims.empty() ? 1 : dims[0];
        }
        int
        cols() const
        {
            return dims.size() > 1 ? dims[1] : 1;
        }
        int
        numDims() const
        {
            return dims.empty() ? 1 : (int)dims.size();
        }
        int
        width() const
        {
            int w = 1;
            for (int d : dims) {
                w *= (d > 0) ? d : 1;
            }
            return w;
        }
        void
        setScalar()
        {
            dims.assign(1, 1);
        }
        void
        setVector(int n)
        {
            dims.assign(1, (n > 0) ? n : 1);
        }
        void
        setShape2D(int r, int c)
        {
            dims = { (r > 0) ? r : 1, (c > 0) ? c : 1 };
        }
        // Entries are clamped to >= 1 and trailing singleton dimensions beyond the
        // second are trimmed. Callers check the kMaxSignalDims cap.
        void
        setDims(const std::vector<int>& d)
        {
            if (d.empty()) {
                setScalar();
                return;
            }
            dims.resize(d.size());
            for (size_t i = 0; i < d.size(); ++i) {
                dims[i] = (d[i] > 0) ? d[i] : 1;
            }
            while (dims.size() > 2 && dims.back() == 1) {
                dims.pop_back();
            }
        }
        void
        copyShape(const PortSig& other)
        {
            dims = other.dims;
        }
    };
    //=============================================================================
    struct Block
    {
        std::string id; // user-facing (JSON / UI)
        BlockId nid = INVALID_ID; // runtime numeric index (assigned at compile phase)
        std::string type;
        int inputs = 0;
        int outputs = 1;
        json params; // raw (already pre-resolved where possible)
        // Resolved port widths (filled by buildSignalLayout; empty = all 1). Used by
        // parameter-dependent state-count callbacks (StateLayout).
        std::vector<int> inWidths;
        std::vector<int> outWidths;
        int
        inWidth(int p) const
        {
            return (p >= 0 && p < (int)inWidths.size() && inWidths[p] > 0) ? inWidths[p] : 1;
        }
        int
        outWidth(int p) const
        {
            return (p >= 0 && p < (int)outWidths.size() && outWidths[p] > 0) ? outWidths[p] : 1;
        }
    };
    //=============================================================================
    struct Connection
    {
        std::string from;
        std::string to;
        int fromIndex = 0;
        int toIndex = 0;
    };
    //=============================================================================
    // External per-block runtime resource (e.g. an imported FMU instance). Owned by
    // BlockState; the concrete type lives in the owning module, so this header has no
    // third-party dependency. Destroyed with BlockState, no teardown phase needed.
    struct IBlockResource
    {
        virtual ~IBlockResource() = default;
    };
    //=============================================================================
    struct BlockState
    {
        // integrator / rate / pid
        double scalar = 0.0;
        double scalar2 = 0.0; // e.g. hpf raw state
        double output = 0.0; // latched output for output-->update blocks
        unsigned int rngState = 1u;

        // Integrator with external reset: the control latched at the previous major
        // step, and whether a latch happened yet (no edge fires before the first latch,
        // so a line already high at t0 is a level, not an edge).
        double resetPrev = 0.0;
        bool resetPrimed = false;
        // Level at the major step this integration covers. Every stage of one step
        // must see the same level.
        bool resetHeld = false;
        // Whether a reset is in effect for the step named by resetStamp. Decided in
        // ALGEBRAIC, where the reset input is settled: OUTPUT walks blocks in diagram
        // order, so a reset source placed after the integrator has not run yet and
        // would give a stale line and an invented edge. OUTPUT applies the stamped
        // decision.
        bool resetActive = false;
        long long resetStamp = -1;

        // derivative: whether the previous-sample input has been seeded. Before that
        // the output is zero; assuming a previous input of 0 produced a u(0)/dt spike.
        bool derivPrimed = false;

        // Multi-rate feedthrough sample-and-hold: a sub-rate feedthrough block latches
        // its port-0 output at each rate hit and replays it between hits.
        std::vector<double> heldOutput;
        bool heldValid = false;

        // transfer function (continuous)
        std::vector<double> tfState;
        std::vector<double> tfNum;
        std::vector<double> tfDen;
        double tfD = 0.0;

        // delay (circular buffer)
        std::vector<double> delayBuffer;
        int delayIndex = 0;
        double delaySamples = 0.0;

        // discrete ZOH/FOH/dTF/dDelay/dStateSpace
        double dScalar = 0.0;
        double dOutput = 0.0;
        double dNextTime = 0.0;
        double dLastTime = 0.0;
        std::vector<double> xHist;
        std::vector<double> yHist;
        double dLastOut = 0.0;
        double dTs = 0.1;
        std::vector<double> queue;

        std::vector<std::vector<double>> scopeSeries;
        // Typed logging (toWorkspace): imaginary parts of a complex input and exact
        // 64-bit samples of an int64/uint64 input. scopeExactType latches the SigType
        // id (0 = none).
        std::vector<std::vector<double>> scopeSeriesImag;
        std::vector<std::vector<long long>> scopeSeriesI64;
        int scopeExactType = 0;
        // Imaginary parts of a complex fromWorkspace source (parallel to
        // fsrcValues).
        std::vector<double> fsrcValuesImag;
        std::vector<double> xSeries;
        std::vector<double> ySeries;
        std::vector<double> zSeries;

        // file source
        std::vector<double> fsrcTimes;
        std::vector<double> fsrcValues;
        int fsrcIdx = 0;

        double pidIntegral = 0.0;
        double pidPrev = 0.0;

        struct SubsystemState
        {
            std::vector<Block> blocks; // inner blocks (nids assigned)
            // Numeric fast-path graph:
            std::vector<std::vector<int>>
                inputIds; // [nid][portIdx] = source output slot (-1 unconnected)
            std::vector<int> outputBase; // [nid] = first output slot index
            std::unordered_map<std::string, BlockId> nameToId; // blockId --> nid
            std::vector<BlockState> blockStateVec; // [nid]
            mutable std::vector<double> outputsVec; // [output slot]
            // String maps (small, for label / external-port routing only):
            std::unordered_map<std::string, std::string> labelSinks; // name --> sinkBlockId
            std::unordered_map<std::string, int> inputLookup; // blockId --> outerInputIndex
            std::unordered_map<std::string, int> outputLookup; // blockId --> outerOutputIndex
            std::unordered_map<std::string, std::string> labelSourceBlocks;
            std::vector<double> lastValues;
            // Scheduling:
            std::vector<int> initIdx, outputIdx, algIdx, algOrdered, afterStepIdx, updateIdx;
            bool algHasCycle = false;
            bool hasLabelBlocks = false;
            // Inner signal layout. When populated, the inner SimCtx carries it so
            // vector blocks work and the external boundary marshals full slices.
            std::vector<PortSig> sigs; // [gpi]
            std::vector<int> portBase; // [nid] -> first gpi
            std::vector<std::vector<int>> inputSigs; // [nid][port] -> src gpi
            std::vector<int> extOutWidth; // [outerOutputIndex] -> width
            std::vector<int> extOutOffset; // [outerOutputIndex] -> lastValues offset
            std::vector<int> extInWidth; // [outerInputIndex] -> width
            std::vector<int> extInOffset; // [outerInputIndex] -> outerInputs offset
            // Sibling lanes of outputsVec (same slot indexing), allocated only when
            // the inner layout has complex / exact-64 ports.
            mutable std::vector<double> outputsImagVec;
            mutable std::vector<long long> outputsI64Vec;
            // Descriptor (shape, type, complexity) of the signal feeding each inner
            // output sink.
            std::vector<PortSig> extOutSig;
            // Sibling lanes of lastValues, sized only when the inner lanes are active.
            std::vector<double> lastValuesImag;
            std::vector<long long> lastValuesI64;
            // Conditional execution. ctrlKind: 0 = none (always active), 1 = enable
            // (active while control > 0), 2 = trigger (runs once per control edge),
            // 3 = enableTrigger (trigger edge honored only while enable > 0).
            // ctrlPortIdx = outer input port carrying the control signal, appended after
            // the data ports; ctrlPortIdx2 = trigger port for enableTrigger (enable at
            // ctrlPortIdx, trigger at ctrlPortIdx + 1). When inactive, external outputs
            // hold their last value and internal state is frozen.
            int ctrlKind = 0;
            int ctrlPortIdx = -1;
            int ctrlPortIdx2 = -1;
            // Major step (SimCtx::stepIndex) at which this subsystem last executed. A
            // Merge block reads it on its sources to pick the branch that ran this step.
            long long lastActiveStamp = -1;
            // Trigger / enableTrigger: edge type (0 = rising, 1 = falling, 2 = either)
            // and the control value latched at the previous major step (UPDATE).
            int triggerType = 0;
            double prevCtrl = 0.0;
            bool havePrevCtrl = false;
            // enable / action: level gate for the step under way, latched at the major
            // step that opened it. One step runs under one gate, stages included, and the
            // sample that ends a step answers under the gate that governed it. Refreshed
            // once, at the end of UPDATE.
            bool levelGate = false;
            bool levelGateLatched = false;
            // Set by SimCtx::invokeSubsystem while a functionCall callee (ctrlKind 6) is
            // driven, so handleSubsystem runs its ALGEBRAIC / UPDATE although the callee
            // is outside the normal schedule and its gate is closed.
            bool functionCallInvoking = false;
            // For-each. feNumIter > 1 runs the body feNumIter times per step: a
            // partitioned input of outer width feNumIter * w_in feeds iteration i its
            // i-th w_in slice, an unpartitioned input is broadcast whole, and each inner
            // output of width w_out is concatenated into an outer output of width
            // feNumIter * w_out (feConcat, laid out per outer output port). The inner
            // state (ss.blockStateVec etc.) is built at the per-iteration width.
            int feNumIter = 1;
            std::vector<char> feIsPartition; // [outerInputIndex] 1 = sliced input
            std::vector<double> feConcat; // concatenated outer outputs
            // For-each with discrete state: iterations 1..feNumIter-1 each get an
            // independent inner-state replica (iteration 0 uses this SubsystemState), so
            // each channel keeps its own history. Empty for a stateless body. Continuous
            // state inside a for-each is rejected at compile time until StateLayout is
            // extended.
            std::vector<std::unique_ptr<SubsystemState>> feReplicas;
            // For-each body with continuous state: each iteration's inner blocks address
            // their own x/xdot slice (StateLayout lays out N * nx_inner) and the
            // DERIVATIVE / ZERO_CROSSING phase runs once per iteration.
            bool feHasContinuous = false;
            // Major step at which a solver-located control edge fired this subsystem
            // through Phase::EVENT. The trigger gate uses it to suppress the duplicate
            // fire the same step's ALGEBRAIC pass would report against the stale prevCtrl.
            long long lastEventStamp = -1;
            // Iterator subsystems. ctrlKind 7 = forIterator, 8 = whileIterator: the body
            // runs several times sequentially within one major step (for-each runs in
            // parallel over input partitions). Inner state is reset to its initial
            // condition at each step entry, then accumulates across the iterations of
            // that step.
            //   iterSource:       0 = count from IterationLimit, 1 = from an input port
            //                     (forIterator external source).
            //   iterCount:        fixed iteration count (forIterator internal).
            //   iterCountPortIdx: input port carrying the external count (>= 0).
            //   maxIterations:    safety cap for whileIterator (mandatory).
            //   iterCondNid:      inner nid of the iteratorCondition block whose value
            //                     is the while loop's continue predicate (-1 if absent:
            //                     the while runs the full cap).
            //   iterIndex/iterCount reach inner blocks through the inner SimCtx
            //   (iteratorNumber reads them). iterStamp makes the loop run once per major
            //   step; lastIterationsRun records how many iterations the last entry ran.
            int iterSource = 0;
            // Whether the body's state is reset to its initial condition at each step
            // entry (default) or carried across steps like other discrete state.
            bool iterResetStates = true;
            int iterCount = 0;
            int iterCountPortIdx = -1;
            int maxIterations = 100;
            int iterCondNid = -1;
            int iterIndex = 0;
            long long iterStamp = -1;
            int lastIterationsRun = 0;
        };
        std::unique_ptr<SubsystemState> subsystem;

        std::unique_ptr<IBlockResource> external;

        // Global-state-vector slice, assigned by buildStateLayout (-1 = none).
        // Blocks scatter/gather through these when SimCtx::x != nullptr.
        int xOffset = -1;
        int xLength = 0;
        int gOffset = -1;
        int gLength = 0;
        int mode = 0; // discontinuous-block branch/mode, latched at events only

        // Vector-signal state:
        // - vec:      per-element internal state (vector integrator, unit
        //             delay, ... ), the vector counterpart of `scalar`.
        // - vec2:     second per-element state (hpf-style raw state).
        // - outLatch: latched port-0 output used for the algebraic warm start
        //             (the vector counterpart of `output`).
        // - displayValues: last values shown by a display sink (width > 1).
        std::vector<double> vec;
        std::vector<double> vec2;
        std::vector<double> outLatch;
        std::vector<double> displayValues;
        std::vector<double> displayValuesImag;
        std::vector<long long> displayValuesI64;
        bool dashboardTouched = false;
        std::string dashboardError;
        // Merge block: source block nid feeding each input port, resolved at INIT so
        // ALGEBRAIC can read each source's SubsystemState::lastActiveStamp and copy the
        // branch that ran this step (holding outLatch otherwise).
        std::vector<int> mergeSourceNids;
        // functionCallGenerator: nids of the callee subsystems it drives (event output
        // wired to their control port), resolved after INIT.
        std::vector<int> functionCallCallees;
        // Last major step at which this generator fired its callees, so repeated
        // ALGEBRAIC evaluations within one step invoke them once.
        long long lastInvokeStamp = -1;
    };
    //=============================================================================
    // SimDiagnostic / SimDiagnostics live in SimDiagnostic.hpp so ode_solvers can
    // share them without including this header.
    //=============================================================================
    // Simulation context passed to block handlers. Hot-path data is flat vectors
    // indexed by numeric ids.
    struct SimCtx
    {
        const std::vector<Block>& blocks;
        // Numeric graph (precompiled, immutable per simulation run):
        const std::vector<std::vector<int>>& inputIds; // [nid][portIdx] = source output slot
        const std::vector<int>& outputBase; // [nid] = first output slot index
        // Label routing (string-keyed small maps, used only for label source/sink blocks):
        const std::unordered_map<std::string, std::string>& labelSinks; // name --> sinkBlockId
        const std::unordered_map<std::string, BlockId>& nameToId; // blockId --> nid
        // Runtime data (vector-indexed, O(1) access):
        std::vector<BlockState>& blockState; // [nid]
        std::vector<double>& outputs; // [output slot]
        const std::unordered_map<std::string, double>& variables;
        double dt;
        double t;
        // Trailing members carry default initializers so existing aggregate
        // brace-initializations (which supply up to `t`) keep compiling.
        double tEnd = 0.0; // last simulated time (samples * dt)
        SimDiagnostics* diag = nullptr; // structured error/warn channel (optional)
        // Global continuous-state scatter/gather (assigned by buildStateLayout). When
        // x == nullptr the engine uses the per-block BlockState scalars (fixed-step path).
        double* x = nullptr; // continuous state vector [nx]
        double* xdot = nullptr; // derivative output, DERIVATIVE phase [nx]
        double* g = nullptr; // zero-crossing surfaces, ZERO_CROSSING phase
        int nx = 0; // length of x / xdot
        // Directory of the .nflow file (for resolving relative asset paths, e.g.
        // imported FMU packages). Empty/nullptr for an unsaved diagram.
        const std::string* baseDir = nullptr;
        // Roots of the packaged example trees for nflow-example:/<namespace>/... asset
        // paths: "fmi" for nflow_fmi, "" for the nflow_blocks categories. Optional.
        const std::map<std::string, std::string>* exampleRoots = nullptr;
        // Signal tables resolved by buildSignalLayout before the run. When null, every
        // port is a scalar double and the outputBase arithmetic applies.
        const std::vector<PortSig>* sigs = nullptr; // [gpi]
        const std::vector<int>* portBase = nullptr; // [nid] -> first gpi
        // [nid][port] -> source gpi (-1 unconnected); parallels inputIds, which keeps
        // the source's first slot for scalar fast paths.
        const std::vector<std::vector<int>>* inputSigs = nullptr;
        // Sibling buffers indexed by the same PortSig.offset as `outputs`: the
        // imaginary lane (complex ports) and the exact 64-bit lane (Int64/UInt64 ports,
        // uint64 stored as its raw bit pattern). Allocated only when the layout needs
        // them. The real lane always carries a double projection for display,
        // serialization and legacy readers.
        std::vector<double>* outputsImag = nullptr;
        std::vector<long long>* outputsI64 = nullptr;
        // Bus type registry of the running diagram (nullptr = no bus types). Read-only
        // at runtime (busSelector member offsets, result serialization).
        const BusRegistry* busRegistry = nullptr;
        // Set by a block (an FMU returning terminateRequested) to ask the engine
        // to stop the run early and keep the results collected so far.
        bool stopRequested = false;
        // Diagram solver relative tolerance (default 1e-6). Read by blocks whose
        // init needs it, e.g. an FMU passing it to enterInitializationMode so its
        // internal solver integrates at the diagram's tolerance, not its own default.
        double relTol = 1e-6;
        // Major-step counter, incremented once per output sample in both engine loops.
        // A conditional subsystem stamps SubsystemState::lastActiveStamp with it so a
        // Merge block can tell which source ran this step. Stable across a step's
        // Gauss-Seidel iterations.
        long long stepIndex = 0;
        // Function-call hook installed by the sim loop. A functionCallGenerator calls
        // invokeSubsystem(calleeNid, iterations) to run a functionCall subsystem
        // (ALGEBRAIC + UPDATE) outside the topological schedule. Left empty during the
        // solver's recordAt / rhs / roots passes so those evaluations never fire
        // callees; an empty target means "do not invoke".
        std::function<void(int, int)> invokeSubsystem;
        // Set by the solver loop around its side-effect-free evaluations (RK/CVODES rhs
        // and zero-crossing roots), false during recordAt and the discrete update.
        // invokeSubsystem checks it so a functionCallGenerator fires its callees once
        // per major step (in recordAt) and never during a sub-stage evaluation.
        bool sideEffectFree = false;
        // Set on the inner SimCtx by an iterator subsystem before each pass over its
        // body: iterationIndex is the current 1-based iteration, iterationCount the
        // total for this step. The iteratorNumber block reads them; both are 0 outside
        // an iterator body.
        int iterationIndex = 0;
        int iterationCount = 0;
        // True when the run integrates without the DAE residual form (fixed-step macro
        // loop and the native explicit backends ode1/ode4/ode45): a physicalIsland then
        // freezes its DERIVATIVE slice and advances itself once per major step in OUTPUT
        // with an embedded implicit (backward-Euler + Newton) solve. False under
        // 'dae'/'idas' (IDAS consumes the residual + id mask directly) and on the
        // linearization path. Propagated into subsystem inner contexts.
        bool islandImplicitStep = false;
        // True only during the t = 0 rhs evaluation that precedes the DAE backend init
        // (x then points at the engine's own state vector). A block that solves its own
        // algebraic components (a physical island) writes a consistent initial guess
        // into its x slice there, so IDACalcIC starts at the solution instead of a cold
        // start it may fail to reach (a mode switch such as an ideal diode flips under
        // a stale Jacobian).
        bool initialStateSeed = false;
    };
    //=============================================================================
    // Simulation phases.
    //
    // These values cross the C plugin ABI (BlockPlugin.h casts Phase to int), so
    // they are APPEND-ONLY with explicit numbering: never insert or renumber, or
    // already-compiled third-party plugins silently mis-dispatch.
    enum class Phase
    {
        INIT = 0,
        OUTPUT = 1,
        ALGEBRAIC = 2,
        AFTER_STEP = 3,
        UPDATE = 4,
        DERIVATIVE = 5, // write xdot into the block's continuous-state slice
        ZERO_CROSSING = 6, // write zero-crossing surface values into the block's g slice
        EVENT = 7 // handle a located state event: update discrete state and write
                  // the (possibly reset) continuous state back into the x slice.
                  // Run by the solver loop at each zero-crossing before reinit.
    };
    //=============================================================================
    enum class SampleTimeClass
    {
        Inherited,
        Continuous,
        Discrete,
        Constant
    };
    //=============================================================================
    // Rounding mode used when quantizing a double-backed value to an integer
    // signal type (convert block, typed block outputs).
    enum class SigRounding
    {
        Nearest = 0,
        Floor = 1,
        Ceiling = 2,
        Zero = 3
    };
    //=============================================================================
    inline void
    sigTypeRange(SigType t, double& lo, double& hi)
    {
        switch (t) {
        case SigType::Int8:
            lo = -128.0;
            hi = 127.0;
            break;
        case SigType::UInt8:
            lo = 0.0;
            hi = 255.0;
            break;
        case SigType::Int16:
            lo = -32768.0;
            hi = 32767.0;
            break;
        case SigType::UInt16:
            lo = 0.0;
            hi = 65535.0;
            break;
        case SigType::Int32:
            lo = -2147483648.0;
            hi = 2147483647.0;
            break;
        case SigType::UInt32:
            lo = 0.0;
            hi = 4294967295.0;
            break;
        case SigType::Int64:
            // Closest-double approximations; the EXACT bounds do not fit a
            // double. Every exact path must use the i64 helpers below; this
            // double range only serves display-grade clamping.
            lo = -9223372036854775808.0;
            hi = 9223372036854775807.0; // rounds to 2^63
            break;
        case SigType::UInt64:
            lo = 0.0;
            hi = 18446744073709551615.0; // rounds to 2^64
            break;
        default:
            lo = -std::numeric_limits<double>::infinity();
            hi = std::numeric_limits<double>::infinity();
            break;
        }
    }
    //=============================================================================
    inline bool
    sigTypeIsInteger(SigType t)
    {
        return t == SigType::Int8 || t == SigType::UInt8 || t == SigType::Int16
            || t == SigType::UInt16 || t == SigType::Int32 || t == SigType::UInt32
            || t == SigType::Int64 || t == SigType::UInt64;
    }
    //=============================================================================
    // Exact 64-bit integer types are carried in the 64-bit lane (SimCtx::outputsI64);
    // the double lane cannot hold them losslessly. The <= 32-bit integer types stay
    // double-backed because their full range fits a double exactly.
    inline bool
    sigTypeIsExact64(SigType t)
    {
        return t == SigType::Int64 || t == SigType::UInt64;
    }
    //=============================================================================
    // Storage lanes a port uses beyond the real lane. Shared by every marshaller
    // (subsystem boundary, bus packing).
    inline bool
    portUsesImag(const PortSig& ps)
    {
        return ps.isComplex;
    }
    //=============================================================================
    inline bool
    portUsesI64(const PortSig& ps)
    {
        return sigTypeIsExact64(ps.type);
    }
    //=============================================================================
    // Exact 64-bit integer support. Values travel as `long long`; a UInt64 value is
    // the raw bit pattern of the unsigned value. MSVC has no 128-bit integer type, so
    // overflow detection is explicit.
    //=============================================================================
    // Double projection of an exact-64 value (display / serialization /
    // legacy readers). Lossy above 2^53 by nature.
    inline double
    i64ToDouble(long long v, SigType t)
    {
        if (t == SigType::UInt64) {
            return static_cast<double>(static_cast<unsigned long long>(v));
        }
        return static_cast<double>(v);
    }
    //=============================================================================
    // Round a double per SigRounding (NaN handled by the callers).
    inline double
    sigRoundDouble(double v, SigRounding rounding)
    {
        switch (rounding) {
        case SigRounding::Floor:
            return std::floor(v);
        case SigRounding::Ceiling:
            return std::ceil(v);
        case SigRounding::Zero:
            return std::trunc(v);
        default:
            return std::round(v);
        }
    }
    //=============================================================================
    // Quantize a double into an exact-64 value (saturate or wrap). The
    // double->integer conversion itself is exact: any in-range double is an
    // integer multiple of a power of two, and fmod on doubles is exact.
    inline long long
    quantizeDoubleToI64(
        double v, SigType t, bool saturate = true, SigRounding rounding = SigRounding::Nearest)
    {
        if (std::isnan(v)) {
            return 0;
        }
        double r = sigRoundDouble(v, rounding);
        if (t == SigType::UInt64) {
            constexpr double two64 = 18446744073709551616.0; // 2^64, exact
            if (saturate) {
                if (r <= 0.0) {
                    return 0;
                }
                if (r >= two64) {
                    return static_cast<long long>(~0ULL); // UINT64_MAX bits
                }
                return static_cast<long long>(static_cast<unsigned long long>(r));
            }
            double m = std::fmod(r, two64);
            if (m < 0.0) {
                m += two64;
            }
            return static_cast<long long>(static_cast<unsigned long long>(m));
        }
        constexpr double two63 = 9223372036854775808.0; // 2^63, exact
        if (saturate) {
            if (r >= two63) {
                return std::numeric_limits<long long>::max();
            }
            if (r < -two63) {
                return std::numeric_limits<long long>::min();
            }
            return static_cast<long long>(r);
        }
        constexpr double two64 = 18446744073709551616.0;
        double m = std::fmod(r, two64);
        if (m < 0.0) {
            m += two64;
        }
        // m in [0, 2^64): reinterpret as two's-complement.
        return static_cast<long long>(static_cast<unsigned long long>(m));
    }
    //=============================================================================
    // Signed 64-bit add/sub/mul with explicit overflow handling. `saturate`
    // selects clamping to INT64_MIN/MAX; otherwise two's-complement wrap
    // (well-defined via unsigned arithmetic).
    inline long long
    i64Add(long long a, long long b, bool saturate)
    {
        const unsigned long long ua = static_cast<unsigned long long>(a);
        const unsigned long long ub = static_cast<unsigned long long>(b);
        const long long wrapped = static_cast<long long>(ua + ub);
        if (!saturate) {
            return wrapped;
        }
        // Overflow iff operands share a sign the result does not.
        if (a >= 0 && b >= 0 && wrapped < 0) {
            return std::numeric_limits<long long>::max();
        }
        if (a < 0 && b < 0 && wrapped >= 0) {
            return std::numeric_limits<long long>::min();
        }
        return wrapped;
    }
    //=============================================================================
    inline long long
    i64Sub(long long a, long long b, bool saturate)
    {
        const unsigned long long ua = static_cast<unsigned long long>(a);
        const unsigned long long ub = static_cast<unsigned long long>(b);
        const long long wrapped = static_cast<long long>(ua - ub);
        if (!saturate) {
            return wrapped;
        }
        if (a >= 0 && b < 0 && wrapped < 0) {
            return std::numeric_limits<long long>::max();
        }
        if (a < 0 && b >= 0 && wrapped >= 0) {
            return std::numeric_limits<long long>::min();
        }
        return wrapped;
    }
    //=============================================================================
    inline long long
    i64Mul(long long a, long long b, bool saturate)
    {
        const unsigned long long ua = static_cast<unsigned long long>(a);
        const unsigned long long ub = static_cast<unsigned long long>(b);
        const long long wrapped = static_cast<long long>(ua * ub);
        if (!saturate) {
            return wrapped;
        }
        // Overflow check without a 128-bit type: division round-trip.
        if (a != 0
            && (wrapped / a != b || (a == -1 && b == std::numeric_limits<long long>::min())
                || (b == -1 && a == std::numeric_limits<long long>::min()))) {
            const bool negative = (a < 0) != (b < 0);
            return negative ? std::numeric_limits<long long>::min()
                            : std::numeric_limits<long long>::max();
        }
        return wrapped;
    }
    //=============================================================================
    // Signed 64-bit division with integer rounding (b == 0 saturates to the
    // sign of a, 0/0 -> 0; INT64_MIN / -1 saturates or wraps).
    inline long long
    i64Div(long long a, long long b, bool saturate, SigRounding rounding)
    {
        if (b == 0) {
            if (a == 0) {
                return 0;
            }
            return (a > 0) ? std::numeric_limits<long long>::max()
                           : std::numeric_limits<long long>::min();
        }
        if (a == std::numeric_limits<long long>::min() && b == -1) {
            return saturate ? std::numeric_limits<long long>::max()
                            : std::numeric_limits<long long>::min(); // wrap: -MIN == MIN
        }
        long long q = a / b;
        const long long r = a % b;
        if (r != 0) {
            const bool negQuot = (a < 0) != (b < 0);
            switch (rounding) {
            case SigRounding::Floor:
                if (negQuot) {
                    q -= 1;
                }
                break;
            case SigRounding::Ceiling:
                if (!negQuot) {
                    q += 1;
                }
                break;
            case SigRounding::Zero:
                break;
            default: { // Nearest, ties away from zero (std::round semantics)
                const unsigned long long absR = (r < 0)
                    ? static_cast<unsigned long long>(-(r + 1)) + 1ULL
                    : static_cast<unsigned long long>(r);
                const unsigned long long absB = (b < 0)
                    ? static_cast<unsigned long long>(-(b + 1)) + 1ULL
                    : static_cast<unsigned long long>(b);
                if (2ULL * absR >= absB) {
                    q += negQuot ? -1 : 1;
                }
                break;
            }
            }
        }
        return q;
    }
    //=============================================================================
    // Unsigned 64-bit counterparts, operating on/returning bit patterns.
    inline long long
    u64Add(long long a, long long b, bool saturate)
    {
        const unsigned long long ua = static_cast<unsigned long long>(a);
        const unsigned long long ub = static_cast<unsigned long long>(b);
        const unsigned long long s = ua + ub;
        if (saturate && s < ua) {
            return static_cast<long long>(~0ULL);
        }
        return static_cast<long long>(s);
    }
    //=============================================================================
    inline long long
    u64Sub(long long a, long long b, bool saturate)
    {
        const unsigned long long ua = static_cast<unsigned long long>(a);
        const unsigned long long ub = static_cast<unsigned long long>(b);
        if (saturate && ub > ua) {
            return 0;
        }
        return static_cast<long long>(ua - ub);
    }
    //=============================================================================
    inline long long
    u64Mul(long long a, long long b, bool saturate)
    {
        const unsigned long long ua = static_cast<unsigned long long>(a);
        const unsigned long long ub = static_cast<unsigned long long>(b);
        const unsigned long long p = ua * ub;
        if (saturate && ua != 0 && p / ua != ub) {
            return static_cast<long long>(~0ULL);
        }
        return static_cast<long long>(p);
    }
    //=============================================================================
    inline long long
    u64Div(long long a, long long b, bool /*saturate*/, SigRounding rounding)
    {
        const unsigned long long ua = static_cast<unsigned long long>(a);
        const unsigned long long ub = static_cast<unsigned long long>(b);
        if (ub == 0) {
            return (ua == 0) ? 0 : static_cast<long long>(~0ULL);
        }
        unsigned long long q = ua / ub;
        const unsigned long long r = ua % ub;
        if (r != 0) {
            switch (rounding) {
            case SigRounding::Ceiling:
                q += 1;
                break;
            case SigRounding::Floor:
            case SigRounding::Zero:
                break;
            default: // Nearest, ties away from zero
                if (2ULL * r >= ub) {
                    q += 1;
                }
                break;
            }
        }
        return static_cast<long long>(q);
    }
    //=============================================================================
    inline long long
    i64ApplyAdd(long long a, long long b, SigType t, bool sat)
    {
        return (t == SigType::UInt64) ? u64Add(a, b, sat) : i64Add(a, b, sat);
    }
    //=============================================================================
    inline long long
    i64ApplySub(long long a, long long b, SigType t, bool sat)
    {
        return (t == SigType::UInt64) ? u64Sub(a, b, sat) : i64Sub(a, b, sat);
    }
    //=============================================================================
    inline long long
    i64ApplyMul(long long a, long long b, SigType t, bool sat)
    {
        return (t == SigType::UInt64) ? u64Mul(a, b, sat) : i64Mul(a, b, sat);
    }
    //=============================================================================
    inline long long
    i64ApplyDiv(long long a, long long b, SigType t, bool sat, SigRounding r)
    {
        return (t == SigType::UInt64) ? u64Div(a, b, sat, r) : i64Div(a, b, sat, r);
    }
    //=============================================================================
    inline long long
    i64ApplyNeg(long long a, SigType t, bool sat)
    {
        if (t == SigType::UInt64) {
            // -u < 0 for any u > 0: saturation clamps to 0; wrap is
            // two's-complement negation of the bit pattern.
            if (sat) {
                return 0;
            }
            return static_cast<long long>(0ULL - static_cast<unsigned long long>(a));
        }
        return i64Sub(0, a, sat);
    }
    //=============================================================================
    inline long long
    i64ApplyAbs(long long a, SigType t, bool sat)
    {
        if (t == SigType::UInt64) {
            return a; // already non-negative
        }
        return (a < 0) ? i64Sub(0, a, sat) : a;
    }
    //=============================================================================
    inline bool
    i64Less(long long a, long long b, SigType t)
    {
        if (t == SigType::UInt64) {
            return static_cast<unsigned long long>(a) < static_cast<unsigned long long>(b);
        }
        return a < b;
    }
    //=============================================================================
    // Relational comparison in exact-64 space; op names match the comparison blocks'
    // operator parameters.
    inline bool
    i64CompareOp(long long a, long long b, SigType t, const std::string& op)
    {
        if (op == "gt" || op == ">") {
            return i64Less(b, a, t);
        }
        if (op == "le" || op == "<=") {
            return !i64Less(b, a, t);
        }
        if (op == "lt" || op == "<") {
            return i64Less(a, b, t);
        }
        if (op == "eq" || op == "==") {
            return a == b;
        }
        if (op == "ne" || op == "!=" || op == "~=") {
            return a != b;
        }
        return !i64Less(a, b, t); // ge (default)
    }
    //=============================================================================
    // Exact cast between the two 64-bit types (bit pattern preserved on
    // wrap; saturation clamps values outside the target's range).
    inline long long
    requantizeI64(long long v, SigType from, SigType to, bool saturate)
    {
        if (from == to) {
            return v;
        }
        if (to == SigType::UInt64) {
            // Int64 -> UInt64: negatives clamp to 0 under saturation.
            if (saturate && v < 0) {
                return 0;
            }
            return v;
        }
        // UInt64 -> Int64: values above INT64_MAX (negative bit pattern)
        // clamp under saturation.
        if (saturate && v < 0) {
            return std::numeric_limits<long long>::max();
        }
        return v;
    }
    //=============================================================================
    // Quantize a computed double to the semantics of the signal type:
    // - Double: unchanged. Single: rounded through float precision.
    // - Boolean: 0.0 / 1.0 (NaN counts as false).
    // - Integer types: rounded (SigRounding), then saturated to the type's
    //   range or wrapped modulo 2^n.
    // - Int64/UInt64: exact paths use quantizeDoubleToI64 + the 64-bit lane;
    //   this double-based fallback quantizes exactly then projects (lossy
    //   above 2^53 by nature of the return type).
    inline double
    quantizeToType(
        double v, SigType t, bool saturate = true, SigRounding rounding = SigRounding::Nearest)
    {
        if (sigTypeIsExact64(t)) {
            return i64ToDouble(quantizeDoubleToI64(v, t, saturate, rounding), t);
        }
        switch (t) {
        case SigType::Double:
            return v;
        case SigType::Single:
            return static_cast<double>(static_cast<float>(v));
        case SigType::Boolean:
            return (!std::isnan(v) && v != 0.0) ? 1.0 : 0.0;
        default:
            break;
        }
        if (std::isnan(v)) {
            return 0.0;
        }
        double r;
        switch (rounding) {
        case SigRounding::Floor:
            r = std::floor(v);
            break;
        case SigRounding::Ceiling:
            r = std::ceil(v);
            break;
        case SigRounding::Zero:
            r = std::trunc(v);
            break;
        default:
            r = std::round(v);
            break;
        }
        double lo, hi;
        sigTypeRange(t, lo, hi);
        if (saturate) {
            return std::min(std::max(r, lo), hi);
        }
        // Wrap modulo the type's span (two's-complement behavior).
        const double span = hi - lo + 1.0;
        double m = std::fmod(r - lo, span);
        if (m < 0.0) {
            m += span;
        }
        return lo + m;
    }
    //=============================================================================
    // Returns false when the name is unknown.
    inline bool
    sigTypeFromString(const std::string& s, SigType& out)
    {
        if (s == "double" || s.empty()) {
            out = SigType::Double;
        } else if (s == "single" || s == "float") {
            out = SigType::Single;
        } else if (s == "int8") {
            out = SigType::Int8;
        } else if (s == "uint8") {
            out = SigType::UInt8;
        } else if (s == "int16") {
            out = SigType::Int16;
        } else if (s == "uint16") {
            out = SigType::UInt16;
        } else if (s == "int32") {
            out = SigType::Int32;
        } else if (s == "uint32") {
            out = SigType::UInt32;
        } else if (s == "int64") {
            out = SigType::Int64;
        } else if (s == "uint64") {
            out = SigType::UInt64;
        } else if (s == "boolean" || s == "logical" || s == "bool") {
            out = SigType::Boolean;
        } else {
            return false;
        }
        return true;
    }
    //=============================================================================
    inline const char*
    sigTypeName(SigType t)
    {
        switch (t) {
        case SigType::Double:
            return "double";
        case SigType::Single:
            return "single";
        case SigType::Int8:
            return "int8";
        case SigType::UInt8:
            return "uint8";
        case SigType::Int16:
            return "int16";
        case SigType::UInt16:
            return "uint16";
        case SigType::Int32:
            return "int32";
        case SigType::UInt32:
            return "uint32";
        case SigType::Boolean:
            return "boolean";
        case SigType::Int64:
            return "int64";
        case SigType::UInt64:
            return "uint64";
        }
        return "double";
    }
    //=============================================================================
    // Custom output-dimension rule signature (BlockMetadata::resolveDims).
    // inSigs holds the current dims of each input port (default PortSig for
    // unconnected); outSigs arrives pre-filled with the port's current dims
    // and is written with the computed ones. Return false + err on conflict.
    using ResolveDimsFn = std::function<bool(const Block& b, const ValMap& vars,
        const std::vector<PortSig>& inSigs, std::vector<PortSig>& outSigs, std::string& err)>;
    // Bus-aware variant (BlockMetadata::resolveBusDims): same contract with
    // access to the mutable bus registry (declared in SimEngineBusTypes.hpp).
    using ResolveBusDimsFn
        = std::function<bool(const Block& b, const ValMap& vars, const std::vector<PortSig>& inSigs,
            std::vector<PortSig>& outSigs, BusRegistry& busReg, std::string& err)>;
    //=============================================================================
    // Per-block-type numerical metadata, declared at registration. Used by the
    // variable-step machinery (state-vector layout, zero-crossing surfaces,
    // feedthrough-aware ordering). Default: no continuous state, feedthrough iff the
    // block runs in the ALGEBRAIC phase.
    struct BlockMetadata
    {
        int numContinuousStates = 0;
        int numDiscreteStates = 0;
        int numZeroCrossings = 0;
        // Differential-algebraic: when true, the block's continuous states are
        // algebraic (mass-0). The DAE solver gives them id = 0 and reads the block's
        // DERIVATIVE-phase output as the algebraic residual g(x) = 0, not a derivative.
        bool algebraicState = false;
        bool directFeedthrough = false;
        SampleTimeClass sampleTime = SampleTimeClass::Inherited;
        // Blocks whose state / zero-crossing counts depend on parameters (tf order,
        // stateSpace size) resolve them after INIT through these callbacks. When null
        // the static counts above apply. Width-scaled counts (vector integrator) read
        // the Block's inWidth/outWidth.
        std::function<int(const Block&, const ValMap&)> continuousStateCount;
        std::function<int(const Block&, const ValMap&)> zeroCrossingCount;
        // Whether the handler is generalized over signal width. When false, the
        // compile pass rejects any width > 1 input wired to this block instead of
        // letting a scalar handler read element 0.
        bool acceptsVectorInputs = false;
        // Custom output-dimension rule (mux/demux/constant/...). When null every
        // output port takes the element-wise maximum of the input dims (scalar
        // expansion; two different non-scalar widths are a compile error). Returns
        // false + err on a dimension conflict. When present it also owns the output
        // port types (outSigs[].type).
        ResolveDimsFn resolveDims;
        // Default type rule: unify the connected input types (all equal -> that type;
        // a mix is a compile error prompting for a convert block).
        // allowMixedInputTypes disables the check (comparison/logic/switch/sinks accept
        // any mix; the output takes the first connected input's type unless
        // fixedOutSigType is set).
        bool allowMixedInputTypes = false;
        // Force every output port to this SigType (-1 = inherit the unified
        // input type). Logic and comparison blocks emit Boolean.
        int fixedOutSigType = -1;
        // Continuous-dynamics blocks integrate real states: reject any
        // non-Double input at compile time.
        bool requiresDoubleInputs = false;
        // Whether the handler has an exact int64/uint64 path (reads the 64-bit lane).
        // When false, an exact-64 signal wired to this block is a compile error instead
        // of a lossy double round-trip.
        bool supportsExact64 = false;
        // Whether the handler understands the imaginary lane. When false, a complex
        // signal wired to this block is a compile error (the real part alone would be
        // wrong).
        bool acceptsComplexInputs = false;
        // UI controls that need event polling and paced execution. Kept in metadata
        // so the engine does not hard-code block types.
        bool interactive = false;
        // Output complexity override: -1 = default OR-propagation (any
        // complex input -> complex output), 0 = always real (abs, the
        // complex-to-real conversion blocks), 1 = always complex (the
        // real-to-complex conversion blocks), 2 = rule-owned (the block's
        // resolveDims sets the flag itself, e.g. subsystem adopting its
        // inner sink's descriptor).
        int forcesComplexOut = -1;
        // Whether the block accepts bus-typed inputs (busCreator nesting,
        // busSelector, bus-aware sinks). When false, a bus wired to this
        // block is a compile error prompting for a busSelector.
        bool acceptsBusInputs = false;
        // Bus-aware dimension rule: like resolveDims but with access to the
        // (mutable) bus registry so busCreator can register inferred types
        // and busSelector can resolve member paths. Takes precedence over
        // resolveDims when a registry is active.
        ResolveBusDimsFn resolveBusDims;
        // A block whose continuous states mix differential and algebraic entries (a
        // physical island: node potentials are algebraic, energy-storage states are
        // differential) fills ids[k] with 1.0 (differential) or 0.0 (algebraic) for
        // each of its xLength states (ids arrives pre-sized, all 1.0). Takes
        // precedence over `algebraicState` when set; when null, `algebraicState`
        // applies to the whole slice.
        std::function<void(const Block&, const ValMap&, std::vector<double>& ids)>
            algebraicStateMask;
        // Per-surface crossing direction the solver reports: +1 rising only, -1
        // falling only, 0 both (default). `dirs` arrives pre-sized to the block's
        // zero-crossing count, all 0. Feeds rootDirection of the solver backends
        // (CVODES / IDAS / native fixed-RK). When null every surface reports both.
        std::function<void(const Block&, const ValMap&, std::vector<int>& dirs)>
            zeroCrossingDirections;
        // A source with hard output jumps at times known in advance (step edge,
        // square / pulse / sawtooth / trapezoid edges, staircase samples) lists its
        // switching instants in (t0, t1] here. On the variable-step DAE path the solver
        // stops just before each instant, so the multistep corrector never solves the
        // discontinuous residual across the jump, then recomputes consistent initial
        // conditions (IDACalcIC) at the instant. When null the block schedules none.
        std::function<void(
            const Block&, const ValMap&, double t0, double t1, std::vector<double>& instants)>
            discontinuityInstants;
        // A direct-feedthrough block whose output must be recomputed from the current
        // input on every base step, even with a slower SampleTime; only its UPDATE is
        // gated on the sample hit. difference and the detect* family work this way:
        // codegen emits `out = in - prev` every step and gates only the `prev` advance.
        // When true, evalAlgebraicBlockMultirate calls the handler directly, with no
        // sample-and-hold.
        bool algebraicOutputEveryStep = false;
    };
    //=============================================================================
    // Block handler function signature (std::function to support capturing wrappers)
    using BlockHandlerFn = std::function<bool(SimCtx&, const Block&, Phase)>;
    //=============================================================================
    // Inline fast-path helpers - inlined here so handlers see zero-overhead expansion
    //=============================================================================
    // Signal descriptor of output port 'port' of block nid, or nullptr when
    // no signal layout is active (all-scalar legacy path).
    inline const PortSig*
    portSigOf(const SimCtx& ctx, BlockId nid, int port = 0)
    {
        if (!ctx.sigs || !ctx.portBase || nid < 0 || nid >= (int)ctx.portBase->size()) {
            return nullptr;
        }
        const int gpi = (*ctx.portBase)[nid] + port;
        if (gpi < 0 || gpi >= (int)ctx.sigs->size()) {
            return nullptr;
        }
        return &(*ctx.sigs)[gpi];
    }
    //=============================================================================
    inline int
    slotOf(const SimCtx& ctx, BlockId nid, int port = 0)
    {
        const PortSig* ps = portSigOf(ctx, nid, port);
        if (ps) {
            return ps->offset;
        }
        return ctx.outputBase[nid] + port;
    }
    //=============================================================================
    inline int
    outputWidth(const SimCtx& ctx, BlockId nid, int port = 0)
    {
        const PortSig* ps = portSigOf(ctx, nid, port);
        return ps ? ps->width() : 1;
    }
    //=============================================================================
    inline double*
    outputSlice(SimCtx& ctx, BlockId nid, int port = 0)
    {
        return ctx.outputs.data() + slotOf(ctx, nid, port);
    }
    //=============================================================================
    // Mutable pointer into the imaginary lane for output port 'port', or nullptr
    // when the lane is absent.
    inline double*
    outputSliceImag(SimCtx& ctx, BlockId nid, int port = 0)
    {
        if (!ctx.outputsImag) {
            return nullptr;
        }
        return ctx.outputsImag->data() + slotOf(ctx, nid, port);
    }
    //=============================================================================
    // Mutable pointer into the exact 64-bit lane for output port 'port', or
    // nullptr when the lane is absent.
    inline long long*
    outputSliceI64(SimCtx& ctx, BlockId nid, int port = 0)
    {
        if (!ctx.outputsI64) {
            return nullptr;
        }
        return ctx.outputsI64->data() + slotOf(ctx, nid, port);
    }
    //=============================================================================
    inline void
    setOutput(SimCtx& ctx, BlockId nid, double v, int port = 0)
    {
        ctx.outputs[slotOf(ctx, nid, port)] = v;
    }
    //=============================================================================
    inline double
    getOutput(const SimCtx& ctx, int slot, double fallback = 0.0)
    {
        if (slot < 0 || slot >= static_cast<int>(ctx.outputs.size())) {
            return fallback;
        }
        return ctx.outputs[slot];
    }
    //=============================================================================
    // Read input port 'portIdx' of block nid (element 0 for vector signals).
    inline double
    getInput(const SimCtx& ctx, BlockId nid, int portIdx, double fallback = 0.0)
    {
        if (nid < 0 || nid >= static_cast<int>(ctx.inputIds.size())) {
            return fallback;
        }
        const auto& ids = ctx.inputIds[nid];
        if (portIdx < 0 || portIdx >= static_cast<int>(ids.size()) || ids[portIdx] < 0) {
            return fallback;
        }
        return ctx.outputs[ids[portIdx]];
    }
    //=============================================================================
    // Vector signal accessors
    //=============================================================================
    // Read-only view of the signal wired to input port 'portIdx' of block nid.
    // For an unconnected port (or when no layout is active) data is null and
    // width is 0; read through sigAt() with a fallback.
    struct SigView
    {
        const double* data = nullptr;
        int rows = 1;
        int cols = 1;
        int numDims = 1;
        // Full N-D shape of the source port (points into the PortSig's dims,
        // stable for the whole run). Null on the legacy scalar path; rows/
        // cols/numDims above stay the fast 2-D projection for hot handlers.
        const int* dims = nullptr;
        SigType type = SigType::Double;
        int width = 0; // 0 = unconnected / absent
        // Typed lanes of the source port (same indexing as data). imag is
        // non-null iff the port is complex and the imaginary lane is
        // allocated; idata iff the port is exact-64 and the 64-bit lane is
        // allocated.
        const double* imag = nullptr;
        const long long* idata = nullptr;
        bool isComplex = false;
    };
    //=============================================================================
    inline SigView
    getInputSig(const SimCtx& ctx, BlockId nid, int portIdx)
    {
        SigView v;
        if (nid < 0 || nid >= static_cast<int>(ctx.inputIds.size())) {
            return v;
        }
        const auto& ids = ctx.inputIds[nid];
        if (portIdx < 0 || portIdx >= static_cast<int>(ids.size()) || ids[portIdx] < 0) {
            return v;
        }
        if (ctx.inputSigs && ctx.sigs && nid < (int)ctx.inputSigs->size()) {
            const auto& sIds = (*ctx.inputSigs)[nid];
            if (portIdx < (int)sIds.size() && sIds[portIdx] >= 0
                && sIds[portIdx] < (int)ctx.sigs->size()) {
                const PortSig& ps = (*ctx.sigs)[sIds[portIdx]];
                v.data = ctx.outputs.data() + ps.offset;
                v.rows = ps.rows();
                v.cols = ps.cols();
                v.numDims = ps.numDims();
                v.dims = ps.dims.data();
                v.type = ps.type;
                v.width = ps.width();
                v.isComplex = ps.isComplex;
                // A bus port exposes every allocated lane: its packed
                // members may be complex or exact-64 even though the port
                // itself carries neither flag.
                if ((portUsesImag(ps) || ps.busTypeId >= 0) && ctx.outputsImag) {
                    v.imag = ctx.outputsImag->data() + ps.offset;
                }
                if ((portUsesI64(ps) || ps.busTypeId >= 0) && ctx.outputsI64) {
                    v.idata = ctx.outputsI64->data() + ps.offset;
                }
                return v;
            }
        }
        // Legacy scalar path: the wire is the slot itself.
        v.data = ctx.outputs.data() + ids[portIdx];
        v.width = 1;
        return v;
    }
    //=============================================================================
    // Element read with scalar expansion: a width-1 signal broadcasts its
    // single value to every element index.
    inline double
    sigAt(const SigView& v, int i, double fallback = 0.0)
    {
        if (!v.data || v.width <= 0) {
            return fallback;
        }
        return (v.width == 1) ? v.data[0] : v.data[i];
    }
    //=============================================================================
    // Complex element read with scalar expansion: the real part comes from
    // the double lane, the imaginary part from the imaginary lane (0 for a
    // real source feeding a complex-aware block).
    inline std::complex<double>
    sigAtC(const SigView& v, int i, double fallback = 0.0)
    {
        const double re = sigAt(v, i, fallback);
        if (!v.imag || v.width <= 0) {
            return { re, 0.0 };
        }
        return { re, (v.width == 1) ? v.imag[0] : v.imag[i] };
    }
    //=============================================================================
    // Exact-64 element read with scalar expansion. When the source port is
    // exact-64 the value comes from the 64-bit lane; a double-lane source
    // (unconnected fallback or a plain port) converts through
    // quantizeDoubleToI64 (round-nearest, saturating).
    inline long long
    sigAtI64(const SigView& v, int i, SigType asType, long long fallback = 0)
    {
        if (v.idata && v.width > 0) {
            return (v.width == 1) ? v.idata[0] : v.idata[i];
        }
        if (!v.data || v.width <= 0) {
            return fallback;
        }
        return quantizeDoubleToI64(sigAt(v, i), asType);
    }
    //=============================================================================
    // True when the block declares wrap-on-overflow instead of the default
    // saturation for its integer-typed output ("SaturateOnOverflow": false).
    inline bool
    blockSaturates(const SimCtx& ctx, BlockId nid)
    {
        const json& p = ctx.blocks[nid].params;
        auto it = p.find("SaturateOnOverflow");
        if (it != p.end() && it->is_boolean()) {
            return it->get<bool>();
        }
        return true;
    }
    //=============================================================================
    inline bool
    updated(SimCtx& ctx, BlockId nid, double newVal, int port);
    // Element-wise evaluation of output port 0: computes the port's width elements
    // via elemFn(i), quantizes each to the port's signal type (integer
    // saturation/wrap, boolean 0/1, single precision), writes/compares the slice and
    // latches it for the algebraic warm start. Handlers wrap their scalar math in
    // elemFn and read inputs through sigAt() (scalar expansion included), so an
    // untyped scalar diagram takes the plain path (width 1, Double, no quantization).
    // Returns true when any element changed (Gauss-Jacobi progress contract).
    template <typename ElemFn>
    inline bool
    emitElementwise(SimCtx& ctx, BlockId nid, ElemFn elemFn)
    {
        const PortSig* ps = portSigOf(ctx, nid, 0);
        const int w = ps ? ps->width() : 1;
        const SigType ty = ps ? ps->type : SigType::Double;
        const bool typed = (ty != SigType::Double);
        const bool saturate = typed ? blockSaturates(ctx, nid) : true;
        if (w <= 1) {
            double v = elemFn(0);
            if (typed) {
                v = quantizeToType(v, ty, saturate);
            }
            bool upd = updated(ctx, nid, v, 0);
            ctx.blockState[nid].output = v;
            return upd;
        }
        double* dst = outputSlice(ctx, nid, 0);
        BlockState& st = ctx.blockState[nid];
        if ((int)st.outLatch.size() != w) {
            st.outLatch.assign(w, 0.0);
        }
        bool changed = false;
        for (int i = 0; i < w; ++i) {
            double v = elemFn(i);
            if (typed) {
                v = quantizeToType(v, ty, saturate);
            }
            if (dst[i] != v) {
                dst[i] = v;
                changed = true;
            }
            st.outLatch[i] = v;
        }
        return changed;
    }
    //=============================================================================
    // Output port 0 descriptor when it is an exact-64 port, else nullptr.
    inline const PortSig*
    exact64OutPort(const SimCtx& ctx, BlockId nid)
    {
        const PortSig* ps = portSigOf(ctx, nid, 0);
        return (ps && portUsesI64(*ps)) ? ps : nullptr;
    }
    //=============================================================================
    // Output port 0 descriptor when it is a complex port, else nullptr.
    inline const PortSig*
    complexOutPort(const SimCtx& ctx, BlockId nid)
    {
        const PortSig* ps = portSigOf(ctx, nid, 0);
        return (ps && ps->isComplex) ? ps : nullptr;
    }
    //=============================================================================
    // Exact-64 sibling of emitElementwise: elemFn(i) returns the already quantized
    // in-type `long long` (use the i64Apply* helpers). Writes the 64-bit lane (exact,
    // drives change detection) and the double projection into the real lane, and
    // latches the projection for the algebraic warm start. Falls back to the double
    // path when the lane is absent (defensive: the layout allocates it for any
    // exact-64 port).
    template <typename ElemFn>
    inline bool
    emitElementwiseI64(SimCtx& ctx, BlockId nid, ElemFn elemFn)
    {
        const PortSig* ps = portSigOf(ctx, nid, 0);
        const SigType ty = ps ? ps->type : SigType::Int64;
        long long* di = outputSliceI64(ctx, nid, 0);
        if (!di) {
            return emitElementwise(ctx, nid, [&](int i) { return i64ToDouble(elemFn(i), ty); });
        }
        const int w = ps ? ps->width() : 1;
        double* dd = outputSlice(ctx, nid, 0);
        BlockState& st = ctx.blockState[nid];
        if (w <= 1) {
            const long long v = elemFn(0);
            const bool changed = (di[0] != v);
            di[0] = v;
            const double p = i64ToDouble(v, ty);
            dd[0] = p;
            st.output = p;
            return changed;
        }
        if ((int)st.outLatch.size() != w) {
            st.outLatch.assign(w, 0.0);
        }
        bool changed = false;
        for (int i = 0; i < w; ++i) {
            const long long v = elemFn(i);
            if (di[i] != v) {
                di[i] = v;
                changed = true;
            }
            const double p = i64ToDouble(v, ty);
            dd[i] = p;
            st.outLatch[i] = p;
        }
        return changed;
    }
    //=============================================================================
    // Complex sibling of emitElementwise: elemFn(i) returns a std::complex<double>.
    // Both parts are quantized to the port's numeric type and both lanes drive the
    // changed contract. The real part latches for the algebraic warm start. The
    // fallback to the real path is defensive: the layout allocates the imaginary lane
    // for any complex port.
    template <typename ElemFn>
    inline bool
    emitElementwiseComplex(SimCtx& ctx, BlockId nid, ElemFn elemFn)
    {
        const PortSig* ps = portSigOf(ctx, nid, 0);
        double* dimag = outputSliceImag(ctx, nid, 0);
        if (!dimag) {
            return emitElementwise(ctx, nid, [&](int i) { return elemFn(i).real(); });
        }
        const SigType ty = ps ? ps->type : SigType::Double;
        const bool typed = (ty != SigType::Double);
        const bool saturate = typed ? blockSaturates(ctx, nid) : true;
        const int w = ps ? ps->width() : 1;
        double* dd = outputSlice(ctx, nid, 0);
        BlockState& st = ctx.blockState[nid];
        if ((int)st.outLatch.size() != w && w > 1) {
            st.outLatch.assign(w, 0.0);
        }
        bool changed = false;
        for (int i = 0; i < w; ++i) {
            std::complex<double> v = elemFn(i);
            double re = v.real();
            double im = v.imag();
            if (typed) {
                re = quantizeToType(re, ty, saturate);
                im = quantizeToType(im, ty, saturate);
            }
            if (dd[i] != re) {
                dd[i] = re;
                changed = true;
            }
            if (dimag[i] != im) {
                dimag[i] = im;
                changed = true;
            }
            if (w > 1) {
                st.outLatch[i] = re;
            } else {
                st.output = re;
            }
        }
        return changed;
    }
    //=============================================================================
    // Write the full computed slice of output port 'port', compare against the
    // previous values and latch it for the algebraic warm start. Returns true
    // when any element changed (Gauss-Jacobi progress contract).
    inline bool
    updatedSlice(SimCtx& ctx, BlockId nid, const double* newVals, int n, int port = 0)
    {
        double* dst = outputSlice(ctx, nid, port);
        bool changed = false;
        for (int i = 0; i < n; ++i) {
            if (dst[i] != newVals[i]) {
                dst[i] = newVals[i];
                changed = true;
            }
        }
        if (port == 0) {
            BlockState& st = ctx.blockState[nid];
            st.outLatch.assign(newVals, newVals + n);
        }
        return changed;
    }
    //=============================================================================
    inline bool
    hasInput(const SimCtx& ctx, BlockId nid, int portIdx)
    {
        if (nid < 0 || nid >= static_cast<int>(ctx.inputIds.size())) {
            return false;
        }
        const auto& ids = ctx.inputIds[nid];
        return portIdx >= 0 && portIdx < static_cast<int>(ids.size()) && ids[portIdx] >= 0;
    }
    //=============================================================================
    inline int
    numInputs(const SimCtx& ctx, BlockId nid)
    {
        if (nid < 0 || nid >= static_cast<int>(ctx.inputIds.size())) {
            return 0;
        }
        return static_cast<int>(ctx.inputIds[nid].size());
    }
    //=============================================================================
    inline int
    numOutputs(const SimCtx& ctx, BlockId nid)
    {
        if (nid < 0 || nid >= static_cast<int>(ctx.blocks.size())) {
            return 0;
        }
        return ctx.blocks[nid].outputs;
    }
    //=============================================================================
    inline BlockState&
    getState(SimCtx& ctx, BlockId nid)
    {
        return ctx.blockState[nid];
    }
    //=============================================================================
    // Pointer to block nid's continuous-state slice in the global x vector, or
    // nullptr when no global vector is active (legacy fixed-step path) or the
    // block has no continuous state. Handlers read x from here under a solver and
    // fall back to their BlockState scalars otherwise.
    inline double*
    blockX(SimCtx& ctx, BlockId nid)
    {
        if (!ctx.x) {
            return nullptr;
        }
        const BlockState& s = ctx.blockState[nid];
        return (s.xOffset >= 0) ? ctx.x + s.xOffset : nullptr;
    }
    //=============================================================================
    // Pointer to block nid's derivative slice in the global xdot vector (written
    // during Phase::DERIVATIVE), or nullptr when inactive.
    inline double*
    blockXdot(SimCtx& ctx, BlockId nid)
    {
        if (!ctx.xdot) {
            return nullptr;
        }
        const BlockState& s = ctx.blockState[nid];
        return (s.xOffset >= 0) ? ctx.xdot + s.xOffset : nullptr;
    }
    //=============================================================================
    // Pointer to block nid's zero-crossing-surface slice in the global g vector
    // (written during Phase::ZERO_CROSSING), or nullptr when inactive.
    inline double*
    blockG(SimCtx& ctx, BlockId nid)
    {
        if (!ctx.g) {
            return nullptr;
        }
        const BlockState& s = ctx.blockState[nid];
        return (s.gOffset >= 0) ? ctx.g + s.gOffset : nullptr;
    }
    //=============================================================================
    // Write output port 'port' of block nid and return true if the value changed.
    inline bool
    updated(SimCtx& ctx, BlockId nid, double newVal, int port = 0)
    {
        double& prev = ctx.outputs[slotOf(ctx, nid, port)];
        if (prev != newVal) {
            prev = newVal;
            return true;
        }
        return false;
    }
    //=============================================================================
    // Boolean typed helpers (0.0 == false, non-zero == true)
    inline bool
    getInputBool(const SimCtx& ctx, BlockId nid, int portIdx, bool fallback = false)
    {
        if (!hasInput(ctx, nid, portIdx)) {
            return fallback;
        }
        double v = ctx.outputs[ctx.inputIds[nid][portIdx]];
        return std::isnan(v) ? fallback : (v != 0.0);
    }
    //=============================================================================
    inline bool
    updatedBool(SimCtx& ctx, BlockId nid, bool newVal)
    {
        return updated(ctx, nid, newVal ? 1.0 : 0.0);
    }
    //=============================================================================
    inline long long
    getInputInt(const SimCtx& ctx, BlockId nid, int portIdx, long long fallback = 0)
    {
        if (!hasInput(ctx, nid, portIdx)) {
            return fallback;
        }
        // Exact-64 source: read the 64-bit lane, never the double projection.
        if (ctx.outputsI64 && ctx.inputSigs && ctx.sigs && nid < (int)ctx.inputSigs->size()) {
            const auto& sIds = (*ctx.inputSigs)[nid];
            if (portIdx < (int)sIds.size() && sIds[portIdx] >= 0
                && sIds[portIdx] < (int)ctx.sigs->size()) {
                const PortSig& ps = (*ctx.sigs)[sIds[portIdx]];
                if (portUsesI64(ps)) {
                    return (*ctx.outputsI64)[ps.offset];
                }
            }
        }
        double v = ctx.outputs[ctx.inputIds[nid][portIdx]];
        return std::isnan(v) ? fallback : static_cast<long long>(std::llround(v));
    }
    //=============================================================================
    // Conventions: boolean 0.0 == false, non-zero == true; integer rounded via std::llround.
    inline double
    getOutputByName(const SimCtx& ctx, BlockId nid, double fallback = 0.0)
    {
        if (nid < 0 || nid >= (int)ctx.outputBase.size()) {
            return fallback;
        }
        double v = ctx.outputs[slotOf(ctx, nid, 0)];
        return std::isnan(v) ? fallback : v;
    }
    //=============================================================================
    inline bool
    getOutputBool(const SimCtx& ctx, BlockId nid, bool fallback = false)
    {
        if (nid < 0 || nid >= (int)ctx.outputBase.size()) {
            return fallback;
        }
        double v = ctx.outputs[slotOf(ctx, nid, 0)];
        return std::isnan(v) ? fallback : (v != 0.0);
    }
    //=============================================================================
    inline long long
    getOutputInt(const SimCtx& ctx, BlockId nid, long long fallback = 0)
    {
        if (nid < 0 || nid >= (int)ctx.outputBase.size()) {
            return fallback;
        }
        double v = ctx.outputs[slotOf(ctx, nid, 0)];
        return std::isnan(v) ? fallback : static_cast<long long>(std::llround(v));
    }
    //=============================================================================
    // Non-inline helpers - defined in SimEngineHelpers.cpp
    //=============================================================================
    NLSNFLOW_BLOCKS_IMPEXP double
    clampVal(double v, double lo, double hi);
    NLSNFLOW_BLOCKS_IMPEXP double
    resolveDouble(
        const json& v, const std::unordered_map<std::string, double>& vars, double fallback = 0.0);
    NLSNFLOW_BLOCKS_IMPEXP double
    param(const Block& b, const std::unordered_map<std::string, double>& vars, const char* key,
        double fallback = 0.0);
    //=============================================================================
    // Transfer function helpers
    //=============================================================================
    NLSNFLOW_BLOCKS_IMPEXP bool
    buildTfState(const json& numJ, const json& denJ, std::vector<double>& A_last_row,
        std::vector<double>& C_vec, double& D, std::vector<double>& initState, int& n,
        const std::unordered_map<std::string, double>& vars = {});
    NLSNFLOW_BLOCKS_IMPEXP double
    tfOutput(
        const std::vector<double>& C, const std::vector<double>& state, double D, double input);
    // Controller-canonical-form derivative of a transfer-function state: fills
    // xdotOut (sized to state) from the companion last-row A and the input. tfRK4 and
    // the DERIVATIVE phase both call it so they cannot drift.
    NLSNFLOW_BLOCKS_IMPEXP void
    tfDerivative(const std::vector<double>& A_last, const std::vector<double>& state, double input,
        std::vector<double>& xdotOut);
    NLSNFLOW_BLOCKS_IMPEXP std::vector<double>
    tfRK4(const std::vector<double>& A_last, const std::vector<double>& state, double input,
        double dt);
    NLSNFLOW_BLOCKS_IMPEXP double
    evalDiscreteTf(const std::vector<double>& num, const std::vector<double>& den,
        const std::vector<double>& xHist, const std::vector<double>& yHist);
    //=============================================================================
    // Topological sort for algebraic blocks (Kahn's algorithm)
    //=============================================================================
    NLSNFLOW_BLOCKS_IMPEXP std::vector<int>
    topoSortAlgebraic(const std::vector<int>& algIndices, const std::vector<Block>& blocks,
        const std::vector<std::vector<int>>& inputIds, const std::vector<int>& outputBase);
    //=============================================================================
    // Strongly-connected-component decomposition of the algebraic dependency graph.
    // One component is a set of algebraic blocks that must be solved together.
    // Components come in topological order, so the engine evaluates acyclic blocks in
    // one pass and iterates only the blocks of a real algebraic loop.
    //=============================================================================
    struct AlgebraicComponent
    {
        std::vector<int> blocks; // indices into the diagram `blocks` vector
        bool cyclic = false; // true iff a real loop (size > 1, or a self-loop)
    };
    //=============================================================================
    NLSNFLOW_BLOCKS_IMPEXP std::vector<AlgebraicComponent>
    computeAlgebraicSCCs(const std::vector<int>& algIndices, const std::vector<Block>& blocks,
        const std::vector<std::vector<int>>& inputIds, const std::vector<int>& outputBase);
    //=============================================================================
    // Signal layout: dimension propagation + slot assignment
    //=============================================================================
    // One resolved wire: (fromNid, fromPort) -> (toNid, toPort).
    struct SignalConn
    {
        BlockId fromNid = INVALID_ID;
        int fromPort = 0;
        BlockId toNid = INVALID_ID;
        int toPort = 0;
    };
    //=============================================================================
    // Result of the compile-time dimension propagation over the diagram graph.
    struct SignalLayout
    {
        std::vector<PortSig> sigs; // [gpi]
        std::vector<int> portBase; // [nid] -> first gpi
        std::vector<std::vector<int>> inputSigs; // [nid][port] -> source gpi (-1)
        int totalSlots = 0;
        bool ok = true;
        std::string error; // user-facing message when !ok
        std::vector<std::string> errorBlocks; // block ids involved in the conflict
        // Lane demand (any port complex / exact-64): the run driver sizes
        // SimCtx::outputsImag / outputsI64 from these, so an all-real, all-double
        // diagram never allocates the sibling lanes.
        bool hasComplex = false;
        bool hasExact64 = false;
    };
    //=============================================================================
    // Fixed-point forward propagation of signal dimensions: scalar expansion, output
    // = element-wise max of inputs, custom per-block rules via
    // BlockMetadata::resolveDims, vector inputs rejected on blocks whose handlers are
    // not width-generalized. Writes the resolved widths back into blocks[i].inWidths
    // / outWidths, then assigns each port its contiguous slot range in the flat
    // outputs buffer. labelSinks/nameToId feed the goto/from dimension short-circuit.
    //
    // seedOutWidths pins the port-0 output width of certain blocks (nid -> width),
    // used for a subsystem's external-input ports whose width is fixed by the outer
    // signal wired to them.
    NLSNFLOW_BLOCKS_IMPEXP SignalLayout
    buildSignalLayout(std::vector<Block>& blocks, const std::vector<SignalConn>& conns,
        const std::unordered_map<std::string, std::string>& labelSinks,
        const std::unordered_map<std::string, BlockId>& nameToId, const ValMap& variables,
        const std::unordered_map<BlockId, int>* seedOutWidths = nullptr,
        BusRegistry* busRegistry = nullptr,
        const std::unordered_map<BlockId, PortSig>* seedOutSigs = nullptr);
    //=============================================================================
    // JSON serialization helpers
    //=============================================================================
    NLSNFLOW_BLOCKS_IMPEXP std::string
    doubleToStr(double v);
    NLSNFLOW_BLOCKS_IMPEXP std::string
    buildOutputJson(const std::string& status, const std::string& reason,
        const std::vector<double>& time, const std::vector<Block>& blocks,
        const std::vector<BlockState>& blockState, const std::vector<std::vector<int>>& inputIds,
        const std::string& extraFields = "");
    // Incremental progress payload, protocol v2.
    // Serializes ONLY the samples in [fromSample, toSample); bounds are
    // inclusive/exclusive indices into `time` and each scope's series, so the
    // cumulative volume of all partials stays O(N) instead of O(N^2):
    //   {"version":2,"sequence":S,"status":"running","progress":p,
    //    "currentTime":t,"fromSample":a,"toSample":b,
    //    "scopes":[{"id","type":"scope","timesDelta":[...],
    //               "seriesDelta":[[...],...],"connected":[...]},
    //              {"id","type":"xyScope","xDelta":[...],"yDelta":[...],...},
    //              {"id","type":"xyzScope","xDelta","yDelta","zDelta",...},
    //              {"id","type":"display","value":<latest>}],
    //    "dashboardUpdates":[...]}
    // toWorkspace blocks are final-result-only and never appear here. Non-finite
    // values follow the final-result policy (NaN -> null, +/-inf -> +/-1e308).
    // A frame with an empty range updates only progress/currentTime.
    // `dashboardPublishedLen` (nid-indexed, resized on demand) tracks how many
    // dashboardScope samples were already published so dashboard streams are
    // also delta-encoded ("delta":true on their update entries); pass nullptr
    // to omit dashboard updates entirely.
    NLSNFLOW_BLOCKS_IMPEXP std::string
    buildProgressJson(long long sequence, double progress, double currentTime, int fromSample,
        int toSample, const std::vector<double>& time, const std::vector<Block>& blocks,
        const std::vector<BlockState>& blockState, const std::vector<std::vector<int>>& inputIds,
        std::vector<std::size_t>* dashboardPublishedLen);
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
