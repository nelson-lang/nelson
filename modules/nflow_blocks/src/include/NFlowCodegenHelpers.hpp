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
#include <set>
#include <string>
#include <unordered_map>
#include <vector>
#include <functional>
#include <nlohmann/json.hpp>
#include <string_view>
#include "nlsNflow_blocks_exports.h"
//=============================================================================
namespace nflow {
using json = nlohmann::json;
using VarMap = std::unordered_map<std::string, double>;
NLSNFLOW_BLOCKS_IMPEXP std::string
sanitizeId(const std::string& id);
NLSNFLOW_BLOCKS_IMPEXP std::string
replaceLatexVars(const std::string& expr);
NLSNFLOW_BLOCKS_IMPEXP std::string
sanitizeExpression(const std::string& expr);
NLSNFLOW_BLOCKS_IMPEXP std::string
replaceWord(const std::string& text, const std::string& word, const std::string& replacement);
NLSNFLOW_BLOCKS_IMPEXP double
evalExpression(const std::string& expr, const VarMap& vars);
// Security gate for code generation: is `expr` (already latex/abs-sanitized)
// safe to emit verbatim into generated C/Rust as a scalar math expression?
// Rejects anything that could break out of `out = <expr>;` - statement
// separators, braces, string/char literals, comments, and the top-level comma
// operator - and requires it to parse as a well-formed math expression via the
// shared ExprEngine. `vars` are the diagram variables (u/t/dt are probed too).
NLSNFLOW_BLOCKS_IMPEXP bool
isSafeUserExpression(const std::string& expr, const VarMap& vars);
NLSNFLOW_BLOCKS_IMPEXP std::string
getLabelName(const json& block);
struct LabelList
{
    std::vector<std::string> list;
    std::unordered_map<std::string, int> map;
};
//=============================================================================
NLSNFLOW_BLOCKS_IMPEXP LabelList
buildUniqueLabelList(const std::vector<json>& blocks);
// True for the blocks that name their own sample time 'SampleTime' and gate
// their own advance on it (zoh_next_<id> and friends), in the runtime and in
// the emitted code alike. They need no schedule from the engine, which is why
// they also work at a slower rate INSIDE a subsystem, where the generic
// multi-rate schedule does not reach.
inline bool
blockSelfSchedulesRate(const std::string& type)
{
    return type == "zoh" || type == "ddelay" || type == "foh" || type == "dtf"
        || type == "dstateSpace";
}
//=============================================================================
NLSNFLOW_BLOCKS_IMPEXP double
resolveNumeric(const json& value, const VarMap& vars);
// True when resolveNumeric found a value rather than falling back to 0.0: a
// number, or text that is a literal, a known variable or an expression over
// them. False says the parameter names nothing.
NLSNFLOW_BLOCKS_IMPEXP bool
resolvesNumeric(const json& value, const VarMap& vars);
NLSNFLOW_BLOCKS_IMPEXP std::string
formatNumber(double value);
NLSNFLOW_BLOCKS_IMPEXP std::string
numToStr(double value);
NLSNFLOW_BLOCKS_IMPEXP std::vector<double>
parseList(const json& value, const VarMap& vars);
// Kahn topological sort over the block graph. `latchedIds` names the
// non-feedthrough stateful blocks whose output does not depend on same-step
// inputs (integrator, unit delay, ...). When the plain sort leaves a cycle, the
// remainder is re-sorted with the latched blocks' outgoing edges dropped, so a
// feedback loop broken at a latched block is ordered correctly (the generators
// pre-load the latched outputs from state).
// Acyclic graphs are returned unchanged. Blocks still unresolved after both
// passes (a genuine algebraic loop, plus every block only reachable through it)
// are appended in diagram order. When `unresolvedCycleIds` is non-null it
// receives those ids, so callers can reject a loop the simulator iterates to a
// fixed point.
NLSNFLOW_BLOCKS_IMPEXP std::vector<std::string>
buildExecutionOrder(const std::vector<json>& blocks, const std::vector<json>& connections,
    const std::vector<json>& extraEdges = {}, const std::set<std::string>& latchedIds = {},
    std::vector<std::string>* unresolvedCycleIds = nullptr);
// True when the generated output of `block` for the current step depends on its
// current-step input. A feedback cycle may only be broken (latched) at a block
// with no real feedthrough: a latched output is emitted from state before its
// input is computed, so latching a feedthrough block reads a stale (zero) input.
//
// Refines the static BlockMetadata.directFeedthrough flag, which is coarse for
// a few stateful blocks whose feedthrough depends on parameters:
//   - tf: feeds through only when not strictly proper (numerator order equal to
//     denominator order, nonzero direct term D). The static flag marks every tf
//     as feedthrough to cover the pure-gain case.
//   - pid: proportional and derivative terms feed through. A pure integral
//     controller (P == 0 and D == 0) reads from state. The static flag marks
//     every pid as non-feedthrough.
//   - stateSpace: feeds through iff the D matrix is nonzero.
// Every other block type uses the static metadata flag.
NLSNFLOW_BLOCKS_IMPEXP bool
blockHasCodegenFeedthrough(const json& block, const VarMap& vars);
struct DiscreteTf
{
    std::vector<double> num;
    std::vector<double> den;
};
//=============================================================================
NLSNFLOW_BLOCKS_IMPEXP DiscreteTf
buildDiscreteTf(const std::vector<double>& numArr, const std::vector<double>& denArr);
struct TustinResult
{
    std::vector<double> b;
    std::vector<double> a;
};
//=============================================================================
NLSNFLOW_BLOCKS_IMPEXP TustinResult
tustinDiscretize(const std::vector<double>& numArr, const std::vector<double>& denArr, double dt);
NLSNFLOW_BLOCKS_IMPEXP bool
buildTfState(const json& numJ, const json& denJ, std::vector<double>& A_last_row,
    std::vector<double>& C_vec, double& D, std::vector<double>& initState, int& n,
    const VarMap& vars);
NLSNFLOW_BLOCKS_IMPEXP double
tfOutput(const std::vector<double>& C, const std::vector<double>& state, double D, double input);
NLSNFLOW_BLOCKS_IMPEXP void
tfDerivative(const std::vector<double>& A_last, const std::vector<double>& state, double input,
    std::vector<double>& xdotOut);
NLSNFLOW_BLOCKS_IMPEXP std::vector<double>
tfRK4(const std::vector<double>& A_last, const std::vector<double>& state, double input, double dt);
NLSNFLOW_BLOCKS_IMPEXP double
evalDiscreteTf(const std::vector<double>& num, const std::vector<double>& den,
    const std::vector<double>& xHist, const std::vector<double>& yHist);
NLSNFLOW_BLOCKS_IMPEXP std::string
doubleToStr(double v);
// Per-port wire reference: source block id + source output port (fromIndex).
// An empty id means "unconnected".
struct CodegenPortRef
{
    std::string id;
    int port = 0;
};
//=============================================================================
// inputs[toBlockId][toIndex] = source (block id, output port)
using CodegenInputsMap = std::unordered_map<std::string, std::vector<CodegenPortRef>>;
// Name of the generated variable holding output `port` of `blockId`:
// "out_<id>" for port 0 (matches the {id} templates),
// "out_<id>_p<k>" for ports k >= 1.
NLSNFLOW_BLOCKS_IMPEXP std::string
codegenOutVar(const std::string& blockId, int port);
NLSNFLOW_BLOCKS_IMPEXP std::string
getInputExpr(const CodegenInputsMap& inputs, const std::string& blockId, int idx,
    const std::string& fallback = "0.0");
NLSNFLOW_BLOCKS_IMPEXP bool
isSimpleExpr(const std::string& expr);
NLSNFLOW_BLOCKS_IMPEXP std::string
wrapExpr(const std::string& expr);
NLSNFLOW_BLOCKS_IMPEXP VarMap
buildVarMap(const json& variables);
// Replace every "{token}" in tpl with resolve(token). Unknown tokens should be
// returned unchanged by the resolver (as "{token}") so typos stay visible.
NLSNFLOW_BLOCKS_IMPEXP std::string
expandBlockTemplate(
    const std::string& tpl, const std::function<std::string(const std::string&)>& resolve);
NLSNFLOW_BLOCKS_IMPEXP std::string
jstr(const json& obj, std::string_view key, std::string_view def = "");
NLSNFLOW_BLOCKS_IMPEXP int
jint(const json& obj, std::string_view key, int def = 0);
NLSNFLOW_BLOCKS_IMPEXP json
jval(const json& obj, std::string_view key);
// Returns the id of the first block whose type cannot be lowered to standalone
// generated code (FMU / FMI Co-Simulation blocks, nelsonFunction blocks which
// run interpreted Nelson code, constraint / modelica blocks, and physicalIsland
// blocks using a component kind outside the embedded-Newton tier), or an empty
// string if the (already-flattened) diagram is fully generatable. When outType
// is non-null it receives the offending block type so callers can tailor their
// message. `outDetail` (when non-null) receives extra context for the rejection
// message (e.g. the joint kind of a planar mechanical island).
NLSNFLOW_BLOCKS_IMPEXP std::string
firstNonGeneratableBlockId(
    const json& diagram, std::string* outType = nullptr, std::string* outDetail = nullptr);
// Returns the id of the first conditional-execution construct that cannot yet be
// lowered to standalone generated code: a leaf if / switchCase / merge block, or
// a subsystem carrying a controlPort (enabled / triggered / action / reset). Runs
// on the ORIGINAL (unflattened) diagram (the flattener drops the control-port
// gate) and recurses into nested subsystem specs. Empty string when none.
NLSNFLOW_BLOCKS_IMPEXP std::string
firstConditionalBlockId(const json& diagram, std::string* outType = nullptr);
// Result of the engine's dimension-propagation pass over a flattened diagram.
// The generators use it to emit array-typed ports and per-element loops.
struct CodegenWidths
{
    // Per block id: resolved element count of each output port (index = port).
    std::unordered_map<std::string, std::vector<int>> outWidths;
    // Per block id: resolved element count of each input port.
    std::unordered_map<std::string, std::vector<int>> inWidths;
    // Per block id: signal type name of each output port ("double", "int8"...).
    std::unordered_map<std::string, std::vector<std::string>> outTypes;
    // Per block id: 2-D shape (rows, cols) of each output / input port. Used by
    // vector code generation to expand the matmul matrix product.
    std::unordered_map<std::string, std::vector<std::pair<int, int>>> outDims;
    std::unordered_map<std::string, std::vector<std::pair<int, int>>> inDims;
    // Full N-D shapes (outDims/inDims above stay the 2-D projection for the
    // matmul expansion): one dims vector per port.
    std::unordered_map<std::string, std::vector<std::vector<int>>> outDimsN;
    std::unordered_map<std::string, std::vector<std::vector<int>>> inDimsN;
    // Per-port complexity flags (char: 0 = real, 1 = complex), parallel to
    // outTypes. Consumed by the complex-lowering pass.
    std::unordered_map<std::string, std::vector<char>> outComplex;
    bool anyComplex = false;
    // Bus analysis: per-port bus ids (-1 = plain), the resolved packed
    // element offset of each busSelector output port, and whether any
    // nonvirtual bus feeds a labelSink (struct emission). Consumed by the
    // vector expansion (bus collapse) and the generators.
    std::unordered_map<std::string, std::vector<int>> outBusIds;
    std::unordered_map<std::string, std::vector<int>> busSelectorOffsets;
    // Per busAssignment id: one (packed offset, member width) region per
    // assigned path; replacement port k+1 overwrites region k of the base
    // bus. Offset -1 marks an unresolved path (skipped, like the runtime).
    std::unordered_map<std::string, std::vector<std::pair<int, int>>> busAssignmentRegions;
    bool anyBus = false;
    bool allScalar = true; // no port wider than 1 anywhere
    bool ok = true; // false when propagation failed (error set)
    std::string error;
};
//=============================================================================
// Build Block structs from the diagram json and run buildSignalLayout. Types
// resolve too. Blocks whose handler is unknown are treated as width-1.
NLSNFLOW_BLOCKS_IMPEXP CodegenWidths
computeCodegenWidths(const json& diagram);
// Complex-lowering pass (code generation): rewrite a diagram carrying
// complex signals into an equivalent all-real diagram where every complex
// wire becomes a re/im pair. Runs BEFORE expandVectorDiagram; a diagram
// without complex ports is returned untouched. On failure returns an object
// with an "error" field.
NLSNFLOW_BLOCKS_IMPEXP json
lowerComplexDiagram(const json& diagram);
// Language-neutral plan rebuilt from the expanded diagram: the vector
// expansion records every label sink fed by a named nonvirtual bus under
// "nonvirtualBusOutputs" (sink tag, bus type name, per-element sink tags in
// packed order), and the model-level "busTypes" declarations survive the
// expansion. The generators emit one struct typedef per referenced bus type
// (dependencies first) and re-group the per-element output fields into a
// single struct-typed field.
struct BusStructMember
{
    std::string name; // sanitized member name
    bool isBus = false;
    std::string busName; // sanitized nested bus type name (isBus)
    std::string typeName; // "double" | "int64" | "uint64" (leaf)
    int width = 1; // packed element count of the member
    std::vector<int> dims; // declared dims (leaf; flattened to width)
};
//=============================================================================
struct BusStructType
{
    std::string name; // sanitized bus type name
    std::vector<BusStructMember> members;
};
//=============================================================================
struct BusStructOutput
{
    std::string name; // sanitized output field name (the sink tag)
    std::string busName; // sanitized bus type name
    // Parallel per packed element: flattened sink tag, member access path
    // (".pos[2]", ".sub.a"; identical syntax in C and Rust) and leaf type.
    std::vector<std::string> elements;
    std::vector<std::string> accesses;
    std::vector<std::string> leafTypes;
};
//=============================================================================
struct BusStructPlan
{
    std::vector<BusStructType> types; // dependencies before dependents
    std::vector<BusStructOutput> outputs;
    // Struct-typed external inputs (bus-typed label sources recorded by the
    // expansion under "nonvirtualBusInputs"); same shape as outputs, types
    // are shared/deduplicated in `types`.
    std::vector<BusStructOutput> inputs;
};
//=============================================================================
NLSNFLOW_BLOCKS_IMPEXP BusStructPlan
buildBusStructPlan(const json& diagram);
// Returns the id of the first block that carries a vector/matrix signal on any
// port (width > 1), or an empty string when the (flattened) diagram is
// all-scalar.
NLSNFLOW_BLOCKS_IMPEXP std::string
firstVectorSignalBlockId(const json& diagram);
// Vector -> scalar diagram expansion for code generation. Each element-wise or
// stateful block becomes one scalar copy per signal element, routing blocks
// collapse into direct element wires, and vector constants / labels split per
// element. An all-scalar input is returned unchanged.
struct VectorExpandResult
{
    json diagram; // the expanded (all-scalar) diagram
    bool ok = true;
    std::string error; // e.g. a dimension-propagation failure from buildSignalLayout
    std::string errorBlockId;
};
//=============================================================================
NLSNFLOW_BLOCKS_IMPEXP VectorExpandResult
expandVectorDiagram(const json& diagram);
// True when `type` has no codegen template of its own but expandVectorDiagram
// lowers it into scalar primitive blocks that the generator emits for `lang`
// ("c" or "rust"): the vector-math reductions (sumElements / productOfElements /
// dotProduct / crossProduct). Used by __nflow_codegen_support__.
NLSNFLOW_BLOCKS_IMPEXP bool
expandsToGeneratableCode(const std::string& type, const std::string& lang);
//=============================================================================
} // namespace nflow
//=============================================================================
