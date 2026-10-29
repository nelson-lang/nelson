//=============================================================================
// Example nflow native block plugin: Cumulative Product (cumprod)
//=============================================================================
// Implements the C ABI defined in BlockPlugin.h.
// The real handler is written against the full Nelson::NFlow types so it has
// access to the SimCtx helpers (getInput, getState, etc.).
//
// Build this file as a shared library alongside library.json by running
// builder.m (it uses Nelson's dlgeneratemake / dlmake build tools).
// The output filename must match the "native" field in library.json:
//   Windows : nflow_cumprod.dll
//   Linux   : nflow_cumprod.so     (the 'lib' prefix is stripped)
//   macOS   : nflow_cumprod.dylib  (the 'lib' prefix is stripped)
//=============================================================================
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "SimEngineTypes.hpp" // SimCtx, Block, Phase, getInput, getState, etc.
#include "BlockPlugin.h" // NfBlockHandlerFn, NfBlockHandlerEntry, etc.
//=============================================================================
using namespace Nelson::NFlow;
//=============================================================================
// Running product of the input samples: y(k) = y(k-1) * u(k), y(0) = u(0).
// INIT resets the accumulator to 1.0; OUTPUT multiplies it by the input and emits.
static bool
handleCumprod(SimCtx& ctx, const Block& b, Phase phase)
{
    BlockState& st = getState(ctx, b.nid);

    if (phase == Phase::INIT) {
        st.scalar = 1.0;
        setOutput(ctx, b.nid, 1.0);
        return false;
    }

    if (phase == Phase::ALGEBRAIC) {
        const double u = getInput(ctx, b.nid, 0, 1.0);
        const double y = st.scalar * u;
        setOutput(ctx, b.nid, y);
        return updated(ctx, b.nid, y);
    }

    if (phase == Phase::UPDATE) {
        const double u = getInput(ctx, b.nid, 0, 1.0);
        st.scalar *= u;
        return false;
    }

    return false;
}
//=============================================================================
// Table of (type-string, handler) pairs returned to the BlockRegistry.
// nfBlockHandlerAdapter (SimEngineTypes.hpp) wraps the typed handler into the
// C ABI int(void*, const void*, int), so the opaque-pointer cast and the
// phase-integer mapping live in the SDK, not in every plugin.
static const NfBlockHandlerEntry kHandlers[] = {
    { "cumprod", &nfBlockHandlerAdapter<handleCumprod> },
};
//=============================================================================
// Phase information as JSON. The BlockRegistry prefers the "phases" array of
// library.json when both exist.
static const char kPhaseInfo[] = R"({"cumprod":["INIT","ALGEBRAIC","UPDATE"]})";
// Code-generation templates: with these, a diagram using this plugin block can
// be exported by nflow_codegenerate to C and Rust exactly like a built-in.
// Placeholders ({id}, {in0}, {state:...}) are expanded by the engine generators.
static const char kCodegenC[] = R"({
  "state": [ { "name": "cumprod", "init": "1.0" } ],
  "step": "out_{id} = {state:cumprod} * {in0};\n{state:cumprod} = out_{id};",
  "inputDefault": "1.0"
})";
static const char kCodegenRust[] = R"({
  "state": [ { "name": "cumprod", "init": "1.0_f64" } ],
  "step": "out_{id} = {state:cumprod} * {in0};\n{state:cumprod} = out_{id};",
  "inputDefault": "1.0_f64"
})";
extern "C"
{

    /**
     * Required export.
     * ABI handshake: the loader refuses plugins whose reported version does
     * not match the engine's NF_PLUGIN_ABI_VERSION (the SimCtx/Block layout).
     */
    NF_PLUGIN_EXPORT int
    nfGetAbiVersion(void)
    {
        return NF_PLUGIN_ABI_VERSION;
    }

    /**
     * Required export.
     * Returns the handler-entry table and sets *count to the number of entries.
     */
    NF_PLUGIN_EXPORT const NfBlockHandlerEntry*
    nfGetBlockHandlers(int* count)
    {
        if (count) {
            *count = static_cast<int>(sizeof(kHandlers) / sizeof(kHandlers[0]));
        }
        return kHandlers;
    }

    /**
     * Optional export.
     * Returns a JSON string mapping block type → phases array.
     * The BlockRegistry falls back to library.json when this symbol is absent.
     */
    NF_PLUGIN_EXPORT const char*
    nfGetPhaseInfo(void)
    {
        return kPhaseInfo;
    }

    /**
     * Optional export.
     * Returns the code-generation template for (type, lang) as JSON, or NULL
     * when the block has no codegen for that language ("c" or "rust").
     */
    NF_PLUGIN_EXPORT const char*
    nfGetBlockCodegen(const char* type, const char* lang)
    {
        if (!type || !lang) {
            return nullptr;
        }
        if (std::string(type) != "cumprod") {
            return nullptr;
        }
        if (std::string(lang) == "c") {
            return kCodegenC;
        }
        if (std::string(lang) == "rust") {
            return kCodegenRust;
        }
        return nullptr;
    }

} // extern "C"
//=============================================================================
