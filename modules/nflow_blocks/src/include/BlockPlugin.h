//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifndef NFLOW_BLOCK_PLUGIN_H
#define NFLOW_BLOCK_PLUGIN_H
//=============================================================================
// C ABI for external block library plugins.
// A plugin is a shared library (.dll / .so / .dylib) placed in a library
// directory alongside a library.json.  It must export at least
// nfGetBlockHandlers(); nfGetPhaseInfo() is optional when the manifest
// already carries phase data.
//=============================================================================
// Export decoration for the plugin's own symbols. MSVC exports nothing by
// default from a DLL, so plugin authors must mark the nfGet* entry points:
//   NF_PLUGIN_EXPORT const NfBlockHandlerEntry* nfGetBlockHandlers(int* count);
#if defined(_WIN32)
#define NF_PLUGIN_EXPORT __declspec(dllexport)
#else
#define NF_PLUGIN_EXPORT __attribute__((visibility("default")))
#endif
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
    // ABI version handshake.
    //
    // The opaque ctx/block pointers below are real SimCtx / Block addresses:
    // a plugin compiled against SimEngineTypes.hpp reads those structs
    // directly, so their memory layout is the ABI. Any layout change to
    // SimCtx / Block / BlockState bumps NF_PLUGIN_ABI_VERSION. The loader
    // refuses plugins whose nfGetAbiVersion() does not match or is absent.
    //
    // Plugins must export:
    //   NF_PLUGIN_EXPORT int nfGetAbiVersion(void) { return NF_PLUGIN_ABI_VERSION; }
    //=============================================================================
#define NF_PLUGIN_ABI_VERSION 4
    typedef int (*NfGetAbiVersionFn)(void);
    //=============================================================================
    // Forward-declare the handler signature as an opaque function pointer.
    // The actual prototype is:
    //   bool handler(SimCtx&, const Block&, Phase);
    // Plugins compiled against SimEngineTypes.hpp know the real signature.
    //=============================================================================
    typedef int (*NfBlockHandlerFn)(void* ctx, const void* block, int phase);
    //=============================================================================
    typedef struct NfBlockHandlerEntry
    {
        const char* type; // block type string (e.g. "myCustomBlock")
        NfBlockHandlerFn handler;
    } NfBlockHandlerEntry;
    //=============================================================================
    // Required export: return an array of handler entries.
    // *count receives the number of entries.
    typedef const NfBlockHandlerEntry* (*NfGetBlockHandlersFn)(int* count);
    //=============================================================================
    // Optional export: return a JSON string mapping type --> phases array.
    // Example: {"myBlock":["init","output","update"]}
    // If absent, the phases from library.json are used.
    typedef const char* (*NfGetPhaseInfoFn)(void);
    //=============================================================================
    // Optional export: return the code-generation template for (type, lang) as a
    // JSON string, or NULL when the block has no codegen for that language.
    // lang is "c" or "rust". Template schema:
    //   { "state": [ { "name": "acc", "init": "1.0" } ],   // optional state vars
    //     "step": "out_{id} = {state:acc} * {in0};\n{state:acc} = out_{id};",
    //     "inputDefault": "1.0" }                           // optional
    // Placeholders ({id}, {inN}, {inNw}, {param:name[:default]},
    // {state:name}, {dt}, {t}) are expanded by the nflow_engine generators, so a
    // plugin block participates in `nflow_codegenerate` exactly like a built-in.
    typedef const char* (*NfGetBlockCodegenFn)(const char* type, const char* lang);
    //=============================================================================
    // Programmatic code generation (optional, complements nfGetBlockCodegen).
    //
    // A plugin that needs real logic in its generated code (loops, parameter
    // conditionals, per-port branching) exports nfEmitBlockCodegen instead of a
    // static JSON template. The engine calls it once per code-generation pass
    // with an NfCodegenContext whose callbacks bridge to the same machinery the
    // built-in generators use, so a plugin block emits C/Rust exactly like a
    // built-in. Nothing C++ or third-party crosses this boundary: the context
    // is pure C function pointers and the params are read through typed getters.
    //
    // The context is handed to the plugin twice per language: once in the state
    // pass (phase == NF_CODEGEN_PHASE_STATE) where addState takes effect, and
    // once in the step pass (phase == NF_CODEGEN_PHASE_STEP) where line takes
    // effect. The simplest plugin ignores phase and always calls both addState
    // (to declare its state) and line (to emit its step body); the engine routes
    // each call to the matching pass and discards the rest. A plugin may read
    // ctx->phase to skip work it does not need in a given pass.
    //
    // String lifetime: every const char* a callback returns is owned by the
    // engine and valid only for the duration of the current nfEmitBlockCodegen
    // call. Copy it if it must outlive the call.
    //=============================================================================
#define NF_CODEGEN_PHASE_STATE 0 // state-declaration pass: addState is live
#define NF_CODEGEN_PHASE_STEP 1 // step-emission pass: line is live
    typedef struct NfCodegenContext
    {
        void* self; // opaque engine handle, pass it back to every callback
        int phase; // NF_CODEGEN_PHASE_STATE or NF_CODEGEN_PHASE_STEP

        // --- emission ---
        // Emit one line of the step body (engine adds indentation). Step pass.
        void (*line)(void* self, const char* text);
        // Declare a scalar state field. name is a logical name; the engine
        // appends the per-instance id suffix, so "acc" becomes acc_<id>, the
        // same field stateRef("acc") resolves to. init is the initial-value
        // expression (use fmt for a numeric literal), type the field type
        // ("" = language default double/f64). State pass.
        void (*addState)(void* self, const char* name, const char* init, const char* type);

        // --- typed parameter access (replaces reading the block JSON) ---
        // Numeric parameter value, with diagram-variable resolution; fallback
        // when the key is absent or not numeric.
        double (*paramNumber)(void* self, const char* key, double fallback);
        // String parameter value; fallback when absent or not a string.
        const char* (*paramString)(void* self, const char* key, const char* fallback);
        // Boolean parameter value (accepts JSON true/false, nonzero number, or
        // the strings "true"/"1"); fallback when absent.
        int (*paramBool)(void* self, const char* key, int fallback);
        // 1 when the parameter key is present (and not null), 0 otherwise.
        int (*hasParam)(void* self, const char* key);
        // Numeric list parameter: writes up to maxOut values into out and
        // returns the total count available (may exceed maxOut). Pass out == NULL
        // / maxOut == 0 to query the count only.
        int (*paramList)(void* self, const char* key, double* out, int maxOut);

        // --- context ---
        // Input expression wired to port (already parenthesized for safe reuse);
        // the unconnected-input default when nothing is wired. "" in the state
        // pass (inputs are not resolved there).
        const char* (*inputExpr)(void* self, int port);
        // Sanitized block id.
        const char* (*blockId)(void* self);
        // The lvalue the block writes its output to (out_<id>).
        const char* (*outputVar)(void* self);
        // Accessor expression for a state field declared via addState
        // (s->name_<id> in C, s.name_<id> in Rust).
        const char* (*stateRef)(void* self, const char* name);
        // Format a double as a numeric literal for the target language
        // (e.g. 1.0 in C, 1.0_f64 in Rust).
        const char* (*fmt)(void* self, double v);
        // Diagram sample time.
        double (*dt)(void* self);
        // Resolved signal type name of the output port ("double", "int64", ...).
        // "" in the state pass.
        const char* (*outType)(void* self);
        // Resolved signal type name of an input port. "" in the state pass.
        const char* (*inType)(void* self, int port);
    } NfCodegenContext;
    //=============================================================================
    // Optional export: emit code for (type, lang) through ctx. Returns 1 when
    // the block emits for this (type, lang), 0 when it has no codegen for it
    // (the engine then falls back to nfGetBlockCodegen, if any). lang is "c" or
    // "rust". When both nfEmitBlockCodegen and nfGetBlockCodegen are present for
    // the same (type, lang), the programmatic emitter wins.
    typedef int (*NfEmitBlockCodegenFn)(const char* type, const char* lang, NfCodegenContext* ctx);
//=============================================================================
// Symbol names to load via dlsym / GetProcAddress
#define NF_GET_ABI_VERSION_SYMBOL "nfGetAbiVersion"
#define NF_GET_BLOCK_HANDLERS_SYMBOL "nfGetBlockHandlers"
#define NF_GET_PHASE_INFO_SYMBOL "nfGetPhaseInfo"
#define NF_GET_BLOCK_CODEGEN_SYMBOL "nfGetBlockCodegen"
#define NF_EMIT_BLOCK_CODEGEN_SYMBOL "nfEmitBlockCodegen"
//=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
#endif // NFLOW_BLOCK_PLUGIN_H
//=============================================================================
