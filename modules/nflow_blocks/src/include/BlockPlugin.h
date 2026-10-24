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
// Symbol names to load via dlsym / GetProcAddress
#define NF_GET_ABI_VERSION_SYMBOL "nfGetAbiVersion"
#define NF_GET_BLOCK_HANDLERS_SYMBOL "nfGetBlockHandlers"
#define NF_GET_PHASE_INFO_SYMBOL "nfGetPhaseInfo"
#define NF_GET_BLOCK_CODEGEN_SYMBOL "nfGetBlockCodegen"
//=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
#endif // NFLOW_BLOCK_PLUGIN_H
//=============================================================================
