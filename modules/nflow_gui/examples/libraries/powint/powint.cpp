//=============================================================================
// Example nflow native block plugin: Integer Power (powint)
//=============================================================================
// Computes y = u^n for a non-negative integer parameter n.
//
// This example exists to show PROGRAMMATIC code generation: nfEmitBlockCodegen
// emits an unrolled product whose length depends on the n parameter. A
// declarative JSON template (see the cumprod example) cannot vary the number of
// factors, so this is the case the C-ABI codegen API is made for.
//
// Build this file as a shared library alongside library.json by running
// builder.m. The output filename must match the "native" field in library.json
// (nflow_powint.dll / .so / .dylib).
//=============================================================================
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "SimEngineTypes.hpp" // SimCtx, Block, Phase, getInput, setOutput, param, ...
#include "BlockPlugin.h" // NfBlockHandlerEntry, NfCodegenContext, ...
#include <string>
//=============================================================================
using namespace Nelson::NFlow;
//=============================================================================
// y = u^n. n is a non-negative integer parameter (default 2); a negative n is
// treated as 0, so y = 1. The block is stateless and feedthrough, so it only
// acts in the ALGEBRAIC phase.
static bool
handlePowint(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase == Phase::ALGEBRAIC) {
        const double u = getInput(ctx, b.nid, 0, 0.0);
        int n = static_cast<int>(param(b, ctx.variables, "n", 2.0));
        if (n < 0) {
            n = 0;
        }
        double y = 1.0;
        for (int i = 0; i < n; ++i) {
            y *= u;
        }
        setOutput(ctx, b.nid, y);
        return updated(ctx, b.nid, y);
    }
    return false;
}
//=============================================================================
// Handler table. nfBlockHandlerAdapter (SimEngineTypes.hpp) wraps the typed
// handler into the C ABI, so no per-plugin phase table or cast is needed.
static const NfBlockHandlerEntry kHandlers[] = {
    { "powint", &nfBlockHandlerAdapter<handlePowint> },
};
//=============================================================================
static const char kPhaseInfo[] = R"({"powint":["ALGEBRAIC"]})";
//=============================================================================
extern "C"
{

    /** Required export: ABI handshake. */
    NF_PLUGIN_EXPORT int
    nfGetAbiVersion(void)
    {
        return NF_PLUGIN_ABI_VERSION;
    }

    /** Required export: handler table. */
    NF_PLUGIN_EXPORT const NfBlockHandlerEntry*
    nfGetBlockHandlers(int* count)
    {
        if (count) {
            *count = static_cast<int>(sizeof(kHandlers) / sizeof(kHandlers[0]));
        }
        return kHandlers;
    }

    /** Optional export: phases (also available from library.json). */
    NF_PLUGIN_EXPORT const char*
    nfGetPhaseInfo(void)
    {
        return kPhaseInfo;
    }

    /**
     * Optional export: PROGRAMMATIC code generation.
     *
     * Emits y = u^n as an unrolled product of n factors. The number of factors
     * comes from the n parameter, read at generation time through the typed
     * paramNumber callback, so the emitted code is shaped by the parameter -
     * something a declarative template cannot do. Everything crosses the
     * boundary as pure C: no C++ type, no JSON.
     *
     * Returns 1 when it emits for (type, lang), 0 otherwise. The block is
     * stateless, so it emits only in the step pass (addState is unused).
     */
    NF_PLUGIN_EXPORT int
    nfEmitBlockCodegen(const char* type, const char* lang, NfCodegenContext* ctx)
    {
        if (!type || !lang || !ctx) {
            return 0;
        }
        if (std::string(type) != "powint") {
            return 0;
        }
        int n = static_cast<int>(ctx->paramNumber(ctx->self, "n", 2.0));
        if (n < 0) {
            n = 0;
        }
        const std::string out = ctx->outputVar(ctx->self);
        if (n == 0) {
            // u^0 = 1.
            ctx->line(ctx->self, (out + " = " + ctx->fmt(ctx->self, 1.0) + ";").c_str());
            return 1;
        }
        // out_<id> = (u) * (u) * ... ;  with n factors (inputExpr is parenthesized).
        const std::string u = ctx->inputExpr(ctx->self, 0);
        std::string expr = u;
        for (int i = 1; i < n; ++i) {
            expr += " * ";
            expr += u;
        }
        ctx->line(ctx->self, (out + " = " + expr + ";").c_str());
        return 1;
    }

} // extern "C"
//=============================================================================
