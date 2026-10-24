//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <algorithm>
#include <cstdio>
#include <string>
#include "sink_blocks.hpp"
//=============================================================================
// fileSink: streams its input signal to a CSV file at major steps. INIT
// truncates the file (FileName parameter, default "output.csv") and writes
// the header "t,<id>" (vector inputs: "t,<id>_0,<id>_1,..."); AFTER_STEP
// appends one row per sample. The file is opened and closed per phase so a
// crashed or cancelled run keeps every row written so far. An unwritable
// path is skipped silently (the simulation itself is unaffected).
//
// Code generation lowers the block to a labelSink-shaped model output (a
// column in the generated runner's CSV) instead of an fopen in model.c;
// see the pipeline's fileSink rewrite.
//=============================================================================
namespace {
std::string
fileSinkPath(const Nelson::NFlow::Block& b, const Nelson::NFlow::ValMap& vars)
{
    nflow::BlockDescriptor bd(b, vars);
    std::string p = bd.paramStr("FileName", "");
    if (p.empty()) {
        p = bd.paramStr("Filename", "");
    }
    if (p.empty()) {
        p = "output.csv";
    }
    return p;
}
//=============================================================================
} // namespace
//=============================================================================
bool
Nelson::NFlow::handleFileSink(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::INIT && phase != Phase::AFTER_STEP) {
        return false;
    }
    const std::string path = fileSinkPath(b, ctx.variables);
    SigView v = getInputSig(ctx, b.nid, 0);
    const int w = std::max(1, v.width);
    if (phase == Phase::INIT) {
        FILE* f = fopen(path.c_str(), "wt");
        if (!f) {
            return false;
        }
        fputs("t", f);
        if (w == 1) {
            fprintf(f, ",%s", b.id.c_str());
        } else {
            for (int k = 0; k < w; ++k) {
                fprintf(f, ",%s_%d", b.id.c_str(), k);
            }
        }
        fputs("\n", f);
        fclose(f);
        return false;
    }
    FILE* f = fopen(path.c_str(), "at");
    if (!f) {
        return false;
    }
    fprintf(f, "%.15g", ctx.t);
    for (int k = 0; k < w; ++k) {
        fprintf(f, ",%.15g", sigAt(v, k));
    }
    fputs("\n", f);
    fclose(f);
    return false;
}
//=============================================================================
