//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebFigureRaster.hpp"
#include "WebFigureBridgeExports.hpp"
#include "WebFigureRenderMutex.hpp"
#include "GOFiguresManager.hpp"
#include "GOFigure.hpp"
#include "RenderWeb.hpp"
//=============================================================================
namespace Nelson {
bool
isWebFigureAnnotation(const WebDrawPrimitive& p)
{
    return p.kind == "text" || p.kind == "clip" || p.kind == "clipSave" || p.kind == "clipRestore";
}
//=============================================================================
bool
rasterizeWebFigureBase(
    const std::vector<WebDrawPrimitive>& primitives, int width, int height, std::string& base64)
{
    // The browser has fonts even when the portable rasterizer has none.
    // Draw annotations separately, also avoiding duplicate glyphs on native hosts.
    std::vector<WebDrawPrimitive> base;
    base.reserve(primitives.size());
    for (const auto& primitive : primitives) {
        if (primitive.kind != "text") {
            base.push_back(primitive);
        }
    }
    return rasterizeWebPrimitivesToPngBase64(base, width, height, base64);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
extern "C"
{
    // Render figure `handle` offscreen to a PNG and hand the base64 of it to sink.
    // Qt-free: the display list is rasterized in software (stb) and PNG-encoded,
    // no QImage grab. Returns false if the figure does not exist or rendering
    // failed.
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiRenderFigurePngBase64(int64_t handle, void (*sink)(void*, const char*, int), void* ctx)
    {
        const std::lock_guard<std::recursive_mutex> lock(Nelson::webFigureRenderMutex());
        Nelson::GOFigureHost* win = Nelson::getFigure(static_cast<Nelson::int64>(handle));
        if (win == nullptr) {
            return false;
        }
        Nelson::GOFigure* fig = win->hostFigure();
        if (fig == nullptr) {
            return false;
        }
        const int width = fig->getWidth();
        const int height = fig->getHeight();
        if (width <= 0 || height <= 0) {
            return false;
        }
        Nelson::RenderWeb renderer(0, 0, width, height);
        fig->paintMe(renderer);
        renderer.flush3d();
        std::string base64;
        if (!Nelson::rasterizeWebPrimitivesAndControlsToPngBase64(
                renderer.primitives(), renderer.controls(), width, height, base64)) {
            return false;
        }
        if (sink != nullptr) {
            sink(ctx, base64.data(), static_cast<int>(base64.size()));
        }
        return true;
    }
    //=============================================================================
}
//=============================================================================
