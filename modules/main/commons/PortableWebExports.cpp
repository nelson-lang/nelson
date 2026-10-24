//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdint>
#include <mutex>
#include <string>
#include "WebFigureBridgeExports.hpp"
#include "WebFigureExport.hpp"
//=============================================================================
#ifdef __EMSCRIPTEN__
#include <emscripten/emscripten.h>
#define NELSON_WASM_EXPORT EMSCRIPTEN_KEEPALIVE
#else
#define NELSON_WASM_EXPORT
#endif
//=============================================================================
namespace {
std::mutex figureImageMutex;
std::string figureImage;
//=============================================================================
void
receiveFigureImage(void* context, const char* data, int length)
{
    if (context == nullptr || data == nullptr || length < 0) {
        return;
    }
    static_cast<std::string*>(context)->assign(data, static_cast<size_t>(length));
}
//=============================================================================
} // namespace
//=============================================================================
extern "C" NELSON_WASM_EXPORT const char*
nlsPortableFigurePngBase64(int64_t handle)
{
    std::lock_guard<std::mutex> lock(figureImageMutex);
    figureImage.clear();
    if (!nlsWebGuiRenderFigurePngBase64(handle, receiveFigureImage, &figureImage)) {
        return nullptr;
    }
    return figureImage.c_str();
}
//=============================================================================
extern "C" NELSON_WASM_EXPORT void
nlsPortableClearCurrentFigure()
{
    nlsWebGuiClearCurrentFigure();
}
//=============================================================================
extern "C" NELSON_WASM_EXPORT int
nlsPortableSaveFigure(int64_t handle, const char* path)
{
    return nlsWebGuiSaveFigure(handle, path) ? 0 : 1;
}
//=============================================================================
