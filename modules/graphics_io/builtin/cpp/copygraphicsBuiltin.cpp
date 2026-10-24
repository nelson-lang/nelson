//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "copygraphicsBuiltin.hpp"
#include "GraphicsClipboard.hpp"
#include "GOFigureHost.hpp"
#include "GOFigure.hpp"
#include "GOHelpers.hpp"
#include "RenderWeb.hpp"
#include "NelsonConfiguration.hpp"
#include "WebDesktopBridge.hpp"
#include "NelSon_engine_mode.h"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsIoGateway {
//=============================================================================
ArrayOfVector
copygraphicsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    GOFigureHost* host = findGOWindows(argIn[0].asGraphicsObjectScalar());
    if (!host || !host->hostFigure()) {
        Error(L"Nelson:copygraphics:invalidFigure", L"Invalid handle.");
    }
    GOFigure* figure = host->hostFigure();
    figure->setRenderingStateInvalid(true);
    const auto mode = NelsonConfiguration::getInstance()->getNelsonEngineMode();
    if (mode == WEB_GUI || mode == WASM_TERMINAL) {
        const int width = figure->getWidth();
        const int height = figure->getHeight();
        RenderWeb renderer(0, 0, width, height);
        figure->paintMe(renderer);
        renderer.flush3d();
        std::string pngBase64;
        if (!rasterizeWebPrimitivesToPngBase64(renderer.primitives(), width, height, pngBase64)
            || !WebDesktopBridge::getInstance()->pushWebClipboardImage(pngBase64)) {
            Error(L"Nelson:copygraphics:copyFailed", L"Cannot copy the figure to the clipboard.");
        }
        return {};
    }
    if (!copyGraphicsToNativeClipboard(host)) {
        Error(L"Nelson:copygraphics:copyFailed", L"Cannot copy the figure to the clipboard.");
    }
    return {};
}
//=============================================================================
} // namespace Nelson::GraphicsIoGateway
//=============================================================================
