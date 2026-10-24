//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "GetFrame.hpp"
#include "GOFigureHost.hpp"
#include "GOAxis.hpp"
#include "GOFigure.hpp"
#include "GOHelpers.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOUIComponent.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GraphicsEnvironment.hpp"
#include "UnitsConversionHelpers.hpp"
#include "RenderWeb.hpp"
#include "NelsonConfiguration.hpp"
#include "NelSon_engine_mode.h"
#include <algorithm>
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
rgbaToArrayOf(const std::vector<unsigned char>& rgba, int width, int height);
//=============================================================================
// A figure with axes is captured at device resolution (Position x DevicePixelRatio), a
// figure without axes at its logical pixel size. GOPolarAxes derives from GOAxis, so it
// is caught here too. The Qt path applies the same split in
// BaseFigureQt::renderFigureToImage.
static bool
figureHasAxes(GOFigure* fig)
{
    if (fig == nullptr) {
        return false;
    }
    auto* children
        = static_cast<GOGObjectsProperty*>(fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (children == nullptr) {
        return false;
    }
    for (int64 handle : children->data()) {
        if (dynamic_cast<GOAxis*>(findGraphicsObject(handle, false)) != nullptr) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static bool
figurePositionPixelSize(GOFigure* fig, int& width, int& height)
{
    if (fig == nullptr) {
        return false;
    }
    std::vector<double> position = fig->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (position.size() < 4) {
        return false;
    }
    int screenWidth = 0;
    int screenHeight = 0;
    GraphicsEnvironment::currentScreenResolution(screenWidth, screenHeight);
    std::vector<double> pixels = convertUnitsVector(
        fig->findStringProperty(GO_UNITS_PROPERTY_NAME_STR), GO_PROPERTY_VALUE_PIXELS_STR,
        position[0], position[1], position[2], position[3], screenWidth, screenHeight);
    if (pixels.size() < 4 || pixels[2] <= 0 || pixels[3] <= 0) {
        return false;
    }
    width = static_cast<int>(std::lround(pixels[2]));
    height = static_cast<int>(std::lround(pixels[3]));
    return width > 0 && height > 0;
}
//=============================================================================
// A hidden figure never realizes its native uicontrol widgets, so a Qt grab is blank and
// unsafe. getframe falls back to the software display list in that case. A UI component
// or uicontrol only lives under a figure or a UI container, so the direct children are
// enough.
static bool
holdsUIControls(GraphicsObject* parent)
{
    if (parent == nullptr) {
        return false;
    }
    auto* children = static_cast<GOGObjectsProperty*>(
        parent->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (children == nullptr) {
        return false;
    }
    for (int64 handle : children->data()) {
        GraphicsObject* child = findGraphicsObject(handle, false);
        if (child == nullptr) {
            continue;
        }
        if (child->getType() == GO_PROPERTY_VALUE_UICONTROL_STR
            || dynamic_cast<GOUIComponent*>(child) != nullptr) {
            return true;
        }
    }
    return false;
}
//=============================================================================
// Captures the frame without Qt: renders the display list with RenderWeb and rasterizes it
// in software (stb). The cdata layout matches QImageToArrayOf.
static bool
webGetFrame(GOFigureHost* f, ArrayOf& result)
{
    GOFigure* fig = f->hostFigure();
    if (fig == nullptr) {
        return false;
    }
    int width = 0;
    int height = 0;
    if (!figurePositionPixelSize(fig, width, height)) {
        return false;
    }
    // A figure with axes is captured at device resolution, like the Qt backend (640x480
    // at 125% yields 800x600). An empty figure keeps its logical pixel size.
    // DevicePixelRatio is set from the native widget, or defaults to the primary screen
    // ratio for a headless web figure.
    double ratio = 1.0;
    if (figureHasAxes(fig)) {
        ratio
            = std::max(1.0, fig->findScalarDoubleProperty(GO_DEVICE_PIXEL_RATIO_PROPERTY_NAME_STR));
    }
    // Record at the logical size, then scale the whole display list to the device ratio so
    // axes, text, line widths and uicontrols scale together.
    const int layoutWidth = width;
    const int layoutHeight = height;
    width = static_cast<int>(std::lround(layoutWidth * ratio));
    height = static_cast<int>(std::lround(layoutHeight * ratio));
    const int previousWidth = fig->getWidth();
    const int previousHeight = fig->getHeight();
    const double previousScale = fig->getRenderScale();
    const bool previousStale = fig->childrenLayoutStale();
    fig->setRenderingSize(layoutWidth, layoutHeight, 1.0);
    RenderWeb renderer(0, 0, layoutWidth, layoutHeight);
    fig->paintMe(renderer);
    renderer.flush3d();
    fig->setRenderingSize(previousWidth, previousHeight, previousScale);
    fig->setChildrenLayoutStale(previousStale);
    std::vector<unsigned char> rgb;
    if (!rasterizeWebPrimitivesAndControls(scalePrimitivesToSurface(renderer.primitives(), ratio),
            scaleControlsToSurface(renderer.controls(), ratio), width, height, rgb)) {
        return false;
    }
    Dimensions dims(std::vector<indexType> {
        static_cast<indexType>(height), static_cast<indexType>(width), static_cast<indexType>(3) });
    uint8* ptr = static_cast<uint8*>(ArrayOf::allocateArrayOf(NLS_UINT8, dims.elementCount()));
    result = ArrayOf(NLS_UINT8, dims, ptr);
    const int nbElements = height * width;
    OMP_PARALLEL_FOR_LOOP(nbElements)
    for (int index = 0; index < nbElements; index++) {
        const int row = index % height;
        const int col = index / height;
        const unsigned char* px = &rgb[(static_cast<size_t>(row) * width + col) * 3];
        ptr[index] = px[0];
        ptr[index + nbElements] = px[1];
        ptr[index + 2 * nbElements] = px[2];
    }
    return true;
}
//=============================================================================
ArrayOf
GetFrame(GOFigureHost* f, bool rawOpenGL)
{

    if (!f) {
        return ArrayOf();
    }
    if (GOFigure* fig = f->hostFigure(); fig != nullptr) {
        fig->updateState();
    }
    if (f->isHeadlessHost()) {
        ArrayOf result;
        if (webGetFrame(f, result)) {
            return result;
        }
        // fall through to the native grab if the software path failed
    }
    // A hidden Qt figure with uicontrols has unrealized widgets: use the software path.
    if (GOFigure* fig = f->hostFigure(); fig != nullptr
        && fig->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)
        && holdsUIControls(fig)) {
        ArrayOf result;
        if (webGetFrame(f, result)) {
            return result;
        }
        // fall through to the native grab if the software path failed
    }
    std::vector<unsigned char> rgba;
    int width = 0;
    int height = 0;
    if (!f->grabHostFrameRGBA(rawOpenGL, rgba, width, height)) {
        return ArrayOf();
    }
    return rgbaToArrayOf(rgba, width, height);
}
//=============================================================================
ArrayOf
rgbaToArrayOf(const std::vector<unsigned char>& rgba, int width, int height)
{
    std::vector<indexType> dimsAsVector;
    dimsAsVector.reserve(3);
    dimsAsVector.push_back(height);
    dimsAsVector.push_back(width);
    dimsAsVector.push_back(3);
    Dimensions dimsA(dimsAsVector);
    uint8* ptrA = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, dimsA.elementCount());
    ArrayOf A = ArrayOf(NLS_UINT8, dimsA, ptrA);

    int nbElements = height * width;
    OMP_PARALLEL_FOR_LOOP(nbElements)
    for (int index = 0; index < nbElements; index++) {
        int row = index % height;
        int col = index / height;
        const unsigned char* px = rgba.data() + ((size_t)row * width + col) * 4;
        ptrA[index] = px[0];
        ptrA[index + height * width] = px[1];
        ptrA[index + 2 * height * width] = px[2];
    }
    return A;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
