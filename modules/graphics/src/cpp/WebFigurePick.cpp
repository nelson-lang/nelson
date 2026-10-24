//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "WebFigureBridgeExports.hpp"
#include "GOFigureMouseEvents.hpp"
#include "GOFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOFigureHost.hpp"
#include "GOAxis.hpp"
#include "GOHelpers.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "RenderWeb.hpp"
#include "WebFigureRenderMutex.hpp"
#include <nlohmann/json.hpp>
#include <cmath>
#include <limits>
//=============================================================================
namespace {
std::vector<double>
numericData(Nelson::GraphicsObject* object, const std::wstring& name)
{
    auto* property = object->findProperty(name, false);
    if (property == nullptr) {
        return {};
    }
    Nelson::ArrayOf value = property->get();
    if (!value.isNumeric() || value.isComplex()) {
        return {};
    }
    value.promoteType(Nelson::NLS_DOUBLE);
    const auto* data = static_cast<const double*>(value.dataPointer());
    return value.isEmpty() ? std::vector<double>()
                           : std::vector<double>(data, data + value.elementCount());
}
//=============================================================================
Nelson::GOAxis*
parentAxes(Nelson::GraphicsObject* object)
{
    while (object != nullptr) {
        if (auto* axis = dynamic_cast<Nelson::GOAxis*>(object)) {
            return axis;
        }
        auto* property = dynamic_cast<Nelson::GOGObjectsProperty*>(
            object->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
        const auto parents = property ? property->data() : std::vector<Nelson::int64>();
        object = parents.empty() ? nullptr : Nelson::findGraphicsObject(parents[0], false);
    }
    return nullptr;
}
//=============================================================================
nlohmann::json
pickPoint(
    Nelson::GOFigure* figure, double x, double y, Nelson::int64 selected = 0, double sample = 0)
{
    const auto picked = selected > 0 ? selected : Nelson::pickObjectAtPixel(figure, x, y);
    auto* object = Nelson::findGraphicsObject(picked, false);
    auto* axis = parentAxes(object);
    nlohmann::json result = { { "found", false } };
    if (object == nullptr || axis == nullptr || axis->getParentFigure() != figure
        || object->findStringProperty(GO_VISIBLE_PROPERTY_NAME_STR) != L"on") {
        return result;
    }
    result["objectHandle"] = picked;
    result["axesHandle"] = Nelson::findGraphicsObjectHandle(axis);
    auto xs = numericData(object, GO_X_DATA_PROPERTY_NAME_STR);
    auto ys = numericData(object, GO_Y_DATA_PROPERTY_NAME_STR);
    auto zs = numericData(object, GO_Z_DATA_PROPERTY_NAME_STR);
    if (xs.empty() || ys.empty()) {
        return result;
    }
    if (!zs.empty() && xs.size() * ys.size() == zs.size()
        && (xs.size() != zs.size() || ys.size() != zs.size())) {
        const auto gridX = xs;
        const auto gridY = ys;
        xs.resize(zs.size());
        ys.resize(zs.size());
        for (size_t i = 0; i < zs.size(); ++i) {
            xs[i] = gridX[i / gridY.size()];
            ys[i] = gridY[i % gridY.size()];
        }
    }
    if (xs.size() != ys.size()) {
        return result;
    }
    if (zs.size() != xs.size()) {
        zs.assign(xs.size(), 0.0);
    }
    Nelson::RenderWeb renderer(0, 0, figure->getWidth(), figure->getHeight(), false);
    axis->setupProjectionOnly(renderer, false);
    std::vector<double> mx, my, mz;
    axis->reMap(xs, ys, zs, mx, my, mz);
    double best = std::numeric_limits<double>::infinity();
    size_t index = 0;
    const double pixelX = x * figure->getRenderScale();
    const double pixelY = y * figure->getRenderScale();
    for (size_t i = 0; i < mx.size(); ++i) {
        if (selected > 0
            && i != static_cast<size_t>(std::clamp(sample, 1.0, double(mx.size())) - 1)) {
            continue;
        }
        double px = 0, py = 0, depth = 0;
        bool clipped = false;
        renderer.toPixels(mx[i], my[i], mz[i], px, py, depth, clipped);
        const double distance = std::hypot(px - pixelX, py - pixelY);
        if (!clipped && std::isfinite(distance) && distance < best) {
            best = distance;
            index = i;
        }
    }
    if (std::isfinite(best)) {
        result["found"] = true;
        result["index"] = index + 1;
        result["count"] = xs.size();
        result["position"] = { xs[index], ys[index], zs[index] };
        double px = 0, py = 0, depth = 0;
        bool clipped = false;
        renderer.toPixels(mx[index], my[index], mz[index], px, py, depth, clipped);
        result["pixel"] = { px, py };
    }
    return result;
}
//=============================================================================
} // namespace
//=============================================================================
extern "C" NLSGRAPHICS_IMPEXP bool
nlsWebGuiInspectDataPoint(int64_t handle, int64_t object, double sample,
    void (*sink)(void*, const char*, int), void* context)
{
    const std::lock_guard<std::recursive_mutex> lock(Nelson::webFigureRenderMutex());
    auto* host = Nelson::getFigure(handle);
    auto* figure = host ? host->hostFigure() : nullptr;
    if (!figure || !std::isfinite(sample)) {
        return false;
    }
    const std::string result = pickPoint(figure, 0, 0, object, std::round(sample)).dump();
    sink(context, result.data(), static_cast<int>(result.size()));
    return true;
}
//=============================================================================
extern "C" NLSGRAPHICS_IMPEXP bool
nlsWebGuiPickDataPoint(
    int64_t handle, double x, double y, void (*sink)(void*, const char*, int), void* context)
{
    const std::lock_guard<std::recursive_mutex> lock(Nelson::webFigureRenderMutex());
    auto* host = Nelson::getFigure(handle);
    auto* figure = host ? host->hostFigure() : nullptr;
    if (figure == nullptr || !std::isfinite(x) || !std::isfinite(y)) {
        return false;
    }
    const std::string result = pickPoint(figure, x, y).dump();
    sink(context, result.data(), static_cast<int>(result.size()));
    return true;
}
//=============================================================================
