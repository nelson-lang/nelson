//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebFigureAxesMetadata.hpp"
#include "GOAxis.hpp"
#include "GOFigure.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    // True when the first `count` doubles from `values` are not all equal. A
    // plot whose z coordinates are constant lies in a single plane, so it is 2D
    // (e.g. area/fill draw a flat patch with a constant z) and not rotatable.
    bool
    valuesVary(const double* values, indexType count)
    {
        for (indexType i = 1; i < count; i++) {
            if (values[i] != values[0]) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    bool
    has3dData(GraphicsObject* object)
    {
        if (object->stringCheck(GO_TAG_PROPERTY_NAME_STR, L"__web_floor_projection__")) {
            return false;
        }
        if (auto* zs = object->findProperty(GO_Z_DATA_PROPERTY_NAME_STR, false)) {
            ArrayOf data = zs->get();
            if (!data.isEmpty()) {
                data.promoteType(NLS_DOUBLE);
                if (valuesVary(static_cast<const double*>(data.dataPointer()),
                        data.rows() * data.columns())) {
                    return true;
                }
            }
        }
        if (auto* vertices = object->findProperty(GO_VERTICES_PROPERTY_NAME_STR, false)) {
            ArrayOf data = vertices->get();
            if (!data.isEmpty() && data.columns() == 3) {
                data.promoteType(NLS_DOUBLE);
                const indexType rows = data.rows();
                const double* z = static_cast<const double*>(data.dataPointer())
                    + static_cast<size_t>(2) * rows;
                if (valuesVary(z, rows)) {
                    return true;
                }
            }
        }
        auto* children = dynamic_cast<GOGObjectsProperty*>(
            object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
        if (children) {
            for (auto handle : children->data()) {
                auto* child = findGraphicsObject(handle, false);
                if (child && has3dData(child)) {
                    return true;
                }
            }
        }
        return false;
    }
    //=============================================================================
    void
    collectAxes(GraphicsObject* object, int width, int height, bool highlight, nlohmann::json& axes)
    {
        if (auto* axis = dynamic_cast<GOAxis*>(object)) {
            const auto box = axis->getClientAreaAsPixels();
            if (box.size() == 4 && width > 0 && height > 0) {
                double azimuth = 0, elevation = 0;
                axis->getView(azimuth, elevation);
                nlohmann::json entry = { { "handle", findGraphicsObjectHandle(axis) },
                    { "is3d", !axis->is2D() || has3dData(axis) },
                    { "rect",
                        { box[0] / width, 1.0 - (box[1] + box[3]) / height, box[2] / width,
                            box[3] / height } },
                    { "view", { azimuth, elevation } } };
                // SelectionHighlight 'off' on the axes or its figure hides the hover frame.
                if (!highlight || !highlightsSelection(axis)) {
                    entry["highlight"] = false;
                }
                // disableDefaultInteractivity turns off the modeless drag-pan/rotate and wheel
                // zoom.
                if (axis->stringCheck(GO_DEFAULT_INTERACTIVITY_INTERNAL_PROPERTY_NAME_STR,
                        GO_PROPERTY_VALUE_OFF_STR)) {
                    entry["interactive"] = false;
                }
                axes.push_back(std::move(entry));
            }
            return;
        }
        auto* children = dynamic_cast<GOGObjectsProperty*>(
            object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
        if (children) {
            for (auto handle : children->data()) {
                if (auto* child = findGraphicsObject(handle, false)) {
                    collectAxes(child, width, height, highlight, axes);
                }
            }
        }
    }
    //=============================================================================
    // True when the figure handles mouse presses, motion or releases itself.
    bool
    figureOwnsPointer(GOFigure* figure)
    {
        for (const wchar_t* name : { GO_WINDOW_BUTTON_DOWN_FCN_PROPERTY_NAME_STR,
                 GO_WINDOW_BUTTON_MOTION_FCN_PROPERTY_NAME_STR,
                 GO_WINDOW_BUTTON_UP_FCN_PROPERTY_NAME_STR }) {
            auto* callback = figure->findProperty(name, false);
            if (callback && !callback->get().isEmpty()) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
} // namespace
//=============================================================================
void
appendWebFigureAxesMetadata(GOFigure* figure, int width, int height, nlohmann::json& result)
{
    nlohmann::json axes = nlohmann::json::array();
    collectAxes(figure, width, height, highlightsSelection(figure), axes);
    // The figure is 3D when any of its axes is. An axes is 3D when it is not seen top-down
    // (is2D() false) or carries 3D vertex/z data (has3dData), the same test collectAxes stores per
    // axes. The render backend's hasScene3d() is not used: it flags a flat patch (fill) as 3D
    // because it is drawn through the WebGL patch path.
    bool is3d = false;
    for (const auto& axis : axes) {
        is3d = is3d || axis["is3d"].get<bool>();
    }
    result["is3d"] = is3d;
    // A figure with WindowButton*Fcn owns the pointer: no hover indicator, pick or drag gestures.
    if (!axes.empty() && !figureOwnsPointer(figure)) {
        result["view"] = axes[0]["view"];
        result["rotatableAxes"] = std::move(axes);
    }
    // ToolBar 'none' hides the navigation tools and the details bar, as the native window does.
    if (figure->stringCheck(GO_TOOL_BAR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)) {
        result["toolbarHidden"] = true;
    }
    // A figure that defines WindowScrollWheelFcn owns the wheel: the web panel
    // keeps its built-in wheel zoom off, as the native window does.
    auto* callback = figure->findProperty(GO_WINDOW_SCROLL_WHEEL_FCN_PROPERTY_NAME_STR, false);
    if (callback && !callback->get().isEmpty()) {
        result["hasScrollWheelCallback"] = true;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
