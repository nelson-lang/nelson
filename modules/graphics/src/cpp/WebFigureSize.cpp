//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebFigureBridgeExports.hpp"
#include "GOAxis.hpp"
#include "GOFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOCallbackProperty.hpp"
//=============================================================================
namespace {
void
invalidateAxesLayout(Nelson::GraphicsObject* object)
{
    if (auto* axis = dynamic_cast<Nelson::GOAxis*>(object)) {
        axis->markStateDirty();
        axis->invalidateViewState();
        return;
    }
    auto* children = dynamic_cast<Nelson::GOGObjectsProperty*>(
        object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return;
    }
    for (auto handle : children->data()) {
        if (auto* child = Nelson::findGraphicsObject(handle, false)) {
            invalidateAxesLayout(child);
        }
    }
}
//=============================================================================
} // namespace
//=============================================================================
extern "C" NLSGRAPHICS_IMPEXP bool
nlsWebGuiSetFigureSize(int64_t handle, int width, int height)
{
    if (width <= 0 || height <= 0) {
        return false;
    }
    auto* host = Nelson::getFigure(static_cast<Nelson::int64>(handle));
    auto* figure = host ? host->hostFigure() : nullptr;
    if (!figure) {
        return false;
    }
    if (figure->getWidth() == width && figure->getHeight() == height
        && figure->getRenderScale() == 1.0) {
        return true;
    }
    // Keep the script's Position, but recompute cameras, ticks and label layout
    // for the new drawing area before the first frame. Otherwise Fit performs
    // this delayed recompute and appears to move unrelated subplot axes.
    figure->setRenderingSize(width, height, 1.0);
    invalidateAxesLayout(figure);
    // setRenderingSize flagged the children layout stale: re-derive the auto axes
    // Positions (title, label and tick insets) for the new drawing area now.
    figure->refreshChildrenLayout();
    figure->setRenderingStateInvalid(true);
    auto* callback = dynamic_cast<Nelson::GOCallbackProperty*>(
        figure->findProperty(GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR, false));
    if (callback && !callback->get().isEmpty()) {
        Nelson::ArrayOfVector values;
        values << Nelson::ArrayOf::doubleScalar(width) << Nelson::ArrayOf::doubleScalar(height);
        // Queued like every other figure callback: it runs at the next
        // drawnow()/pause() (or __web_flush_callbacks__ for an idle-time RPC),
        // never in the middle of a running script.
        callback->pushEvent(
            figure, L"SizeChangedData", L"SizeChanged", { L"Width", L"Height" }, values);
    }
    return true;
}
//=============================================================================
