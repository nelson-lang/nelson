//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "HeadlessFigureHost.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOFigure.hpp"
#include "GOFigureMouseEvents.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GraphicsEnvironment.hpp"
#include "WebDesktopBridge.hpp"
#include "WebFigureAnimation.hpp"
#include "WebFigureFramePush.hpp"
#include <algorithm>
#include <chrono>
#include <cmath>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    static constexpr long long HEADLESS_WEB_REPAINT_INTERVAL_US = 16000;
    //=============================================================================
    static bool
    objectHasDrawableContent(GraphicsObject* object)
    {
        if (!object) {
            return false;
        }
        if (object->haveProperty(GO_VISIBLE_PROPERTY_NAME_STR)
            && object->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
            return false;
        }
        if (object->isType(GO_PROPERTY_VALUE_UIMENU_STR)
            || object->isType(GO_PROPERTY_VALUE_UICONTEXTMENU_STR)) {
            return false;
        }
        if (object->isType(GO_PROPERTY_VALUE_TEXT_STR)) {
            return !object->findStringProperty(GO_STRING_PROPERTY_NAME_STR).empty();
        }
        if (!object->isType(GO_PROPERTY_VALUE_AXES_STR)
            && !object->isType(GO_PROPERTY_VALUE_POLAR_AXES_STR)
            && !object->isType(GO_PROPERTY_VALUE_TILED_LAYOUT_STR)) {
            return true;
        }
        auto* children = static_cast<GOGObjectsProperty*>(
            object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
        if (!children) {
            return false;
        }
        for (int64 childHandle : children->data()) {
            if (objectHasDrawableContent(findGraphicsObject(childHandle, false))) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    static bool
    figureHasDrawableContent(GOFigure* fig)
    {
        if (!fig) {
            return false;
        }
        auto* children = static_cast<GOGObjectsProperty*>(
            fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
        if (!children) {
            return false;
        }
        for (int64 childHandle : children->data()) {
            if (objectHasDrawableContent(findGraphicsObject(childHandle, false))) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    class HeadlessFigureHost final : public GOFigureHost
    {
    public:
        explicit HeadlessFigureHost(int64 figureId)
            : id(figureId), figure(new GOFigure(this, static_cast<int>(figureId + 1)))
        {
        }

        ~HeadlessFigureHost() override { delete figure; }

        GOFigure*
        hostFigure() override
        {
            return figure;
        }

        int64
        hostId() const override
        {
            return id;
        }

        bool
        isHeadlessHost() const override
        {
            return true;
        }

        void
        requestHostRepaint() override
        {
            pushWebFrame(false);
        }

        void
        requestHostRepaintNow() override
        {
            pushWebFrame(true);
        }

        bool
        syncHostRendering(int) override
        {
            return true;
        }

        void
        updateHostState(bool) override
        {
        }

        void
        presentHost() override
        {
        }

        void
        prepareHostClose(bool) override
        {
        }

        void
        destroyHost() override
        {
            delete this;
        }

        bool
        isDockedHost() override
        {
            return false;
        }

        bool
        hostFrameGeometry(double&, double&, double&, double&, double&) override
        {
            return false;
        }

        void
        setWaitMouseOrKeyPressEvent(bool waiting) override
        {
            waitMode = waiting ? WAIT_PRESS_MODE::WAIT : WAIT_PRESS_MODE::NONE;
        }

        bool
        isWaitingMouseOrKeyPressEvent(WAIT_PRESS_MODE& mode) override
        {
            mode = waitMode;
            return waitMode == WAIT_PRESS_MODE::WAIT;
        }

        bool
        grabHostFrameRGBA(bool, std::vector<unsigned char>&, int&, int&) override
        {
            return false;
        }

        void*
        nativeParentWidget() override
        {
            return nullptr;
        }

        void
        rebuildHostMenus() override
        {
        }

        MOUSE_MODE
        currentHostMouseMode() override { return mouseMode; }

        void
        toggleHostMouseMode(MOUSE_MODE mode) override
        {
            mouseMode = mouseMode == mode ? MOUSE_MODE::NONE : mode;
        }

        bool
        injectHostMouseEvent(const std::wstring& action, double x, double y, int button = 0,
            bool shiftDown = false, bool ctrlDown = false, bool doubleClick = false) override
        {
            dispatchFigureMouseEvent(figure, action, x, y,
                resolveSelectionType(button, shiftDown, ctrlDown, doubleClick));
            return true;
        }

        bool
        hostDrawingAreaScreenRect(double&, double&, double&, double&) override
        {
            return false;
        }

    private:
        void
        pushWebFrame(bool force)
        {
            if (m_renderingFrame) {
                figure->setRenderingStateInvalid(true);
                return;
            }
            if (!figureHasDrawableContent(figure)) {
                figure->setRenderingStateInvalid(true);
                return;
            }
            WebDesktopBridge* bridge = WebDesktopBridge::getInstance();
            if (!bridge->hasWebFigureFrameHook()) {
                figure->setRenderingStateInvalid(true);
                return;
            }
            // Follow a display scale change made while the figure is open: the
            // browser draws at its own ratio, captures use this one.
            const double ratio = GraphicsEnvironment::frameCaptureDeviceRatio();
            const double current
                = figure->findScalarDoubleProperty(GO_DEVICE_PIXEL_RATIO_PROPERTY_NAME_STR);
            if (std::fabs(current - ratio) > 1e-6) {
                figure->setScalarDoubleDefault(GO_DEVICE_PIXEL_RATIO_PROPERTY_NAME_STR, ratio);
            }
            // Shared clock with drawNow/view(): a drawnow-driven loop sets the
            // properties (which lands here) and then streams its own frame; a
            // per-host clock would render the same state twice every frame.
            if (!force && webFigureFramePushedWithin(id, HEADLESS_WEB_REPAINT_INTERVAL_US)) {
                figure->setRenderingStateInvalid(true);
                return;
            }
            m_renderingFrame = true;
            const bool pushed = pushWebFigureAnimationFrame(bridge, id);
            m_renderingFrame = false;
            if (pushed) {
                noteWebFigureFramePushed(id);
            } else {
                figure->setRenderingStateInvalid(true);
            }
        }

        int64 id;
        GOFigure* figure;
        bool m_renderingFrame = false;
        WAIT_PRESS_MODE waitMode = WAIT_PRESS_MODE::NONE;
        MOUSE_MODE mouseMode = MOUSE_MODE::NONE;
    };
    //=============================================================================
} // namespace
//=============================================================================
GOFigureHost*
createHeadlessFigureHost(int64 figureId)
{
    return new HeadlessFigureHost(figureId);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
