//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdint>
#include <string>
#include <chrono>
#include <thread>
#include "DrawNow.hpp"
#include "WebDesktopBridge.hpp"
#include "WebFigureFramePush.hpp"
#include "GOFiguresManager.hpp"
#include "GOFigure.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "WebFigureAnimation.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static constexpr long long DRAWNOW_LIMIT_RATE_INTERVAL_MS = 16;
// Streamed web animation frames are throttled to ~60 fps. The client re-fetches the final
// state at the next prompt, so a dropped last frame is fine. A few milliseconds are budgeted
// for display-list encoding and websocket delivery.
static constexpr long long DRAWNOW_WEB_PUSH_INTERVAL_US = 10000;
// Busy-wait window before the frame deadline. Keep it short: the yield() loop burns a full
// core for its whole duration.
static constexpr long long DRAWNOW_WEB_SPIN_MARGIN_US = 500;
static constexpr int DRAWNOW_RENDER_SYNC_TIMEOUT_MS = 250;
//=============================================================================
static bool
isCompactFigure(GOFigure* goFigure)
{
    std::vector<double> position
        = goFigure->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (position.size() >= 4 && position[2] > 0 && position[3] > 0) {
        return position[3] <= 500;
    }
    return goFigure->getHeight() <= 500;
}
//=============================================================================
static void
updateSubplotTextAutoFontSize(GOFigure* goFigure)
{
    if (!goFigure) {
        return;
    }
    GOGObjectsProperty* children = static_cast<GOGObjectsProperty*>(
        goFigure->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return;
    }
    double fontSize = isCompactFigure(goFigure) ? 14.5 : 15.0;
    for (auto handle : children->data()) {
        GraphicsObject* child = findGraphicsObject(handle, false);
        if (!child || child->getType() != GO_PROPERTY_VALUE_SUBPLOT_TEXT_STR
            || !child->isAuto(GO_FONT_SIZE_MODE_PROPERTY_NAME_STR)) {
            continue;
        }
        child->setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, fontSize);
    }
}
//=============================================================================
static bool
skipLimitedDraw()
{
    using clock = std::chrono::steady_clock;
    static clock::time_point lastDraw;
    const clock::time_point now = clock::now();
    if (lastDraw == clock::time_point {}) {
        lastDraw = now;
        return false;
    }
    if (std::chrono::duration_cast<std::chrono::milliseconds>(now - lastDraw).count()
        < DRAWNOW_LIMIT_RATE_INTERVAL_MS) {
        return true;
    }
    lastDraw = now;
    return false;
}
//=============================================================================
void
drawNow(DrawNowMode mode)
{
    if (mode == DrawNowMode::LimitRate && skipLimitedDraw()) {
        return;
    }

    std::vector<int64> figures = getFigureGraphicsObjects();
    // With the web frame hook (adv-cli --webview) the hook below renders each figure, so a
    // HeadlessFigureHost repaint here would render the state twice. Without the hook that
    // repaint is the only path that refreshes object state (auto limits, modes) at drawnow.
    const bool webFrameHookActive = WebDesktopBridge::getInstance()->hasWebFigureFrameHook();
    for (auto f : figures) {
        GOFigureHost* window = getFigure(f);
        if (!window) {
            return;
        }
        GOFigure* goFigure = window->hostFigure();
        if (!goFigure) {
            return;
        }
        GOOnOffProperty* drawLaterProperty = static_cast<GOOnOffProperty*>(
            goFigure->findProperty(GO_DRAW_LATER_PROPERTY_NAME_STR));
        updateSubplotTextAutoFontSize(goFigure);
        std::wstring value = drawLaterProperty->data();
        bool restoreOffAfter = false;
        if (value == GO_PROPERTY_VALUE_ON_STR) {
            goFigure->setStringDefault(GO_DRAW_LATER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
            goFigure->setRenderingStateInvalid(true);
            restoreOffAfter = true;
        }
        if (goFigure->isRenderingStateInvalid()
            && !(window->isHeadlessHost() && webFrameHookActive)) {
            if (window->isHeadlessHost()) {
                goFigure->repaint();
            } else {
                // Paint synchronously. The coalescing repaint timer costs several Windows
                // timer ticks (~60 ms) per frame and caps animations near 16 fps.
                goFigure->repaintNow();
            }
        }
        window->syncHostRendering(DRAWNOW_RENDER_SYNC_TIMEOUT_MS);
        if (restoreOffAfter) {
            goFigure->setStringDefault(GO_DRAW_LATER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
        }
    }

    // WEB_GUI: stream the current frame so drawnow animations play live. drawNow runs on the
    // interpreter thread and renders the display list directly. An RPC would block until the
    // script yields at the prompt.
    WebDesktopBridge* bridge = WebDesktopBridge::getInstance();
    if (bridge->hasWebFigureFrameHook()) {
        // Pace the loop to ~60 fps by sleeping the rest of the frame interval. The browser has
        // no Qt repaint to pace it, so an un-delayed loop would finish instantly.
        using clock = std::chrono::steady_clock;
        static clock::time_point lastPush;
        if (mode != DrawNowMode::LimitRate && lastPush != clock::time_point {}) {
            const auto deadline
                = lastPush + std::chrono::microseconds(DRAWNOW_WEB_PUSH_INTERVAL_US);
            const auto remaining
                = std::chrono::duration_cast<std::chrono::microseconds>(deadline - clock::now());
            if (remaining.count() > DRAWNOW_WEB_SPIN_MARGIN_US) {
                std::this_thread::sleep_for(
                    remaining - std::chrono::microseconds(DRAWNOW_WEB_SPIN_MARGIN_US));
            }
            while (clock::now() < deadline) {
                std::this_thread::yield();
            }
        }
        lastPush = clock::now();
        for (auto f : figures) {
            if (webFigureFramePushedWithin(static_cast<int64_t>(f), DRAWNOW_WEB_PUSH_INTERVAL_US)) {
                continue;
            }
            if (pushWebFigureAnimationFrame(bridge, static_cast<int64_t>(f))) {
                // Feeds the shared throttle so the property-set repaint path does not re-render.
                noteWebFigureFramePushed(static_cast<int64_t>(f));
            }
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
