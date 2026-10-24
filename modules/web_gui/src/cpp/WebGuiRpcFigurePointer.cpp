//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebGuiRpc.hpp"
#include <stdexcept>
//=============================================================================
namespace Nelson {
//=============================================================================
// Pan a 2D figure: shift the axes limits by the drag fraction, then repaint from
// the fresh draw list (server-authoritative, same pattern as setView).
nlohmann::json
WebGuiRpc::figurePan(const nlohmann::json& params)
{
    if (!ensureGraphicsBridge() || m_panAxesFn == nullptr) {
        throw std::runtime_error("native figure rendering not available");
    }
    const long long handle = params.value("handle", 0LL);
    const double dxFrac = params.value("dxFrac", 0.0);
    const double dyFrac = params.value("dyFrac", 0.0);
    bool ok = false;
    runOnInterpreterThread([&] { ok = m_panAxesFn(handle, dxFrac, dyFrac); });
    if (!ok) {
        throw std::runtime_error("figure " + std::to_string(handle) + " has no axes to pan");
    }
    return figureGetDisplayList(params);
}
//=============================================================================
// Pointer over the figure drawing: (x, y) in figure pixels, bottom-left
// origin. The engine updates CurrentPoint and queues the WindowButton*Fcn
// callbacks; `motion` tells the client whether motion events are wanted.
nlohmann::json
WebGuiRpc::figureMouseEvent(const nlohmann::json& params)
{
    if (!ensureGraphicsBridge() || m_figureMouseEventFn == nullptr) {
        throw std::runtime_error("figure mouse events not available");
    }
    const long long handle = params.value("handle", 0LL);
    const std::string action = params.value("action", std::string());
    const double x = params.value("x", 0.0);
    const double y = params.value("y", 0.0);
    const int button = params.value("button", 0);
    const bool shiftDown = params.value("shiftKey", false);
    const bool ctrlDown = params.value("ctrlKey", false);
    const bool doubleClick = params.value("doubleClick", false);
    bool ok = false;
    bool motion = false;
    runOnInterpreterThread([&] {
        ok = m_figureMouseEventFn(
            handle, action.c_str(), x, y, &motion, button, shiftDown, ctrlDown, doubleClick);
    });
    return { { "handle", handle }, { "ok", ok }, { "motion", motion } };
}
//=============================================================================
} // namespace Nelson
//=============================================================================
