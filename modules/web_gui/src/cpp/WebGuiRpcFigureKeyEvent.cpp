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
nlohmann::json
WebGuiRpc::figureKeyEvent(const nlohmann::json& params)
{
    if (!ensureGraphicsBridge() || m_figureKeyEventFn == nullptr) {
        throw std::runtime_error("figure key events not available");
    }
    const long long handle = params.value("handle", 0LL);
    const bool pressed = params.value("pressed", false);
    const std::string character = params.value("character", std::string());
    const std::string key = params.value("key", std::string());
    const bool shift = params.value("shift", false);
    const bool control = params.value("control", false);
    const bool alt = params.value("alt", false);
    const bool meta = params.value("meta", false);
    bool ok = false;
    runOnInterpreterThread([&] {
        ok = m_figureKeyEventFn(
            handle, pressed, character.c_str(), key.c_str(), shift, control, alt, meta);
    });
    return { { "handle", handle }, { "ok", ok } };
}
//=============================================================================
} // namespace Nelson
//=============================================================================
