//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "webui.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// The `webui_set_config()` options are PROCESS-GLOBAL, not per-window: the last
// caller wins for every window shown afterwards. Several modules configure WebUI,
// so a setting left behind by one silently changed how the next window was shown.
//
// This guard restores the previous value on scope exit.
//
// Only use it for options consumed synchronously by `webui_show*` (in practice
// `show_wait_connection`). Options read later by the running server threads, such
// as `multi_client` or `use_cookies`, must stay set for the window's lifetime.
//
// Not thread-safe by itself: the guarded option is global, so overlapping scopes
// on different threads would restore each other's values. Nelson shows its
// windows from the interpreter thread.
class WebUiConfigScope
{
public:
    WebUiConfigScope(webui_config option, bool status)
        : m_option(option), m_previous(webui_get_config(option))
    {
        webui_set_config(option, status);
    }
    ~WebUiConfigScope() { webui_set_config(m_option, m_previous); }

    WebUiConfigScope(const WebUiConfigScope&) = delete;
    WebUiConfigScope&
    operator=(const WebUiConfigScope&)
        = delete;

private:
    webui_config m_option;
    bool m_previous;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
