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
#include <webui.h>
//=============================================================================
namespace Nelson {
//=============================================================================
struct WebUiWindowGeometry
{
    int x = 0;
    int y = 0;
    int width = 0;
    int height = 0;
    bool maximized = false;
    bool valid = false;
};
//=============================================================================
inline bool
validateWebUiWindowGeometry(WebUiWindowGeometry& geometry, int minimumWidth = 800,
    int minimumHeight = 600, int maximumSize = 16384)
{
    geometry.valid = geometry.width >= minimumWidth && geometry.width <= maximumSize
        && geometry.height >= minimumHeight && geometry.height <= maximumSize;
    return geometry.valid;
}
//=============================================================================
inline WebUiWindowGeometry
captureWebUiWindowGeometry(size_t window)
{
    WebUiWindowGeometry geometry;
    unsigned int width = 0;
    unsigned int height = 0;
    if (!webui_get_window_geometry(
            window, &geometry.x, &geometry.y, &width, &height, &geometry.maximized)) {
        return geometry;
    }
    geometry.width = static_cast<int>(width);
    geometry.height = static_cast<int>(height);
    validateWebUiWindowGeometry(geometry);
    return geometry;
}
//=============================================================================
inline bool
restoreWebUiWindowGeometry(size_t window, const WebUiWindowGeometry& geometry)
{
    if (!geometry.valid) {
        return false;
    }
    webui_set_size(window, static_cast<unsigned int>(geometry.width),
        static_cast<unsigned int>(geometry.height));
    webui_set_position(
        window, static_cast<unsigned int>(geometry.x), static_cast<unsigned int>(geometry.y));
    if (geometry.maximized) {
        webui_maximize(window);
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
