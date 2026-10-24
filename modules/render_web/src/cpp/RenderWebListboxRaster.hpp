//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
static void
rasterizeListboxControl(RasterCanvas& canvas, const WebUIControl& control, double fontSize,
    const RasterColor& fg, int supersample)
{
    const RasterColor softBorder = parseCssColor("rgb(170,170,170)");
    rasterizeRoundedBox(canvas, control.x, control.y, control.w, control.h, 2.0,
        parseCssColor("rgb(255,255,255)"), softBorder, supersample);
    const size_t itemCount = control.items.size();
    if (itemCount == 0) {
        return;
    }
    const double rowH = std::max(fontSize + 6.0, 16.0);
    const int maxVisible = std::max(1, static_cast<int>(control.h / rowH));
    const bool overflow = static_cast<int>(itemCount) > maxVisible;
    const double scrollbarW = overflow ? 12.0 : 0.0;
    const size_t sel = selectedItemIndex(control);
    size_t first = 0;
    if (overflow && sel >= static_cast<size_t>(maxVisible)) {
        first = sel - static_cast<size_t>(maxVisible) + 1;
    }
    for (int row = 0; row < maxVisible && first + static_cast<size_t>(row) < itemCount; ++row) {
        const size_t idx = first + static_cast<size_t>(row);
        const double ry = control.y + row * rowH;
        if (idx == sel) {
            // Subtle gray band: a saturated highlight looks harsh in a static image
            // and hurts the dark entry text.
            canvas.fillRect((control.x + 1.0) * supersample, ry * supersample,
                (control.w - 2.0 - scrollbarW) * supersample, rowH * supersample,
                parseCssColor("rgb(200,200,200)"));
        }
        rasterizeControlLabel(canvas, control, control.x + 6.0, ry + rowH / 2.0 + fontSize * 0.35,
            "left", control.items[idx], fg);
    }
    if (!overflow) {
        return;
    }
    const double sbx = control.x + control.w - scrollbarW;
    canvas.fillRect(sbx * supersample, control.y * supersample, scrollbarW * supersample,
        control.h * supersample, parseCssColor("rgb(240,240,240)"));
    const double buttonH = std::min(scrollbarW, control.h * 0.2);
    const RasterColor arrowColor = parseCssColor("rgb(120,120,120)");
    rasterizeArrow(canvas, sbx + scrollbarW / 2.0, control.y + buttonH * 0.55, scrollbarW * 0.28,
        scrollbarW * 0.22, 'u', arrowColor, supersample);
    rasterizeArrow(canvas, sbx + scrollbarW / 2.0, control.y + control.h - buttonH * 0.55,
        scrollbarW * 0.28, scrollbarW * 0.22, 'd', arrowColor, supersample);
    const double trackTop = control.y + buttonH;
    const double trackH = control.h - 2.0 * buttonH;
    const double thumbH = std::max(rowH, trackH * maxVisible / static_cast<double>(itemCount));
    const double travel = trackH - thumbH;
    const double denom = static_cast<double>(itemCount - maxVisible);
    const double frac = denom > 0.0 ? static_cast<double>(first) / denom : 0.0;
    const double thumbY = trackTop + std::max(0.0, travel) * frac;
    canvas.fillRect((sbx + 2.0) * supersample, (thumbY + 1.0) * supersample,
        (scrollbarW - 4.0) * supersample, (thumbH - 2.0) * supersample,
        parseCssColor("rgb(190,190,190)"));
}
//=============================================================================
