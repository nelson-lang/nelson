//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Software rasterization of the stepped / stateful controls (uislider, including
// the two-thumb range variant; uispinner steppers; uiswitch two-state toggle), so
// getframe/exportapp in adv-cli draw a recognizable widget instead of a plain box.
// Geometry mirrors the web frontend (FigureControls / figureSvg).
//=============================================================================
// Pixel advance of a UTF-8 label at the given (unscaled) font size, summed over
// its glyphs. Used to centre the switch's label / toggle / label row.
static double
controlTextWidth(const std::string& text, double fontSize)
{
    const std::wstring wide = utf8_to_wstring(text);
    const std::wstring family = L"sans-serif";
    double total = 0.0;
    for (wchar_t ch : wide) {
        total += FontMetrics::glyphAdvancePixels(ch, family, fontSize, false, false);
    }
    return total;
}
//=============================================================================
// Pull the two thumb values out of a range slider's extraJson ({"range":[lo,hi]}).
static bool
parseSliderRange(const std::string& json, double& lo, double& hi)
{
    const size_t key = json.find("\"range\"");
    if (key == std::string::npos) {
        return false;
    }
    const size_t open = json.find('[', key);
    if (open == std::string::npos) {
        return false;
    }
    char* end = nullptr;
    lo = std::strtod(json.c_str() + open + 1, &end);
    if (end == json.c_str() + open + 1) {
        return false;
    }
    const size_t comma = json.find(',', static_cast<size_t>(end - json.c_str()));
    if (comma == std::string::npos) {
        return false;
    }
    char* end2 = nullptr;
    hi = std::strtod(json.c_str() + comma + 1, &end2);
    return end2 != json.c_str() + comma + 1;
}
//=============================================================================
// A horizontal track with a thumb at Value; the range variant adds a second
// thumb and fills the span between the two.
static void
rasterizeSliderControl(RasterCanvas& canvas, const WebUIControl& control, int supersample)
{
    const double minimum = control.minimum;
    const double maximum = control.maximum;
    const bool vertical = parseDialBoolField(control.extraJson, "vertical");
    const std::vector<double> majorTicks = parseDialNumberArray(control.extraJson, "major");
    const std::vector<double> minorTicks = parseDialNumberArray(control.extraJson, "minor");
    const std::vector<std::string> labels = parseDialStringArray(control.extraJson, "labels");
    double lo = control.value;
    double hi = 0.0;
    const bool isRange = parseSliderRange(control.extraJson, lo, hi);
    const RasterColor track { 160, 160, 160, 1.0 };
    const RasterColor thumbFill { 240, 240, 240, 1.0 };
    const RasterColor thumbBorder { 100, 100, 100, 1.0 };
    const RasterColor fontColor = control.foreground.empty()
        ? RasterColor { 33, 33, 33, 1.0 }
        : controlColor(control.foreground, "rgb(33,33,33)");
    const double labelSize = control.fontSize > 0 ? control.fontSize : 11.0;
    auto fraction = [&](double v) {
        return maximum > minimum ? std::clamp((v - minimum) / (maximum - minimum), 0.0, 1.0) : 0.0;
    };
    auto drawThumb = [&](double cx, double cy) {
        // Qt draws a 10x8 thumb centred on the track.
        canvas.fillRect((cx - 5.0) * supersample, (cy - 4.0) * supersample, 10.0 * supersample,
            8.0 * supersample, thumbFill);
        canvas.strokePolyline({ { (cx - 5.0) * supersample, (cy - 4.0) * supersample },
                                  { (cx + 5.0) * supersample, (cy - 4.0) * supersample },
                                  { (cx + 5.0) * supersample, (cy + 4.0) * supersample },
                                  { (cx - 5.0) * supersample, (cy + 4.0) * supersample },
                                  { (cx - 5.0) * supersample, (cy - 4.0) * supersample } },
            1.0 * supersample, thumbBorder, {});
    };
    if (vertical) {
        // The Qt widget grows 8px either side; the track sits at relative x=10.
        const double x = control.x + 2.0;
        const double top = control.y;
        const double length = control.h;
        auto posY = [&](double v) { return top + (1.0 - fraction(v)) * length; };
        dialStrokeLine(canvas, x, top, x, top + length, 3.0, track, supersample);
        if (isRange) {
            dialStrokeLine(canvas, x, posY(lo), x, posY(hi), 3.0, track, supersample);
        }
        for (double tick : minorTicks) {
            if (!dialWithinLimits(tick, minimum, maximum)) {
                continue;
            }
            dialStrokeLine(
                canvas, x + 4.0, posY(tick), x + 7.0, posY(tick), 1.0, track, supersample);
        }
        for (size_t k = 0; k < majorTicks.size(); ++k) {
            if (!dialWithinLimits(majorTicks[k], minimum, maximum)) {
                continue;
            }
            const double y = posY(majorTicks[k]);
            dialStrokeLine(canvas, x + 4.0, y, x + 10.0, y, 1.0, track, supersample);
            if (k < labels.size()) {
                dialLabel(
                    canvas, x + 12.0 + labelSize, y, labels[k], fontColor, labelSize, supersample);
            }
        }
        if (isRange) {
            drawThumb(x, posY(lo));
            drawThumb(x, posY(hi));
        } else {
            drawThumb(x, posY(control.value));
        }
        return;
    }
    // The Qt widget grows 20px above the nominal track Position; the track sits at
    // relative y=10 of that taller widget, i.e. 10px above the Position top.
    const double trackY = control.y - 10.0;
    const double x0 = control.x;
    const double length = control.w;
    auto posX = [&](double v) { return x0 + fraction(v) * length; };
    dialStrokeLine(canvas, x0, trackY, x0 + length, trackY, 3.0, track, supersample);
    if (isRange) {
        dialStrokeLine(canvas, posX(std::min(lo, hi)), trackY, posX(std::max(lo, hi)), trackY, 3.0,
            track, supersample);
    }
    for (double tick : minorTicks) {
        if (!dialWithinLimits(tick, minimum, maximum)) {
            continue;
        }
        const double x = posX(tick);
        dialStrokeLine(canvas, x, trackY + 4.0, x, trackY + 7.0, 1.0, track, supersample);
    }
    for (size_t k = 0; k < majorTicks.size(); ++k) {
        if (!dialWithinLimits(majorTicks[k], minimum, maximum)) {
            continue;
        }
        const double x = posX(majorTicks[k]);
        dialStrokeLine(canvas, x, trackY + 4.0, x, trackY + 10.0, 1.0, track, supersample);
        if (k < labels.size()) {
            dialLabel(canvas, x, trackY + 11.0 + labelSize * 0.6, labels[k], fontColor, labelSize,
                supersample);
        }
    }
    if (isRange) {
        drawThumb(posX(lo), trackY);
        drawThumb(posX(hi), trackY);
    } else {
        drawThumb(posX(control.value), trackY);
    }
}
//=============================================================================
static void
rasterizeSpinnerControl(RasterCanvas& canvas, const WebUIControl& control, const std::string& text,
    const RasterColor& fg, double baseline, int supersample)
{
    rasterizeControlBox(canvas, control.x * supersample, control.y * supersample,
        control.w * supersample, control.h * supersample, parseCssColor("rgb(255,255,255)"),
        parseCssColor("rgb(143,143,143)"));
    const double stepperW = std::min(18.0, control.w * 0.3);
    const double stepperX = control.x + control.w - stepperW;
    rasterizeControlLabel(canvas, control, stepperX - 6.0, baseline, "right", text, fg);
    const RasterColor separator = parseCssColor("rgb(200,200,200)");
    canvas.strokePolyline(
        { { stepperX * supersample, (control.y + 2.0) * supersample },
            { stepperX * supersample, (control.y + control.h - 2.0) * supersample } },
        1.0 * supersample, separator, {});
    const double midY = control.y + control.h / 2.0;
    canvas.strokePolyline({ { stepperX * supersample, midY * supersample },
                              { (control.x + control.w) * supersample, midY * supersample } },
        1.0 * supersample, separator, {});
    const RasterColor arrow = parseCssColor("rgb(110,110,110)");
    const double arrowCx = stepperX + stepperW / 2.0;
    const double halfW = stepperW * 0.26;
    const double halfH = control.h * 0.12;
    rasterizeArrow(
        canvas, arrowCx, control.y + control.h * 0.27, halfW, halfH, 'u', arrow, supersample);
    rasterizeArrow(
        canvas, arrowCx, control.y + control.h * 0.73, halfW, halfH, 'd', arrow, supersample);
}
//=============================================================================
// Two-state switch with the two Items just outside the track. Horizontal by
// default; the rocker (Orientation 'vertical') stacks On over Off.
static void
rasterizeSwitchControl(
    RasterCanvas& canvas, const WebUIControl& control, double fontSize, int supersample)
{
    const bool on = control.value >= 1.0;
    const bool vertical = parseDialBoolField(control.extraJson, "vertical");
    const std::string offLabel = !control.items.empty() ? control.items[0] : std::string("Off");
    const std::string onLabel = control.items.size() > 1 ? control.items[1] : std::string("On");
    const RasterColor trackColor { 112, 112, 112, 1.0 };
    const RasterColor thumbColor { 255, 255, 255, 1.0 };
    const RasterColor thumbRing { 150, 150, 150, 1.0 };
    const RasterColor labelColor { 33, 33, 33, 1.0 };
    const double thumbRadius = 9.0;
    auto drawThumb = [&](double cx, double cy) {
        canvas.fillDisc(cx * supersample, cy * supersample, thumbRadius * supersample, thumbColor);
        dialStrokeArc(canvas, cx, cy, thumbRadius, 0.0, 360.0, 1.0, thumbRing, supersample);
    };
    if (vertical) {
        const double trackX = control.x + control.w / 2.0;
        const double trackTop = control.y;
        const double trackBottom = control.y + control.h;
        rasterizeRoundedBox(canvas, trackX - 7.0, trackTop, 14.0, control.h, 7.0, trackColor,
            trackColor, supersample);
        const double thumbY = on ? trackTop + 7.0 : trackBottom - 7.0;
        drawThumb(trackX, thumbY);
        // On (rightLabel) sits above the track, Off (leftLabel) below it.
        rasterizeControlLabel(
            canvas, control, trackX, control.y - 4.0, "center", onLabel, labelColor);
        rasterizeControlLabel(canvas, control, trackX, control.y + control.h + 4.0 + fontSize * 0.7,
            "center", offLabel, labelColor);
        return;
    }
    const double trackCy = control.y + control.h / 2.0;
    rasterizeRoundedBox(canvas, control.x, trackCy - 7.0, control.w, 14.0, 7.0, trackColor,
        trackColor, supersample);
    const double thumbX = on ? control.x + control.w - 7.0 : control.x + 7.0;
    drawThumb(thumbX, trackCy);
    const double baseline = trackCy + fontSize * 0.35;
    rasterizeControlLabel(
        canvas, control, control.x - 4.0, baseline, "right", offLabel, labelColor);
    rasterizeControlLabel(
        canvas, control, control.x + control.w + 4.0, baseline, "left", onLabel, labelColor);
}
//=============================================================================
