//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Software rasterization of the rotary / scale dials (uigauge, uiknob), aligned
// on the native Qt widgets (UIBindingGauge / UIBindingKnob): the same scale arc,
// minor/major ticks, numeric tick labels and value needle, so a headless
// getframe/exportapp matches the Qt desktop face. Angles use the Qt convention
// (degrees, y up: a point is (cx + cos*r, cy - sin*r)); the gauge sweeps 225deg
// clockwise to -45deg, the knob likewise.
//=============================================================================
// A point on a circle in the Qt angle convention (y up), in control space.
static std::pair<double, double>
dialPoint(double cx, double cy, double r, double angleDegrees)
{
    const double a = angleDegrees * M_PI / 180.0;
    return { cx + std::cos(a) * r, cy - std::sin(a) * r };
}
//=============================================================================
static void
dialStrokeLine(RasterCanvas& canvas, double x1, double y1, double x2, double y2, double width,
    const RasterColor& color, int supersample)
{
    canvas.strokePolyline(
        { { x1 * supersample, y1 * supersample }, { x2 * supersample, y2 * supersample } },
        width * supersample, color, {});
}
//=============================================================================
// Stroke a circular arc (Qt convention) sampled as a short polyline. a0..a1 in
// degrees; a full circle is 0..360.
static void
dialStrokeArc(RasterCanvas& canvas, double cx, double cy, double r, double a0, double a1,
    double width, const RasterColor& color, int supersample)
{
    if (r <= 0.0) {
        return;
    }
    const int segments = std::max(8, static_cast<int>(std::ceil(std::fabs(a1 - a0) / 4.0)));
    std::vector<std::pair<double, double>> pts;
    pts.reserve(static_cast<size_t>(segments) + 1);
    for (int i = 0; i <= segments; ++i) {
        const auto p = dialPoint(cx, cy, r, a0 + (a1 - a0) * i / segments);
        pts.push_back({ p.first * supersample, p.second * supersample });
    }
    canvas.strokePolyline(pts, width * supersample, color, {});
}
//=============================================================================
static void
dialLabel(RasterCanvas& canvas, double cx, double cy, const std::string& text,
    const RasterColor& color, double size, int supersample)
{
    if (text.empty()) {
        return;
    }
    WebDrawPrimitive primitive;
    primitive.kind = "text";
    primitive.x = cx;
    primitive.y = cy + size * 0.35;
    primitive.text = text;
    primitive.align = "center";
    primitive.color = "rgb(" + std::to_string(static_cast<int>(color.r)) + ","
        + std::to_string(static_cast<int>(color.g)) + ","
        + std::to_string(static_cast<int>(color.b)) + ")";
    primitive.size = size;
    primitive.fontFamily = "sans-serif";
    rasterizeText(canvas, primitive, supersample);
}
//=============================================================================
// Evenly spaced ticks over [minimum, maximum] (used when MinorTicks is empty, as
// the Qt widget does).
static std::vector<double>
dialAutoTicks(double minimum, double maximum, int divisions)
{
    std::vector<double> ticks;
    if (maximum <= minimum || divisions <= 0) {
        return ticks;
    }
    const double step = (maximum - minimum) / divisions;
    for (int k = 0; k <= divisions; ++k) {
        ticks.push_back(minimum + k * step);
    }
    return ticks;
}
//=============================================================================
static bool
dialWithinLimits(double tick, double minimum, double maximum)
{
    const double tolerance = (maximum - minimum) * 1e-9;
    return tick >= minimum - tolerance && tick <= maximum + tolerance;
}
//=============================================================================
static std::vector<double>
parseDialNumberArray(const std::string& json, const char* name)
{
    std::vector<double> result;
    size_t pos = findJsonArrayMember(json, name);
    if (pos == std::string::npos) {
        return result;
    }
    for (++pos;;) {
        skipJsonSpaces(json, pos);
        if (pos >= json.size() || json[pos] == ']') {
            return result;
        }
        char* end = nullptr;
        const double value = std::strtod(json.c_str() + pos, &end);
        if (end == json.c_str() + pos) {
            return result;
        }
        result.push_back(value);
        pos = static_cast<size_t>(end - json.c_str());
        skipJsonSpaces(json, pos);
        if (pos < json.size() && json[pos] == ',') {
            ++pos;
        }
    }
}
//=============================================================================
static std::vector<std::string>
parseDialStringArray(const std::string& json, const char* name)
{
    size_t pos = findJsonArrayMember(json, name);
    return pos == std::string::npos ? std::vector<std::string>() : parseJsonStringArray(json, pos);
}
//=============================================================================
static std::string
parseDialStringField(const std::string& json, const char* name)
{
    const std::string key = std::string("\"") + name + "\"";
    size_t pos = json.find(key);
    if (pos == std::string::npos) {
        return std::string();
    }
    pos = json.find(':', pos + key.size());
    if (pos == std::string::npos) {
        return std::string();
    }
    ++pos;
    skipJsonSpaces(json, pos);
    std::string out;
    if (pos < json.size() && json[pos] == '"') {
        parseJsonString(json, pos, out);
    }
    return out;
}
//=============================================================================
static bool
parseDialBoolField(const std::string& json, const char* name)
{
    const std::string key = std::string("\"") + name + "\"";
    size_t pos = json.find(key);
    if (pos == std::string::npos) {
        return false;
    }
    pos = json.find(':', pos + key.size());
    if (pos == std::string::npos) {
        return false;
    }
    ++pos;
    skipJsonSpaces(json, pos);
    return json.compare(pos, 4, "true") == 0;
}
//=============================================================================
static void
rasterizeGaugeControl(RasterCanvas& canvas, const WebUIControl& control, int supersample)
{
    const double minimum = control.minimum;
    const double maximum = control.maximum;
    auto fractionOf = [&](double v) {
        return maximum > minimum ? std::clamp((v - minimum) / (maximum - minimum), 0.0, 1.0) : 0.0;
    };
    const std::string shape = parseDialStringField(control.extraJson, "shape");
    const bool vertical = parseDialBoolField(control.extraJson, "vertical");
    const std::vector<double> majorTicks = parseDialNumberArray(control.extraJson, "major");
    const std::vector<double> minorTicks = parseDialNumberArray(control.extraJson, "minor");
    const std::vector<std::string> labels = parseDialStringArray(control.extraJson, "labels");
    const RasterColor frameColor { 70, 70, 70, 1.0 };
    const RasterColor tickColor { 40, 40, 40, 1.0 };
    const RasterColor needleColor { 200, 30, 30, 1.0 };
    const RasterColor background = controlColor(control.background, "rgb(255,255,255)");
    const double labelSize = control.fontSize > 0 ? control.fontSize : 11.0;

    if (shape == "linear") {
        canvas.fillRect((control.x + 1.0) * supersample, (control.y + 1.0) * supersample,
            (control.w - 2.0) * supersample, (control.h - 2.0) * supersample, background);
        rasterizeControlBox(canvas, (control.x + 1.0) * supersample,
            (control.y + 1.0) * supersample, (control.w - 2.0) * supersample,
            (control.h - 2.0) * supersample, background, frameColor);
        std::vector<double> minors
            = minorTicks.empty() ? dialAutoTicks(minimum, maximum, 25) : minorTicks;
        if (vertical) {
            const double x = control.x + control.w / 2.0;
            const double top = control.y + 8.0;
            const double bottom = control.y + control.h - 8.0;
            for (double tick : minors) {
                if (!dialWithinLimits(tick, minimum, maximum)) {
                    continue;
                }
                const double y = bottom - (bottom - top) * fractionOf(tick);
                dialStrokeLine(canvas, x - 4.0, y, x - 1.0, y, 1.0, frameColor, supersample);
            }
            for (size_t k = 0; k < majorTicks.size(); ++k) {
                if (!dialWithinLimits(majorTicks[k], minimum, maximum)) {
                    continue;
                }
                const double y = bottom - (bottom - top) * fractionOf(majorTicks[k]);
                dialStrokeLine(canvas, x - 8.0, y, x - 1.0, y, 1.0, frameColor, supersample);
                if (k < labels.size()) {
                    dialLabel(canvas, x + 2.0 + labelSize, y, labels[k], frameColor, labelSize,
                        supersample);
                }
            }
            const double yv = bottom - (bottom - top) * fractionOf(control.value);
            dialStrokeLine(canvas, x - 12.0, yv, x + 12.0, yv, 2.0, needleColor, supersample);
        } else {
            const double left = control.x + 8.0;
            const double right = control.x + control.w - 8.0;
            const double baseline = control.y + 11.0;
            for (double tick : minors) {
                if (!dialWithinLimits(tick, minimum, maximum)) {
                    continue;
                }
                const double x = left + (right - left) * fractionOf(tick);
                dialStrokeLine(
                    canvas, x, baseline, x, baseline + 4.0, 1.0, frameColor, supersample);
            }
            for (size_t k = 0; k < majorTicks.size(); ++k) {
                if (!dialWithinLimits(majorTicks[k], minimum, maximum)) {
                    continue;
                }
                const double x = left + (right - left) * fractionOf(majorTicks[k]);
                dialStrokeLine(
                    canvas, x, baseline, x, baseline + 8.0, 1.0, frameColor, supersample);
                if (k < labels.size()) {
                    dialLabel(canvas, x, baseline + 8.0 + labelSize * 0.6, labels[k], frameColor,
                        labelSize, supersample);
                }
            }
            const double xv = left + (right - left) * fractionOf(control.value);
            dialStrokeLine(canvas, xv, baseline + 5.0, xv, control.y + control.h - 5.0, 2.0,
                needleColor, supersample);
        }
        return;
    }

    double startAngle = 225.0;
    double sweep = -270.0;
    double cx = control.x + control.w / 2.0;
    double cy = control.y + control.h / 2.0 + 2.0;
    double radius = std::min(control.w, control.h) / 2.0 - 8.0;
    int minorDivisions = 25;
    if (shape == "semicircular") {
        startAngle = 180.0;
        sweep = -180.0;
        cx = control.x + control.w / 2.0;
        cy = control.y + control.h - 7.0;
        radius = std::min(control.w / 2.0 - 8.0, control.h - 12.0);
    } else if (shape == "ninetydegree") {
        startAngle = 180.0;
        sweep = -90.0;
        cx = control.x + control.w - 8.0;
        cy = control.y + control.h - 8.0;
        radius = std::min(control.w - 14.0, control.h - 14.0);
        minorDivisions = 20;
    }
    if (radius <= 0.0) {
        return;
    }
    if (shape == "circular" || shape.empty()) {
        canvas.fillDisc(cx * supersample, cy * supersample, radius * supersample, background);
        dialStrokeArc(canvas, cx, cy, radius, 0.0, 360.0, 1.0, frameColor, supersample);
    } else {
        std::vector<std::pair<double, double>> wedge;
        wedge.push_back({ cx * supersample, cy * supersample });
        const int segments = 40;
        for (int i = 0; i <= segments; ++i) {
            const auto p = dialPoint(cx, cy, radius, startAngle + sweep * i / segments);
            wedge.push_back({ p.first * supersample, p.second * supersample });
        }
        canvas.fillPolygon(wedge, background);
        dialStrokeArc(
            canvas, cx, cy, radius, startAngle, startAngle + sweep, 1.0, frameColor, supersample);
        const auto e0 = dialPoint(cx, cy, radius, startAngle);
        const auto e1 = dialPoint(cx, cy, radius, startAngle + sweep);
        dialStrokeLine(canvas, cx, cy, e0.first, e0.second, 1.0, frameColor, supersample);
        dialStrokeLine(canvas, cx, cy, e1.first, e1.second, 1.0, frameColor, supersample);
    }
    std::vector<double> minors
        = minorTicks.empty() ? dialAutoTicks(minimum, maximum, minorDivisions) : minorTicks;
    for (double tick : minors) {
        if (!dialWithinLimits(tick, minimum, maximum)) {
            continue;
        }
        const double angle = startAngle + sweep * fractionOf(tick);
        const auto outer = dialPoint(cx, cy, radius - 2.0, angle);
        const auto inner = dialPoint(cx, cy, radius - 7.0, angle);
        dialStrokeLine(canvas, outer.first, outer.second, inner.first, inner.second, 1.0, tickColor,
            supersample);
    }
    for (size_t k = 0; k < majorTicks.size(); ++k) {
        if (!dialWithinLimits(majorTicks[k], minimum, maximum)) {
            continue;
        }
        const double angle = startAngle + sweep * fractionOf(majorTicks[k]);
        const auto outer = dialPoint(cx, cy, radius - 2.0, angle);
        const auto inner = dialPoint(cx, cy, radius - 12.0, angle);
        dialStrokeLine(canvas, outer.first, outer.second, inner.first, inner.second, 1.0, tickColor,
            supersample);
        if (k < labels.size()) {
            const auto lp = dialPoint(cx, cy, radius - 25.0, angle);
            dialLabel(canvas, lp.first, lp.second, labels[k], tickColor, labelSize, supersample);
        }
    }
    const double valueAngle = startAngle + sweep * fractionOf(control.value);
    const auto needleEnd = dialPoint(cx, cy, radius - 24.0, valueAngle);
    dialStrokeLine(
        canvas, cx, cy, needleEnd.first, needleEnd.second, 2.0, needleColor, supersample);
}
//=============================================================================
static void
rasterizeContinuousKnob(RasterCanvas& canvas, const WebUIControl& control, int supersample)
{
    const double cx = control.x + control.w / 2.0;
    const double cy = control.y + control.h / 2.0;
    const double outerRadius = std::min(control.w, control.h) / 2.0 - 24.0;
    const double faceRadius = outerRadius - 20.0;
    if (faceRadius <= 0.0) {
        return;
    }
    const double minimum = control.minimum;
    const double maximum = control.maximum;
    auto fractionOf = [&](double v) {
        return maximum > minimum ? std::clamp((v - minimum) / (maximum - minimum), 0.0, 1.0) : 0.0;
    };
    const std::vector<double> majorTicks = parseDialNumberArray(control.extraJson, "major");
    const std::vector<double> minorTicks = parseDialNumberArray(control.extraJson, "minor");
    const std::vector<std::string> labels = parseDialStringArray(control.extraJson, "labels");
    const RasterColor fontColor = control.foreground.empty()
        ? RasterColor { 33, 33, 33, 1.0 }
        : controlColor(control.foreground, "rgb(33,33,33)");
    const double labelSize = control.fontSize > 0 ? control.fontSize : 11.0;
    std::vector<double> minors
        = minorTicks.empty() ? dialAutoTicks(minimum, maximum, 40) : minorTicks;
    for (double tick : minors) {
        if (!dialWithinLimits(tick, minimum, maximum)) {
            continue;
        }
        const double angle = 225.0 - fractionOf(tick) * 270.0;
        const auto outer = dialPoint(cx, cy, outerRadius, angle);
        const auto inner = dialPoint(cx, cy, outerRadius - 7.0, angle);
        dialStrokeLine(canvas, outer.first, outer.second, inner.first, inner.second, 1.0, fontColor,
            supersample);
    }
    for (size_t k = 0; k < majorTicks.size(); ++k) {
        if (!dialWithinLimits(majorTicks[k], minimum, maximum)) {
            continue;
        }
        const double angle = 225.0 - fractionOf(majorTicks[k]) * 270.0;
        const auto outer = dialPoint(cx, cy, outerRadius, angle);
        const auto inner = dialPoint(cx, cy, outerRadius - 12.0, angle);
        dialStrokeLine(canvas, outer.first, outer.second, inner.first, inner.second, 2.0, fontColor,
            supersample);
        if (k < labels.size()) {
            const auto lp = dialPoint(cx, cy, outerRadius + 9.0, angle);
            dialLabel(canvas, lp.first, lp.second, labels[k], fontColor, labelSize, supersample);
        }
    }
    canvas.fillDisc(cx * supersample, cy * supersample, faceRadius * supersample,
        parseCssColor("rgb(202,202,202)"));
    dialStrokeArc(
        canvas, cx, cy, faceRadius, 0.0, 360.0, 1.0, parseCssColor("rgb(90,90,90)"), supersample);
    const double innerRadius = faceRadius * 0.65;
    canvas.fillDisc(cx * supersample, cy * supersample, innerRadius * supersample,
        parseCssColor("rgb(238,238,238)"));
    dialStrokeArc(
        canvas, cx, cy, innerRadius, 0.0, 360.0, 1.0, parseCssColor("rgb(90,90,90)"), supersample);
    const double angle = 225.0 - fractionOf(control.value) * 270.0;
    const auto tip = dialPoint(cx, cy, faceRadius + 5.0, angle);
    const auto left = dialPoint(cx, cy, innerRadius - 2.0, angle - 8.0);
    const auto right = dialPoint(cx, cy, innerRadius - 2.0, angle + 8.0);
    canvas.fillPolygon({ { tip.first * supersample, tip.second * supersample },
                           { left.first * supersample, left.second * supersample },
                           { right.first * supersample, right.second * supersample } },
        parseCssColor("rgb(196,38,46)"));
}
//=============================================================================
static void
rasterizeDiscreteKnob(RasterCanvas& canvas, const WebUIControl& control, int supersample)
{
    const double cx = control.x + control.w / 2.0;
    const double cy = control.y + control.h / 2.0;
    const double outerRadius = std::min(control.w, control.h) / 2.0 - 62.0;
    const double labelRadius = outerRadius + 35.0;
    if (outerRadius <= 0.0) {
        return;
    }
    const RasterColor fontColor = control.foreground.empty()
        ? RasterColor { 33, 33, 33, 1.0 }
        : controlColor(control.foreground, "rgb(33,33,33)");
    const double labelSize = control.fontSize > 0 ? control.fontSize : 11.0;
    const int count = std::max(1, static_cast<int>(control.items.size()));
    int itemIndex = static_cast<int>(std::lround(control.value)) - 1;
    itemIndex = std::clamp(itemIndex, 0, count - 1);
    canvas.fillDisc(cx * supersample, cy * supersample, outerRadius * supersample,
        parseCssColor("rgb(202,202,202)"));
    dialStrokeArc(
        canvas, cx, cy, outerRadius, 0.0, 360.0, 1.0, parseCssColor("rgb(80,80,80)"), supersample);
    const double activeFraction = count > 1 ? static_cast<double>(itemIndex) / (count - 1) : 0.0;
    const double activeAngle = 180.0 - activeFraction * 180.0;
    const auto axisStart = dialPoint(cx, cy, outerRadius, activeAngle);
    const auto axisEnd = dialPoint(cx, cy, outerRadius, activeAngle + 180.0);
    const auto normal = dialPoint(0.0, 0.0, 10.0, activeAngle + 90.0);
    std::vector<std::pair<double, double>> band
        = { { (axisStart.first + normal.first) * supersample,
                (axisStart.second + normal.second) * supersample },
              { (axisStart.first - normal.first) * supersample,
                  (axisStart.second - normal.second) * supersample },
              { (axisEnd.first - normal.first) * supersample,
                  (axisEnd.second - normal.second) * supersample },
              { (axisEnd.first + normal.first) * supersample,
                  (axisEnd.second + normal.second) * supersample } };
    canvas.fillPolygon(band, parseCssColor("rgb(238,238,238)"));
    std::vector<std::pair<double, double>> bandOutline = band;
    bandOutline.push_back(band.front());
    canvas.strokePolyline(bandOutline, 1.0 * supersample, parseCssColor("rgb(110,110,110)"), {});
    for (int k = 0; k < count; ++k) {
        const double fraction = count > 1 ? static_cast<double>(k) / (count - 1) : 0.0;
        const double angle = 180.0 - fraction * 180.0;
        const auto dot = dialPoint(cx, cy, outerRadius + 13.0, angle);
        canvas.fillDisc(
            dot.first * supersample, dot.second * supersample, 3.5 * supersample, fontColor);
        if (k < static_cast<int>(control.items.size())) {
            const auto lp = dialPoint(cx, cy, labelRadius + 2.0, angle);
            dialLabel(
                canvas, lp.first, lp.second, control.items[k], fontColor, labelSize, supersample);
        }
    }
    const auto start = dialPoint(cx, cy, outerRadius * 0.32, activeAngle);
    const auto tip = dialPoint(cx, cy, outerRadius - 9.0, activeAngle);
    dialStrokeLine(canvas, start.first, start.second, tip.first, tip.second, 3.0,
        parseCssColor("rgb(200,30,30)"), supersample);
}
//=============================================================================
// The discrete variant shows its Items around a band.
static void
rasterizeKnobControl(RasterCanvas& canvas, const WebUIControl& control, int supersample)
{
    if (parseDialBoolField(control.extraJson, "discrete")) {
        rasterizeDiscreteKnob(canvas, control, supersample);
    } else {
        rasterizeContinuousKnob(canvas, control, supersample);
    }
}
//=============================================================================
