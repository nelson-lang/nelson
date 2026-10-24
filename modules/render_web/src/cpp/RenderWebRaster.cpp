//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _USE_MATH_DEFINES
#include "RenderWeb.hpp"
#include "RenderWebInternal.hpp"
#include "RenderWebMarkerGeometry.hpp"
#include "FontMetrics.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <array>
#include <cctype>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <map>
#include <mutex>
#include <unordered_map>
#include <vector>
#define STBI_ONLY_PNG
#define STB_IMAGE_STATIC
#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"
#define STBI_WRITE_NO_STDIO
#define STB_IMAGE_WRITE_STATIC
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image_write.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Qt-free software rasterizer for the web display list. It replays the primitives
// FigureCanvas draws (scanline polygons, lines as quads, stb_image rasters,
// stb_truetype text) at 2x and box-filters down for anti-aliasing. getframe and
// the web PNG export use it in WEB_GUI mode.
namespace {
    //=============================================================================
    struct RasterColor
    {
        unsigned char r = 0, g = 0, b = 0;
        double a = 1.0;
    };
    //=============================================================================
    RasterColor
    parseCssColor(const std::string& css)
    {
        RasterColor color;
        // "rgba(r,g,b,a)" | "rgb(r,g,b)" (what colorString emits)
        const size_t open = css.find('(');
        if (open == std::string::npos) {
            return color;
        }
        int r = 0, g = 0, b = 0;
        double a = 1.0;
#ifdef _MSC_VER
        sscanf_s(css.c_str() + open, "(%d,%d,%d,%lf", &r, &g, &b, &a);
#else
        sscanf(css.c_str() + open, "(%d,%d,%d,%lf", &r, &g, &b, &a);
#endif
        auto clamp255
            = [](int v) { return static_cast<unsigned char>(v < 0 ? 0 : (v > 255 ? 255 : v)); };
        color.r = clamp255(r);
        color.g = clamp255(g);
        color.b = clamp255(b);
        color.a = a < 0.0 ? 0.0 : (a > 1.0 ? 1.0 : a);
        return color;
    }
    //=============================================================================
    std::vector<unsigned char>
    base64Decode(const std::string& in)
    {
        static const std::string alphabet
            = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        std::vector<unsigned char> out;
        int value = 0;
        int bits = -8;
        for (char c : in) {
            const size_t pos = alphabet.find(c);
            if (pos == std::string::npos) {
                if (c == '=') {
                    break;
                }
                continue;
            }
            value = (value << 6) | static_cast<int>(pos);
            bits += 6;
            if (bits >= 0) {
                out.push_back(static_cast<unsigned char>((value >> bits) & 0xFF));
                bits -= 8;
            }
        }
        return out;
    }
    //=============================================================================
    std::string
    base64Encode(const unsigned char* data, size_t length)
    {
        static const char alphabet[]
            = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        std::string out;
        out.reserve(((length + 2) / 3) * 4);
        for (size_t i = 0; i < length; i += 3) {
            const unsigned int b0 = data[i];
            const unsigned int b1 = i + 1 < length ? data[i + 1] : 0;
            const unsigned int b2 = i + 2 < length ? data[i + 2] : 0;
            const unsigned int triple = (b0 << 16) | (b1 << 8) | b2;
            out.push_back(alphabet[(triple >> 18) & 0x3F]);
            out.push_back(alphabet[(triple >> 12) & 0x3F]);
            out.push_back(i + 1 < length ? alphabet[(triple >> 6) & 0x3F] : '=');
            out.push_back(i + 2 < length ? alphabet[triple & 0x3F] : '=');
        }
        return out;
    }
    //=============================================================================
    class RasterCanvas
    {
    public:
        int width;
        int height;
        std::vector<unsigned char> rgb; // row-major, opaque

        RasterCanvas(int w, int h) : width(w), height(h), rgb(static_cast<size_t>(w) * h * 3, 255)
        {
        }

        void
        blend(int x, int y, const RasterColor& c, double alphaScale = 1.0)
        {
            if (x < 0 || y < 0 || x >= width || y >= height) {
                return;
            }
            const double a = c.a * alphaScale;
            if (a <= 0.0) {
                return;
            }
            unsigned char* p = &rgb[(static_cast<size_t>(y) * width + x) * 3];
            p[0] = static_cast<unsigned char>(p[0] + (c.r - p[0]) * a);
            p[1] = static_cast<unsigned char>(p[1] + (c.g - p[1]) * a);
            p[2] = static_cast<unsigned char>(p[2] + (c.b - p[2]) * a);
        }

        void
        fillRect(double x, double y, double w, double h, const RasterColor& c)
        {
            const int x0 = std::max(0, static_cast<int>(std::floor(x)));
            const int y0 = std::max(0, static_cast<int>(std::floor(y)));
            const int x1 = std::min(width, static_cast<int>(std::ceil(x + w)));
            const int y1 = std::min(height, static_cast<int>(std::ceil(y + h)));
            for (int yy = y0; yy < y1; ++yy) {
                for (int xx = x0; xx < x1; ++xx) {
                    blend(xx, yy, c);
                }
            }
        }

        void
        fillPolygon(const std::vector<std::pair<double, double>>& pts, const RasterColor& c)
        {
            if (pts.size() < 3) {
                return;
            }
            double ymin = pts[0].second;
            double ymax = pts[0].second;
            for (const auto& p : pts) {
                ymin = std::min(ymin, p.second);
                ymax = std::max(ymax, p.second);
            }
            const int y0 = std::max(0, static_cast<int>(std::floor(ymin)));
            const int y1 = std::min(height - 1, static_cast<int>(std::ceil(ymax)));
            std::vector<double> xs;
            for (int yy = y0; yy <= y1; ++yy) {
                const double sy = yy + 0.5;
                xs.clear();
                for (size_t i = 0; i < pts.size(); ++i) {
                    const auto& p1 = pts[i];
                    const auto& p2 = pts[(i + 1) % pts.size()];
                    if ((p1.second <= sy && p2.second > sy)
                        || (p2.second <= sy && p1.second > sy)) {
                        const double t = (sy - p1.second) / (p2.second - p1.second);
                        xs.push_back(p1.first + t * (p2.first - p1.first));
                    }
                }
                std::sort(xs.begin(), xs.end());
                for (size_t i = 0; i + 1 < xs.size(); i += 2) {
                    const int xa = std::max(0, static_cast<int>(std::round(xs[i])));
                    const int xb = std::min(width - 1, static_cast<int>(std::round(xs[i + 1])));
                    for (int xx = xa; xx <= xb; ++xx) {
                        blend(xx, yy, c);
                    }
                }
            }
        }

        void
        thickSegment(double x1, double y1, double x2, double y2, double w, const RasterColor& c)
        {
            const double dx = x2 - x1;
            const double dy = y2 - y1;
            const double len = std::sqrt(dx * dx + dy * dy);
            if (len <= 1e-9) {
                fillRect(x1 - w / 2, y1 - w / 2, w, w, c);
                return;
            }
            const double nx = -dy / len * (w / 2);
            const double ny = dx / len * (w / 2);
            fillPolygon({ { x1 + nx, y1 + ny }, { x2 + nx, y2 + ny }, { x2 - nx, y2 - ny },
                            { x1 - nx, y1 - ny } },
                c);
        }

        void
        fillDisc(double cx, double cy, double r, const RasterColor& c)
        {
            if (r <= 0.0) {
                return;
            }
            const int x0 = std::max(0, static_cast<int>(std::floor(cx - r)));
            const int y0 = std::max(0, static_cast<int>(std::floor(cy - r)));
            const int x1 = std::min(width - 1, static_cast<int>(std::ceil(cx + r)));
            const int y1 = std::min(height - 1, static_cast<int>(std::ceil(cy + r)));
            const double r2 = r * r;
            for (int yy = y0; yy <= y1; ++yy) {
                const double ddy = (yy + 0.5) - cy;
                for (int xx = x0; xx <= x1; ++xx) {
                    const double ddx = (xx + 0.5) - cx;
                    if (ddx * ddx + ddy * ddy <= r2) {
                        blend(xx, yy, c);
                    }
                }
            }
        }

        // Dash-aware polyline stroke; dash array is in device pixels.
        void
        strokePolyline(const std::vector<std::pair<double, double>>& pts, double w,
            const RasterColor& c, const std::vector<double>& dash)
        {
            if (pts.size() < 2) {
                return;
            }
            if (dash.empty()) {
                for (size_t i = 0; i + 1 < pts.size(); ++i) {
                    thickSegment(
                        pts[i].first, pts[i].second, pts[i + 1].first, pts[i + 1].second, w, c);
                }
                // Round the joints of a thick polyline: butt-capped segments leave a
                // V-notch at each shared vertex, which reads as a cogged ring on a
                // segmented curve (a round marker outline). Thin lines (radius <= 1px)
                // are skipped, their notches are sub-pixel.
                const double radius = w / 2.0;
                if (radius > 1.0 && pts.size() > 2) {
                    const double dxClose = pts.front().first - pts.back().first;
                    const double dyClose = pts.front().second - pts.back().second;
                    const bool closed = (dxClose * dxClose + dyClose * dyClose) < 1e-6;
                    const size_t last = pts.size() - 1;
                    for (size_t i = 1; i < last; ++i) {
                        fillDisc(pts[i].first, pts[i].second, radius, c);
                    }
                    if (closed) {
                        fillDisc(pts.front().first, pts.front().second, radius, c);
                    }
                }
                return;
            }
            double dashPos = 0.0;
            size_t dashIndex = 0;
            bool penDown = true;
            for (size_t i = 0; i + 1 < pts.size(); ++i) {
                double sx = pts[i].first;
                double sy = pts[i].second;
                const double ex = pts[i + 1].first;
                const double ey = pts[i + 1].second;
                double remaining = std::hypot(ex - sx, ey - sy);
                const double ux = remaining > 1e-9 ? (ex - sx) / remaining : 0.0;
                const double uy = remaining > 1e-9 ? (ey - sy) / remaining : 0.0;
                while (remaining > 1e-9) {
                    const double dashLeft = dash[dashIndex % dash.size()] - dashPos;
                    const double step = std::min(remaining, dashLeft);
                    const double nxp = sx + ux * step;
                    const double nyp = sy + uy * step;
                    if (penDown) {
                        thickSegment(sx, sy, nxp, nyp, w, c);
                    }
                    sx = nxp;
                    sy = nyp;
                    remaining -= step;
                    dashPos += step;
                    if (dashPos >= dash[dashIndex % dash.size()] - 1e-9) {
                        dashPos = 0.0;
                        ++dashIndex;
                        penDown = !penDown;
                    }
                }
            }
        }
    };
    //=============================================================================
} // anonymous namespace
//=============================================================================
// PNG is lossless at every zlib level. These frames are interactive (streamed and
// re-encoded every frame), so favor encode speed over the default level 8: on
// incompressible content (imagesc of noise) the default costs several
// milliseconds per frame for no visual difference.
static void
configureInteractivePngEncoder()
{
    stbi_write_png_compression_level = 2;
    // Adaptive filtering (the default -1) tries all five PNG filters on every
    // scanline, which is O(5 * pixels) of extra work per frame for a few percent
    // of size. Force filter 0 (none).
    stbi_write_force_png_filter = 0;
}
//=============================================================================
bool
renderWebRgbaToPngBase64(
    const unsigned char* rgba, int width, int height, int stride, std::string& base64)
{
    if (!rgba || width <= 0 || height <= 0 || stride < width * 4) {
        return false;
    }
    configureInteractivePngEncoder();
    std::vector<unsigned char> png;
    auto sink = [](void* context, void* data, int size) {
        auto* output = static_cast<std::vector<unsigned char>*>(context);
        const auto* bytes = static_cast<const unsigned char*>(data);
        output->insert(output->end(), bytes, bytes + size);
    };
    if (!stbi_write_png_to_func(sink, &png, width, height, 4, rgba, stride)) {
        return false;
    }
    base64 = base64Encode(png.data(), png.size());
    return true;
}
//=============================================================================
static bool
renderWebRgbToPngBase64(
    const unsigned char* rgb, int width, int height, int stride, std::string& base64)
{
    if (!rgb || width <= 0 || height <= 0 || stride < width * 3) {
        return false;
    }
    configureInteractivePngEncoder();
    std::vector<unsigned char> png;
    auto sink = [](void* context, void* data, int size) {
        auto* output = static_cast<std::vector<unsigned char>*>(context);
        const auto* bytes = static_cast<const unsigned char*>(data);
        output->insert(output->end(), bytes, bytes + size);
    };
    if (!stbi_write_png_to_func(sink, &png, width, height, 3, rgb, stride)) {
        return false;
    }
    base64 = base64Encode(png.data(), png.size());
    return true;
}
//=============================================================================
// Baseline-anchored at (x, y) as putText emits it, with optional rotation about
// the anchor.
static void
rasterizeText(RasterCanvas& canvas, const WebDrawPrimitive& p, int supersample)
{
    // p.fontFamily is a CSS generic ("serif"/"sans-serif"/"monospace"); the
    // shared font engine resolves it back to the platform TTF.
    const std::wstring family(p.fontFamily.begin(), p.fontFamily.end());
    if (!FontMetrics::hasRealFont(family, p.fontBold, p.fontItalic)) {
        return;
    }
    const std::wstring text = utf8_to_wstring(p.text);
    if (text.empty()) {
        return;
    }
    const RasterColor color = parseCssColor(p.color);
    const double pixelSize = p.size * supersample;
    const double anchorX = p.x * supersample;
    const double anchorY = p.y * supersample;
    // Alignment (putText bakes it to "left"; direct emitters may not).
    double penStartX = 0.0;
    if (p.align == "center" || p.align == "right") {
        double total = 0.0;
        for (wchar_t c : text) {
            total
                += FontMetrics::glyphAdvancePixels(c, family, pixelSize, p.fontBold, p.fontItalic);
        }
        penStartX = p.align == "center" ? -total / 2.0 : -total;
    }
    const double rad = -p.rotation * M_PI / 180.0; // screen-space rotation
    const double cosr = std::cos(rad);
    const double sinr = std::sin(rad);
    double pen = penStartX;
    for (wchar_t c : text) {
        FontMetrics::GlyphBitmap glyph;
        if (FontMetrics::glyphBitmap(c, family, pixelSize, p.fontBold, p.fontItalic, glyph)
            && !glyph.alpha.empty()) {
            for (int yy = 0; yy < glyph.height; ++yy) {
                for (int xx = 0; xx < glyph.width; ++xx) {
                    const unsigned char alpha = glyph.alpha[yy * glyph.width + xx];
                    if (alpha == 0) {
                        continue;
                    }
                    const double lx = pen + glyph.xoffset + xx;
                    const double ly = glyph.yoffset + yy;
                    const double rx = anchorX + lx * cosr - ly * sinr;
                    const double ry = anchorY + lx * sinr + ly * cosr;
                    canvas.blend(static_cast<int>(std::lround(rx)),
                        static_cast<int>(std::lround(ry)), color, alpha / 255.0);
                }
            }
        }
        pen += FontMetrics::glyphAdvancePixels(c, family, pixelSize, p.fontBold, p.fontItalic);
    }
}
//=============================================================================
static void
rasterizeImage(RasterCanvas& canvas, const WebDrawPrimitive& p, int supersample)
{
    const std::string prefix = "base64,";
    const size_t at = p.image.find(prefix);
    if (at == std::string::npos) {
        return;
    }
    const std::vector<unsigned char> bytes = base64Decode(p.image.substr(at + prefix.size()));
    if (bytes.empty()) {
        return;
    }
    int iw = 0, ih = 0, channels = 0;
    unsigned char* pixels = stbi_load_from_memory(
        bytes.data(), static_cast<int>(bytes.size()), &iw, &ih, &channels, 4);
    if (pixels == nullptr) {
        return;
    }
    const double dx = p.x * supersample;
    const double dy = p.y * supersample;
    const double dw = p.w * supersample;
    const double dh = p.h * supersample;
    const int x0 = std::max(0, static_cast<int>(std::floor(dx)));
    const int y0 = std::max(0, static_cast<int>(std::floor(dy)));
    const int x1 = std::min(canvas.width, static_cast<int>(std::ceil(dx + dw)));
    const int y1 = std::min(canvas.height, static_cast<int>(std::ceil(dy + dh)));
    // The destination rect starts at floor(dx) / floor(dy), so the first row and
    // column map to a slightly negative source offset: clamp both ends, or a tall
    // raster reads rows before its buffer (one row per ~ih/dh pixels of offset).
    for (int yy = y0; yy < y1; ++yy) {
        const int sy = std::clamp(static_cast<int>((yy - dy) / dh * ih), 0, ih - 1);
        for (int xx = x0; xx < x1; ++xx) {
            const int sx = std::clamp(static_cast<int>((xx - dx) / dw * iw), 0, iw - 1);
            const unsigned char* sp = &pixels[(static_cast<size_t>(sy) * iw + sx) * 4];
            RasterColor c { sp[0], sp[1], sp[2], sp[3] / 255.0 };
            canvas.blend(xx, yy, c);
        }
    }
    stbi_image_free(pixels);
}
//=============================================================================
static RasterColor
controlColor(const std::string& css, const char* fallback)
{
    if (!css.empty()) {
        return parseCssColor(css);
    }
    return parseCssColor(fallback);
}
//=============================================================================
static void
rasterizeControlLabel(RasterCanvas& canvas, const WebUIControl& control, double x, double y,
    const std::string& align, const std::string& text, const RasterColor& color)
{
    if (text.empty()) {
        return;
    }
    WebDrawPrimitive p;
    p.kind = "text";
    p.x = x;
    p.y = y;
    p.text = text;
    p.align = align;
    p.color = "rgb(" + std::to_string(static_cast<int>(color.r)) + ","
        + std::to_string(static_cast<int>(color.g)) + ","
        + std::to_string(static_cast<int>(color.b)) + ")";
    p.size = control.fontSize > 0 ? control.fontSize : 12.0;
    p.fontFamily = "sans-serif";
    rasterizeText(canvas, p, 2);
}
//=============================================================================
static void
rasterizeControlBox(RasterCanvas& canvas, double x, double y, double w, double h,
    const RasterColor& fill, const RasterColor& stroke)
{
    canvas.fillRect(x, y, w, h, fill);
    canvas.strokePolyline(
        { { x, y }, { x + w, y }, { x + w, y + h }, { x, y + h }, { x, y } }, 1.0, stroke, {});
}
//=============================================================================
#include "RenderWebTableRaster.hpp"
//=============================================================================
// Rounded, filled and stroked rectangle in control space (coordinates are
// multiplied by supersample here), for the drop-down and text entries.
static void
rasterizeRoundedBox(RasterCanvas& canvas, double x, double y, double w, double h, double radius,
    const RasterColor& fill, const RasterColor& stroke, int supersample)
{
    const double r = std::max(0.0, std::min(radius, std::min(w, h) / 2.0));
    std::vector<std::pair<double, double>> path;
    auto arc = [&](double cx, double cy, double a0deg, double a1deg) {
        const int seg = 5;
        for (int i = 0; i <= seg; ++i) {
            const double a = (a0deg + (a1deg - a0deg) * i / seg) * M_PI / 180.0;
            path.push_back(
                { (cx + r * std::cos(a)) * supersample, (cy + r * std::sin(a)) * supersample });
        }
    };
    arc(x + r, y + r, 180.0, 270.0); // top-left
    arc(x + w - r, y + r, 270.0, 360.0); // top-right
    arc(x + w - r, y + h - r, 0.0, 90.0); // bottom-right
    arc(x + r, y + h - r, 90.0, 180.0); // bottom-left
    canvas.fillPolygon(path, fill);
    std::vector<std::pair<double, double>> outline = path;
    outline.push_back(path.front());
    canvas.strokePolyline(outline, 1.0 * supersample, stroke, {});
}
//=============================================================================
// Used for the drop-down chevron and the scrollbar end buttons.
static void
rasterizeArrow(RasterCanvas& canvas, double cx, double cy, double halfW, double halfH,
    char direction, const RasterColor& color, int supersample)
{
    std::vector<std::pair<double, double>> tri;
    if (direction == 'd') {
        tri = { { cx - halfW, cy - halfH }, { cx + halfW, cy - halfH }, { cx, cy + halfH } };
    } else if (direction == 'u') {
        tri = { { cx - halfW, cy + halfH }, { cx + halfW, cy + halfH }, { cx, cy - halfH } };
    } else if (direction == 'l') {
        tri = { { cx + halfW, cy - halfH }, { cx + halfW, cy + halfH }, { cx - halfW, cy } };
    } else {
        tri = { { cx - halfW, cy - halfH }, { cx - halfW, cy + halfH }, { cx + halfW, cy } };
    }
    for (auto& p : tri) {
        p.first *= supersample;
        p.second *= supersample;
    }
    canvas.fillPolygon(tri, color);
}
//=============================================================================
static std::string
firstControlText(const WebUIControl& control)
{
    if (!control.text.empty()) {
        return control.text;
    }
    if (!control.items.empty()) {
        return control.items.front();
    }
    return std::string();
}
//=============================================================================
// Value is a 1-based index into items for the menu-like styles. Clamp it back
// into range so a stale or unset Value never reads out of bounds.
static size_t
selectedItemIndex(const WebUIControl& control)
{
    if (control.items.empty()) {
        return 0;
    }
    if (control.value >= 1.0 && control.value <= static_cast<double>(control.items.size())) {
        return static_cast<size_t>(control.value) - 1;
    }
    return 0;
}
//=============================================================================
#include "RenderWebListboxRaster.hpp"
//=============================================================================
#include "RenderWebDialRaster.hpp"
//=============================================================================
#include "RenderWebSteppedControlsRaster.hpp"
//=============================================================================
#include "RenderWebTreeTextRaster.hpp"
//=============================================================================
static void
rasterizeControl(RasterCanvas& canvas, const WebUIControl& control, int supersample)
{
    const double x = control.x * supersample;
    const double y = control.y * supersample;
    const double w = control.w * supersample;
    const double h = control.h * supersample;
    if (w <= 0 || h <= 0) {
        return;
    }
    const RasterColor fg = controlColor(control.foreground, "rgb(0,0,0)");
    const RasterColor bg = controlColor(control.background, "rgb(232,232,232)");
    const RasterColor border = parseCssColor("rgb(143,143,143)");
    const std::string labelText = firstControlText(control);
    const double fontSize = std::max(8.0, control.fontSize > 0 ? control.fontSize : 12.0);
    const double baseline = (control.y + control.h / 2.0 + fontSize * 0.35);

    if (control.style == "text") {
        if (!control.background.empty()) {
            canvas.fillRect(x, y, w, h, bg);
        }
        rasterizeControlLabel(canvas, control, control.x + 2.0, baseline, "left", labelText, fg);
    } else if (control.style == "pushbutton" || control.style == "togglebutton") {
        const bool pressed = control.style == "togglebutton" && control.value >= control.maximum;
        rasterizeControlBox(
            canvas, x, y, w, h, pressed ? parseCssColor("rgb(207,207,207)") : bg, border);
        const std::string& hAlign = control.horizontalAlignment;
        const std::string& vAlign = control.verticalAlignment;
        const double labelX = hAlign == "left" ? control.x + 6.0
            : hAlign == "right"                ? control.x + control.w - 6.0
                                               : control.x + control.w / 2.0;
        const double labelY = vAlign == "top" ? control.y + 4.0 + fontSize * 0.9
            : vAlign == "bottom"              ? control.y + control.h - 4.0 - fontSize * 0.25
                                              : baseline;
        rasterizeControlLabel(canvas, control, labelX, labelY, hAlign, labelText, fg);
    } else if (control.style == "edit") {
        const RasterColor softBorder = parseCssColor("rgb(170,170,170)");
        rasterizeRoundedBox(canvas, control.x, control.y, control.w, control.h, 3.0,
            parseCssColor("rgb(255,255,255)"), softBorder, supersample);
        rasterizeEditText(canvas, control, labelText, fg, fontSize, baseline, supersample);
    } else if (control.style == "popupmenu") {
        const RasterColor softBorder = parseCssColor("rgb(170,170,170)");
        const double radius = std::min(6.0, control.h * 0.28);
        rasterizeRoundedBox(canvas, control.x, control.y, control.w, control.h, radius,
            parseCssColor("rgb(255,255,255)"), softBorder, supersample);
        std::string selectedText = labelText;
        if (!control.items.empty()) {
            selectedText = control.items[selectedItemIndex(control)];
        }
        rasterizeControlLabel(canvas, control, control.x + 8.0, baseline, "left", selectedText, fg);
        const double arrowW = std::max(7.0, std::min(control.h * 0.22, 10.0));
        const double acx = control.x + control.w - arrowW * 0.5 - 9.0;
        const double acy = control.y + control.h / 2.0;
        const double sepX = acx - arrowW * 0.5 - 6.0;
        canvas.strokePolyline(
            { { sepX * supersample, (control.y + 3.0) * supersample },
                { sepX * supersample, (control.y + control.h - 3.0) * supersample } },
            1.0 * supersample, parseCssColor("rgb(200,200,200)"), {});
        rasterizeArrow(canvas, acx, acy, arrowW * 0.5, arrowW * 0.32, 'd',
            parseCssColor("rgb(110,110,110)"), supersample);
    } else if (control.style == "listbox") {
        rasterizeListboxControl(canvas, control, fontSize, fg, supersample);
    } else if (control.style == "table") {
        rasterizeTableControl(canvas, control, supersample);
    } else if (control.style == "tree") {
        rasterizeTreeControl(canvas, control, fontSize, fg, supersample);
    } else if (control.style == "checkbox" || control.style == "radiobutton") {
        const double boxSize = std::max(4.0, std::min(control.h - 2.0, 14.0));
        const double bx = control.x + 1.0;
        const double by = control.y + (control.h - boxSize) / 2.0;
        if (control.style == "checkbox") {
            rasterizeControlBox(canvas, bx * supersample, by * supersample, boxSize * supersample,
                boxSize * supersample, parseCssColor("rgb(255,255,255)"),
                parseCssColor("rgb(102,102,102)"));
            if (control.value >= control.maximum) {
                const double x0 = bx + 2.0;
                const double y0 = by + boxSize * 0.55;
                const double x1 = bx + boxSize * 0.40;
                const double y1 = by + boxSize * 0.82;
                const double x2 = bx + boxSize - 2.0;
                const double y2 = by + boxSize * 0.18;
                canvas.strokePolyline({ { x0 * supersample, y0 * supersample },
                                          { x1 * supersample, y1 * supersample },
                                          { x2 * supersample, y2 * supersample } },
                    2.0 * supersample, parseCssColor("rgb(51,51,51)"), {});
            }
        } else {
            const double r = boxSize / 2.0;
            const double cx = bx + r;
            const double cy = by + r;
            const int steps = 24;
            std::vector<std::pair<double, double>> circle;
            circle.reserve(steps + 1);
            for (int k = 0; k <= steps; ++k) {
                const double a = (2.0 * M_PI * k) / steps;
                circle.push_back(
                    { (cx + std::cos(a) * r) * supersample, (cy + std::sin(a) * r) * supersample });
            }
            canvas.strokePolyline(circle, 1.0 * supersample, parseCssColor("rgb(102,102,102)"), {});
            if (control.value >= control.maximum) {
                canvas.fillRect((cx - r * 0.5) * supersample, (cy - r * 0.5) * supersample,
                    r * supersample, r * supersample, parseCssColor("rgb(51,51,51)"));
            }
        }
        rasterizeControlLabel(canvas, control, bx + boxSize + 4.0, baseline, "left", labelText, fg);
    } else if (control.style == "slider") {
        rasterizeSliderControl(canvas, control, supersample);
    } else if (control.style == "spinner") {
        rasterizeSpinnerControl(canvas, control, labelText, fg, baseline, supersample);
    } else if (control.style == "switch") {
        rasterizeSwitchControl(canvas, control, fontSize, supersample);
    } else if (control.style == "gauge") {
        rasterizeGaugeControl(canvas, control, supersample);
    } else if (control.style == "knob") {
        rasterizeKnobControl(canvas, control, supersample);
    } else if (control.style == "lamp") {
        // Only the disc (grey rim, Color fill): the corners keep the parent colour.
        const double r = std::min(w, h) / 2.0 - supersample;
        canvas.fillDisc(x + w / 2.0, y + h / 2.0, r, parseCssColor("rgb(120,120,120)"));
        canvas.fillDisc(x + w / 2.0, y + h / 2.0, r - supersample, bg);
    } else if (control.style == "frame") {
        rasterizeControlBox(canvas, x, y, w, h, bg, border);
    } else if (control.style == "panel" || control.style == "tabgroup") {
        // A container draws its own background, border and tab bar into the display
        // list, under its children. A box here would cover them; the control entry
        // only makes the container interactive in the browser.
        return;
    } else {
        rasterizeControlBox(canvas, x, y, w, h, parseCssColor("rgb(255,255,255)"), border);
        rasterizeControlLabel(canvas, control, control.x + 4.0, baseline, "left", labelText, fg);
    }
}
//=============================================================================
static bool
rasterizeWebPrimitivesImpl(const std::vector<WebDrawPrimitive>& primitives,
    const std::vector<WebUIControl>* controls, int width, int height,
    std::vector<unsigned char>& rgbOut)
{
    if (width <= 0 || height <= 0) {
        return false;
    }
    constexpr int SS = 2; // supersampling factor (anti-aliasing)
    RasterCanvas canvas(width * SS, height * SS);
    auto scaledDash = [](const std::vector<double>& dash) {
        std::vector<double> out;
        out.reserve(dash.size());
        for (double d : dash) {
            out.push_back(d * SS);
        }
        return out;
    };
    for (const WebDrawPrimitive& p : primitives) {
        if (p.kind == "rect") {
            canvas.fillRect(p.x * SS, p.y * SS, p.w * SS, p.h * SS, parseCssColor(p.color));
        } else if (p.kind == "line") {
            canvas.strokePolyline({ { p.x1 * SS, p.y1 * SS }, { p.x2 * SS, p.y2 * SS } },
                std::max(1.0, p.width) * SS, parseCssColor(p.color), scaledDash(p.dash));
        } else if (p.kind == "segments") {
            for (size_t index = 0; index + 1 < p.points.size(); index += 2) {
                canvas.strokePolyline(
                    { { p.points[index].first * SS, p.points[index].second * SS },
                        { p.points[index + 1].first * SS, p.points[index + 1].second * SS } },
                    std::max(1.0, p.width) * SS, parseCssColor(p.color), scaledDash(p.dash));
            }
        } else if (p.kind == "polyline") {
            std::vector<std::pair<double, double>> pts;
            pts.reserve(p.points.size());
            for (const auto& pt : p.points) {
                pts.push_back({ pt.first * SS, pt.second * SS });
            }
            canvas.strokePolyline(
                pts, std::max(1.0, p.width) * SS, parseCssColor(p.color), scaledDash(p.dash));
        } else if (p.kind == "polygon") {
            std::vector<std::pair<double, double>> pts;
            pts.reserve(p.points.size());
            for (const auto& pt : p.points) {
                pts.push_back({ pt.first * SS, pt.second * SS });
            }
            canvas.fillPolygon(pts, parseCssColor(p.color));
            if (!p.stroke.empty()) {
                std::vector<std::pair<double, double>> outline = pts;
                if (!outline.empty()) {
                    outline.push_back(outline.front()); // close
                }
                canvas.strokePolyline(
                    outline, std::max(1.0, p.width) * SS, parseCssColor(p.stroke), {});
            }
        } else if (p.kind == "image") {
            rasterizeImage(canvas, p, SS);
        } else if (p.kind == "text") {
            rasterizeText(canvas, p, SS);
        } else if (p.kind == "points") {
            // Keeps this raster (getImage / saveAs / print / WebGL software fallback)
            // in step with the frontend canvas, which draws the same batch.
            const size_t pointCount = p.pointRadii.size();
            const bool hasFace = p.pointColors.size() == pointCount * 4;
            const bool hasEdge = p.pointEdgeColors.size() == pointCount * 4;
            const double edgeWidth = std::max(1.0, p.pointEdgeWidth) * SS;
            std::vector<std::pair<double, double>> poly;
            std::vector<std::array<double, 4>> segs;
            auto colorAt = [](const std::vector<uint8_t>& src, size_t i) {
                RasterColor c;
                c.r = src[i * 4];
                c.g = src[i * 4 + 1];
                c.b = src[i * 4 + 2];
                c.a = src[i * 4 + 3] / 255.0;
                return c;
            };
            for (size_t i = 0; i < pointCount; ++i) {
                const double cx = p.pointCenters[i * 2] * SS;
                const double cy = p.pointCenters[i * 2 + 1] * SS;
                const double r = p.pointRadii[i] * SS;
                const MarkerGeom geom = markerShapeGeometry(p.pointShape, cx, cy, r, poly, segs);
                if (geom == MarkerGeom::Disc) {
                    if (hasFace) {
                        canvas.fillDisc(cx, cy, r, colorAt(p.pointColors, i));
                    }
                    if (hasEdge) {
                        std::vector<std::pair<double, double>> ring;
                        for (int k = 0; k <= 24; ++k) {
                            const double a = 2.0 * 3.14159265358979323846 * k / 24.0;
                            ring.emplace_back(cx + r * std::cos(a), cy + r * std::sin(a));
                        }
                        canvas.strokePolyline(ring, edgeWidth, colorAt(p.pointEdgeColors, i), {});
                    }
                } else if (geom == MarkerGeom::Polygon) {
                    if (hasFace) {
                        canvas.fillPolygon(poly, colorAt(p.pointColors, i));
                    }
                    if (hasEdge) {
                        std::vector<std::pair<double, double>> ring = poly;
                        ring.push_back(poly.front());
                        canvas.strokePolyline(ring, edgeWidth, colorAt(p.pointEdgeColors, i), {});
                    }
                } else {
                    const RasterColor sc
                        = hasEdge ? colorAt(p.pointEdgeColors, i) : colorAt(p.pointColors, i);
                    for (const auto& s : segs) {
                        canvas.strokePolyline(
                            { { s[0], s[1] }, { s[2], s[3] } }, edgeWidth, sc, {});
                    }
                }
            }
        } else if (p.kind == "rects") {
            const size_t rectCount = p.rectXYWH.size() / 4;
            const bool hasFill = !p.color.empty();
            const bool hasStroke = !p.stroke.empty();
            const RasterColor fillColor = hasFill ? parseCssColor(p.color) : RasterColor {};
            const RasterColor strokeColor = hasStroke ? parseCssColor(p.stroke) : RasterColor {};
            const double strokeW = std::max(1.0, p.width) * SS;
            for (size_t i = 0; i < rectCount; i++) {
                const double x = p.rectXYWH[i * 4] * SS;
                const double y = p.rectXYWH[i * 4 + 1] * SS;
                const double w = p.rectXYWH[i * 4 + 2] * SS;
                const double h = p.rectXYWH[i * 4 + 3] * SS;
                if (hasFill) {
                    canvas.fillRect(x, y, w, h, fillColor);
                }
                if (hasStroke) {
                    canvas.strokePolyline(
                        { { x, y }, { x + w, y }, { x + w, y + h }, { x, y + h }, { x, y } },
                        strokeW, strokeColor, {});
                }
            }
        }
    }
    if (controls != nullptr) {
        for (const WebUIControl& control : *controls) {
            rasterizeControl(canvas, control, SS);
        }
    }
    rgbOut.resize(static_cast<size_t>(width) * height * 3);
    for (int y = 0; y < height; ++y) {
        for (int x = 0; x < width; ++x) {
            int sums[3] = { 0, 0, 0 };
            for (int sy = 0; sy < SS; ++sy) {
                const unsigned char* row
                    = &canvas.rgb[(static_cast<size_t>(y * SS + sy) * canvas.width + x * SS) * 3];
                for (int sx = 0; sx < SS; ++sx) {
                    sums[0] += row[sx * 3];
                    sums[1] += row[sx * 3 + 1];
                    sums[2] += row[sx * 3 + 2];
                }
            }
            unsigned char* out = &rgbOut[(static_cast<size_t>(y) * width + x) * 3];
            out[0] = static_cast<unsigned char>(sums[0] / (SS * SS));
            out[1] = static_cast<unsigned char>(sums[1] / (SS * SS));
            out[2] = static_cast<unsigned char>(sums[2] / (SS * SS));
        }
    }
    return true;
}
//=============================================================================
NLSRENDERWEB_IMPEXP bool
rasterizeWebPrimitives(const std::vector<WebDrawPrimitive>& primitives, int width, int height,
    std::vector<unsigned char>& rgbOut)
{
    return rasterizeWebPrimitivesImpl(primitives, nullptr, width, height, rgbOut);
}
//=============================================================================
NLSRENDERWEB_IMPEXP bool
rasterizeWebPrimitivesAndControls(const std::vector<WebDrawPrimitive>& primitives,
    const std::vector<WebUIControl>& controls, int width, int height,
    std::vector<unsigned char>& rgbOut)
{
    return rasterizeWebPrimitivesImpl(primitives, &controls, width, height, rgbOut);
}
//=============================================================================
NLSRENDERWEB_IMPEXP bool
rasterizeWebPrimitivesToPngBase64(
    const std::vector<WebDrawPrimitive>& primitives, int width, int height, std::string& base64Out)
{
    std::vector<unsigned char> rgb;
    if (!rasterizeWebPrimitives(primitives, width, height, rgb)) {
        return false;
    }
    return renderWebRgbToPngBase64(rgb.data(), width, height, width * 3, base64Out);
}
//=============================================================================
NLSRENDERWEB_IMPEXP bool
rasterizeWebPrimitivesAndControlsToPngBase64(const std::vector<WebDrawPrimitive>& primitives,
    const std::vector<WebUIControl>& controls, int width, int height, std::string& base64Out)
{
    std::vector<unsigned char> rgb;
    if (!rasterizeWebPrimitivesAndControls(primitives, controls, width, height, rgb)) {
        return false;
    }
    return renderWebRgbToPngBase64(rgb.data(), width, height, width * 3, base64Out);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
