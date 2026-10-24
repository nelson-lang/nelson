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
#include "RenderWebMarkerGeometry.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <cmath>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <sstream>
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
    xmlEscape(const std::string& in)
    {
        std::string out;
        out.reserve(in.size());
        for (char c : in) {
            switch (c) {
            case '&':
                out += "&amp;";
                break;
            case '<':
                out += "&lt;";
                break;
            case '>':
                out += "&gt;";
                break;
            case '"':
                out += "&quot;";
                break;
            default:
                out += c;
                break;
            }
        }
        return out;
    }
    //=============================================================================
    // "rgba(r,g,b,a)" for the i-th packed colour, in the format of
    // RenderWeb::colorString.
    std::string
    batchColorCss(const std::vector<uint8_t>& rgba, size_t i)
    {
        return "rgba(" + std::to_string(static_cast<int>(rgba[i * 4])) + ","
            + std::to_string(static_cast<int>(rgba[i * 4 + 1])) + ","
            + std::to_string(static_cast<int>(rgba[i * 4 + 2])) + ","
            + std::to_string(rgba[i * 4 + 3] / 255.0) + ")";
    }
    //=============================================================================
    RasterColor
    batchRasterColor(const std::vector<uint8_t>& rgba, size_t i)
    {
        RasterColor color;
        color.r = rgba[i * 4];
        color.g = rgba[i * 4 + 1];
        color.b = rgba[i * 4 + 2];
        color.a = rgba[i * 4 + 3] / 255.0;
        return color;
    }
    //=============================================================================
    // One SVG element per marker of a "points" batch, with the geometry of the
    // software raster (markerShapeGeometry). A face-less shape (plus, cross,
    // asterisk) is stroked in the outline colour, or in the face colour when only
    // that one was given.
    void
    appendSvgMarkers(std::ostringstream& svg, const WebDrawPrimitive& p)
    {
        const size_t pointCount = p.pointRadii.size();
        const bool hasFace = p.pointColors.size() == pointCount * 4;
        const bool hasEdge = p.pointEdgeColors.size() == pointCount * 4;
        if (!hasFace && !hasEdge) {
            return;
        }
        const double edgeWidth = std::max(1.0, p.pointEdgeWidth);
        std::vector<std::pair<double, double>> poly;
        std::vector<std::array<double, 4>> segs;
        for (size_t i = 0; i < pointCount; ++i) {
            const double cx = p.pointCenters[i * 2];
            const double cy = p.pointCenters[i * 2 + 1];
            const double r = p.pointRadii[i];
            const MarkerGeom geom = markerShapeGeometry(p.pointShape, cx, cy, r, poly, segs);
            if (geom == MarkerGeom::Strokes) {
                const std::string stroke
                    = batchColorCss(hasEdge ? p.pointEdgeColors : p.pointColors, i);
                for (const auto& s : segs) {
                    svg << "<line x1=\"" << s[0] << "\" y1=\"" << s[1] << "\" x2=\"" << s[2]
                        << "\" y2=\"" << s[3] << "\" stroke=\"" << stroke << "\" stroke-width=\""
                        << edgeWidth << "\"/>\n";
                }
                continue;
            }
            if (geom == MarkerGeom::Disc) {
                svg << "<circle cx=\"" << cx << "\" cy=\"" << cy << "\" r=\"" << r << "\"";
            } else {
                svg << "<polygon points=\"";
                for (const auto& pt : poly) {
                    svg << pt.first << "," << pt.second << " ";
                }
                svg << "\"";
            }
            svg << " fill=\"" << (hasFace ? batchColorCss(p.pointColors, i) : std::string("none"))
                << "\"";
            if (hasEdge) {
                svg << " stroke=\"" << batchColorCss(p.pointEdgeColors, i) << "\" stroke-width=\""
                    << edgeWidth << "\"";
            }
            svg << "/>\n";
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
NLSRENDERWEB_IMPEXP bool
webPrimitivesToSvgFile(const std::vector<WebDrawPrimitive>& primitives, int width, int height,
    const std::wstring& filename, double displayScale)
{
    // The viewBox carries the internal (export DPI) resolution so every coordinate
    // maps 1:1. The declared width/height are the logical figure size, so a viewer
    // shows the content at its intended size with the extra resolution kept as
    // sub-pixel detail. Both come from the same factor, so their ratio matches the
    // viewBox.
    const double declaredWidth = width * displayScale;
    const double declaredHeight = height * displayScale;
    std::ostringstream svg;
    svg << "<svg width=\"" << declaredWidth << "\" height=\"" << declaredHeight
        << "\" viewBox=\"0 0 " << width << " " << height
        << "\" xmlns=\"http://www.w3.org/2000/svg\">\n";
    svg << "<rect x=\"0\" y=\"0\" width=\"" << width << "\" height=\"" << height
        << "\" fill=\"#ffffff\"/>\n";
    auto dashAttr = [](const std::vector<double>& dash) {
        if (dash.empty()) {
            return std::string();
        }
        std::string out = " stroke-dasharray=\"";
        for (size_t i = 0; i < dash.size(); ++i) {
            if (i > 0) {
                out += ",";
            }
            out += std::to_string(dash[i]);
        }
        return out + "\"";
    };
    for (const WebDrawPrimitive& p : primitives) {
        if (p.kind == "rect") {
            svg << "<rect x=\"" << p.x << "\" y=\"" << p.y << "\" width=\"" << p.w << "\" height=\""
                << p.h << "\" fill=\"" << p.color << "\"/>\n";
        } else if (p.kind == "line") {
            svg << "<line x1=\"" << p.x1 << "\" y1=\"" << p.y1 << "\" x2=\"" << p.x2 << "\" y2=\""
                << p.y2 << "\" stroke=\"" << p.color << "\" stroke-width=\""
                << std::max(1.0, p.width) << "\"" << dashAttr(p.dash) << "/>\n";
        } else if (p.kind == "segments") {
            for (size_t index = 0; index + 1 < p.points.size(); index += 2) {
                svg << "<line x1=\"" << p.points[index].first << "\" y1=\""
                    << p.points[index].second << "\" x2=\"" << p.points[index + 1].first
                    << "\" y2=\"" << p.points[index + 1].second << "\" stroke=\"" << p.color
                    << "\" stroke-width=\"" << std::max(1.0, p.width) << "\"" << dashAttr(p.dash)
                    << "/>\n";
            }
        } else if (p.kind == "polyline" || p.kind == "polygon") {
            svg << "<" << p.kind << " points=\"";
            for (const auto& pt : p.points) {
                svg << pt.first << "," << pt.second << " ";
            }
            svg << "\"";
            if (p.kind == "polyline") {
                svg << " fill=\"none\" stroke=\"" << p.color << "\" stroke-width=\""
                    << std::max(1.0, p.width) << "\"" << dashAttr(p.dash);
            } else {
                svg << " fill=\"" << p.color << "\"";
                if (!p.stroke.empty()) {
                    svg << " stroke=\"" << p.stroke << "\" stroke-width=\""
                        << std::max(1.0, p.width) << "\"";
                }
            }
            svg << "/>\n";
        } else if (p.kind == "image") {
            // image-rendering:pixelated so a small CData raster (imagesc/image/
            // heatmap cells) upscales as crisp flat blocks, not smoothed.
            svg << "<image x=\"" << p.x << "\" y=\"" << p.y << "\" width=\"" << p.w
                << "\" height=\"" << p.h
                << "\" preserveAspectRatio=\"none\" image-rendering=\"pixelated\" href=\""
                << p.image << "\"/>\n";
        } else if (p.kind == "text") {
            const char* anchor
                = p.align == "center" ? "middle" : (p.align == "right" ? "end" : "start");
            svg << "<text ";
            if (p.rotation != 0.0) {
                svg << "x=\"0\" y=\"0\" transform=\"translate(" << p.x << "," << p.y << ") rotate("
                    << -p.rotation << ")\"";
            } else {
                svg << "x=\"" << p.x << "\" y=\"" << p.y << "\"";
            }
            svg << " fill=\"" << p.color << "\" font-size=\"" << p.size << "\" font-family=\""
                << p.fontFamily << "\"";
            if (p.fontBold) {
                svg << " font-weight=\"bold\"";
            }
            if (p.fontItalic) {
                svg << " font-style=\"italic\"";
            }
            svg << " text-anchor=\"" << anchor << "\">" << xmlEscape(p.text) << "</text>\n";
        } else if (p.kind == "points") {
            appendSvgMarkers(svg, p);
        } else if (p.kind == "rects") {
            // Batched bar/histogram rectangles with one shared style: set once on a
            // <g>, then a bare <rect> per bar (keeps a dense histogram small).
            const size_t rectCount = p.rectXYWH.size() / 4;
            if (rectCount != 0 && (!p.color.empty() || !p.stroke.empty())) {
                svg << "<g fill=\"" << (p.color.empty() ? std::string("none") : p.color) << "\"";
                if (!p.stroke.empty()) {
                    svg << " stroke=\"" << p.stroke << "\" stroke-width=\""
                        << std::max(1.0, p.width) << "\"";
                }
                svg << ">\n";
                for (size_t i = 0; i < rectCount; ++i) {
                    svg << "<rect x=\"" << p.rectXYWH[i * 4] << "\" y=\"" << p.rectXYWH[i * 4 + 1]
                        << "\" width=\"" << p.rectXYWH[i * 4 + 2] << "\" height=\""
                        << p.rectXYWH[i * 4 + 3] << "\"/>\n";
                }
                svg << "</g>\n";
            }
        }
    }
    svg << "</svg>\n";
    std::ofstream out(std::filesystem::path(filename), std::ios::binary | std::ios::trunc);
    if (!out.is_open()) {
        return false;
    }
    const std::string markup = svg.str();
    out.write(markup.data(), static_cast<std::streamsize>(markup.size()));
    return out.good();
}
//=============================================================================
namespace {
    //=============================================================================
    struct PdfImage
    {
        int width = 0;
        int height = 0;
        std::vector<unsigned char> zrgb;
    };
    //=============================================================================
    struct PdfScene
    {
        explicit PdfScene(double pageHeight) : height(pageHeight)
        {
            content << std::fixed << std::setprecision(4) << "0.75 0 0 0.75 0 0 cm\n";
        }
        double height;
        std::ostringstream content;
        std::vector<PdfImage> images;
        std::vector<std::string> fonts;
    };
    //=============================================================================
    std::string
    pdfColor(const RasterColor& color, const char* operation)
    {
        std::ostringstream output;
        output << color.r / 255.0 << " " << color.g / 255.0 << " " << color.b / 255.0 << " "
               << operation << "\n";
        return output.str();
    }
    //=============================================================================
    std::string
    pdfEscape(const std::string& utf8)
    {
        std::string output;
        for (wchar_t character : utf8_to_wstring(utf8)) {
            if (character == L'(' || character == L')' || character == L'\\') {
                output += '\\';
                output += static_cast<char>(character);
            } else if (character >= 32 && character < 256) {
                output += static_cast<char>(character);
            } else {
                output += '?';
            }
        }
        return output;
    }
    //=============================================================================
    size_t
    pdfFont(PdfScene& scene, const WebDrawPrimitive& primitive)
    {
        const bool bold = primitive.fontBold;
        const bool italic = primitive.fontItalic;
        std::string name;
        if (primitive.fontFamily == "serif") {
            name = bold ? (italic ? "Times-BoldItalic" : "Times-Bold")
                        : (italic ? "Times-Italic" : "Times-Roman");
        } else if (primitive.fontFamily == "monospace") {
            name = bold ? (italic ? "Courier-BoldOblique" : "Courier-Bold")
                        : (italic ? "Courier-Oblique" : "Courier");
        } else {
            name = bold ? (italic ? "Helvetica-BoldOblique" : "Helvetica-Bold")
                        : (italic ? "Helvetica-Oblique" : "Helvetica");
        }
        auto found = std::find(scene.fonts.begin(), scene.fonts.end(), name);
        if (found != scene.fonts.end()) {
            return static_cast<size_t>(found - scene.fonts.begin()) + 1;
        }
        scene.fonts.push_back(name);
        return scene.fonts.size();
    }
    //=============================================================================
    bool
    appendPdfPath(PdfScene& scene, const WebDrawPrimitive& primitive)
    {
        auto& output = scene.content;
        if (primitive.kind == "rect") {
            output << pdfColor(parseCssColor(primitive.color), "rg") << primitive.x << " "
                   << scene.height - primitive.y - primitive.h << " " << primitive.w << " "
                   << primitive.h << " re f\n";
            return true;
        }
        if (primitive.kind == "rects") {
            // Batched bar/histogram rectangles: the shared colour/line state is set
            // once, then one `re` per bar with the paint operator (fill, stroke or both).
            const size_t rectCount = primitive.rectXYWH.size() / 4;
            const bool hasFill = !primitive.color.empty();
            const bool hasStroke = !primitive.stroke.empty();
            if (rectCount == 0 || (!hasFill && !hasStroke)) {
                return true;
            }
            if (hasFill) {
                output << pdfColor(parseCssColor(primitive.color), "rg");
            }
            if (hasStroke) {
                output << pdfColor(parseCssColor(primitive.stroke), "RG")
                       << std::max(0.25, primitive.width) << " w\n";
            }
            const char* op = (hasFill && hasStroke) ? "B" : (hasFill ? "f" : "S");
            for (size_t i = 0; i < rectCount; ++i) {
                const double x = primitive.rectXYWH[i * 4];
                const double y = primitive.rectXYWH[i * 4 + 1];
                const double w = primitive.rectXYWH[i * 4 + 2];
                const double h = primitive.rectXYWH[i * 4 + 3];
                output << x << " " << scene.height - y - h << " " << w << " " << h << " re " << op
                       << "\n";
            }
            return true;
        }
        if (primitive.kind == "polygon") {
            if (primitive.points.size() < 3) {
                return true;
            }
            output << pdfColor(parseCssColor(primitive.color), "rg");
            const bool stroked = !primitive.stroke.empty();
            if (stroked) {
                output << pdfColor(parseCssColor(primitive.stroke), "RG")
                       << std::max(0.25, primitive.width) << " w\n";
            }
            output << primitive.points[0].first << " " << scene.height - primitive.points[0].second
                   << " m\n";
            for (size_t index = 1; index < primitive.points.size(); ++index) {
                output << primitive.points[index].first << " "
                       << scene.height - primitive.points[index].second << " l\n";
            }
            output << "h " << (stroked ? "B*" : "f*") << "\n";
            return true;
        }
        if (primitive.kind != "line" && primitive.kind != "segments"
            && primitive.kind != "polyline") {
            return false;
        }
        output << pdfColor(parseCssColor(primitive.color), "RG") << std::max(0.25, primitive.width)
               << " w\n";
        if (!primitive.dash.empty()) {
            output << "[";
            for (double distance : primitive.dash) {
                output << distance << " ";
            }
            output << "] 0 d\n";
        }
        if (primitive.kind == "line") {
            output << primitive.x1 << " " << scene.height - primitive.y1 << " m " << primitive.x2
                   << " " << scene.height - primitive.y2 << " l S\n";
        } else if (primitive.kind == "segments") {
            for (size_t index = 0; index + 1 < primitive.points.size(); index += 2) {
                output << primitive.points[index].first << " "
                       << scene.height - primitive.points[index].second << " m "
                       << primitive.points[index + 1].first << " "
                       << scene.height - primitive.points[index + 1].second << " l S\n";
            }
        } else if (!primitive.points.empty()) {
            output << primitive.points[0].first << " " << scene.height - primitive.points[0].second
                   << " m\n";
            for (size_t index = 1; index < primitive.points.size(); ++index) {
                output << primitive.points[index].first << " "
                       << scene.height - primitive.points[index].second << " l\n";
            }
            output << "S\n";
        }
        if (!primitive.dash.empty()) {
            output << "[] 0 d\n";
        }
        return true;
    }
    //=============================================================================
    // One path per marker of a "points" batch, with the same geometry as the
    // raster and the SVG writer. The geometry is built in screen space (y down)
    // and every y is flipped into PDF space; a disc is four cubic Bezier arcs.
    void
    appendPdfMarkers(PdfScene& scene, const WebDrawPrimitive& p)
    {
        const size_t pointCount = p.pointRadii.size();
        const bool hasFace = p.pointColors.size() == pointCount * 4;
        const bool hasEdge = p.pointEdgeColors.size() == pointCount * 4;
        if (!hasFace && !hasEdge) {
            return;
        }
        auto& output = scene.content;
        const double H = scene.height;
        output << std::max(0.25, p.pointEdgeWidth) << " w\n";
        const char* paint = hasFace ? (hasEdge ? "B" : "f") : "S";
        std::vector<std::pair<double, double>> poly;
        std::vector<std::array<double, 4>> segs;
        for (size_t i = 0; i < pointCount; ++i) {
            const double cx = p.pointCenters[i * 2];
            const double cy = p.pointCenters[i * 2 + 1];
            const double r = p.pointRadii[i];
            const MarkerGeom geom = markerShapeGeometry(p.pointShape, cx, cy, r, poly, segs);
            if (geom == MarkerGeom::Strokes) {
                output << pdfColor(
                    batchRasterColor(hasEdge ? p.pointEdgeColors : p.pointColors, i), "RG");
                for (const auto& s : segs) {
                    output << s[0] << " " << H - s[1] << " m " << s[2] << " " << H - s[3]
                           << " l S\n";
                }
                continue;
            }
            if (hasFace) {
                output << pdfColor(batchRasterColor(p.pointColors, i), "rg");
            }
            if (hasEdge) {
                output << pdfColor(batchRasterColor(p.pointEdgeColors, i), "RG");
            }
            if (geom == MarkerGeom::Disc) {
                const double k = 0.5523 * r;
                const double y = H - cy;
                output << cx + r << " " << y << " m\n"
                       << cx + r << " " << y + k << " " << cx + k << " " << y + r << " " << cx
                       << " " << y + r << " c\n"
                       << cx - k << " " << y + r << " " << cx - r << " " << y + k << " " << cx - r
                       << " " << y << " c\n"
                       << cx - r << " " << y - k << " " << cx - k << " " << y - r << " " << cx
                       << " " << y - r << " c\n"
                       << cx + k << " " << y - r << " " << cx + r << " " << y - k << " " << cx + r
                       << " " << y << " c\n";
            } else {
                output << poly[0].first << " " << H - poly[0].second << " m\n";
                for (size_t index = 1; index < poly.size(); ++index) {
                    output << poly[index].first << " " << H - poly[index].second << " l\n";
                }
            }
            output << "h " << paint << "\n";
        }
    }
    //=============================================================================
    void
    appendPdfImage(PdfScene& scene, const WebDrawPrimitive& primitive)
    {
        const size_t position = primitive.image.find("base64,");
        if (position == std::string::npos) {
            return;
        }
        const std::vector<unsigned char> bytes = base64Decode(primitive.image.substr(position + 7));
        int width = 0;
        int height = 0;
        int channels = 0;
        unsigned char* pixels = bytes.empty()
            ? nullptr
            : stbi_load_from_memory(
                  bytes.data(), static_cast<int>(bytes.size()), &width, &height, &channels, 3);
        if (!pixels) {
            return;
        }
        int compressedSize = 0;
        unsigned char* compressed
            = stbi_zlib_compress(pixels, width * height * 3, &compressedSize, 8);
        stbi_image_free(pixels);
        if (!compressed) {
            return;
        }
        PdfImage image;
        image.width = width;
        image.height = height;
        image.zrgb.assign(compressed, compressed + compressedSize);
        STBIW_FREE(compressed);
        scene.images.push_back(std::move(image));
        scene.content << "q " << primitive.w << " 0 0 " << primitive.h << " " << primitive.x << " "
                      << scene.height - primitive.y - primitive.h << " cm /Im"
                      << scene.images.size() << " Do Q\n";
    }
    //=============================================================================
    void
    appendPdfText(PdfScene& scene, const WebDrawPrimitive& primitive)
    {
        if (primitive.text.empty()) {
            return;
        }
        scene.content << pdfColor(parseCssColor(primitive.color), "rg") << "BT /F"
                      << pdfFont(scene, primitive) << " " << primitive.size << " Tf\n";
        const double x = primitive.x;
        const double y = scene.height - primitive.y;
        if (primitive.rotation != 0.0) {
            const double radians = primitive.rotation * M_PI / 180.0;
            scene.content << std::cos(radians) << " " << std::sin(radians) << " "
                          << -std::sin(radians) << " " << std::cos(radians) << " " << x << " " << y
                          << " Tm\n";
        } else {
            scene.content << "1 0 0 1 " << x << " " << y << " Tm\n";
        }
        scene.content << "(" << pdfEscape(primitive.text) << ") Tj ET\n";
    }
    //=============================================================================
    PdfScene
    buildPdfScene(const std::vector<WebDrawPrimitive>& primitives, int height)
    {
        PdfScene scene(height);
        for (const WebDrawPrimitive& primitive : primitives) {
            if (appendPdfPath(scene, primitive)) {
                continue;
            }
            if (primitive.kind == "image") {
                appendPdfImage(scene, primitive);
            } else if (primitive.kind == "text") {
                appendPdfText(scene, primitive);
            } else if (primitive.kind == "points") {
                appendPdfMarkers(scene, primitive);
            }
        }
        return scene;
    }
    //=============================================================================
    std::string
    assemblePdf(PdfScene& scene, int width, int height)
    {
        const std::string content = scene.content.str();
        std::ostringstream pdf;
        std::vector<size_t> offsets;
        auto beginObject = [&](size_t number) {
            offsets.resize(std::max(offsets.size(), number));
            offsets[number - 1] = static_cast<size_t>(pdf.tellp());
            pdf << number << " 0 obj\n";
        };
        pdf << "%PDF-1.4\n";
        beginObject(1);
        pdf << "<< /Type /Catalog /Pages 2 0 R >>\nendobj\n";
        beginObject(2);
        pdf << "<< /Type /Pages /Kids [3 0 R] /Count 1 >>\nendobj\n";
        const size_t firstFont = 5;
        const size_t firstImage = firstFont + scene.fonts.size();
        beginObject(3);
        pdf << "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 " << width * 0.75 << " "
            << height * 0.75 << "] /Contents 4 0 R /Resources <<";
        if (!scene.fonts.empty()) {
            pdf << " /Font <<";
            for (size_t index = 0; index < scene.fonts.size(); ++index) {
                pdf << " /F" << index + 1 << " " << firstFont + index << " 0 R";
            }
            pdf << " >>";
        }
        if (!scene.images.empty()) {
            pdf << " /XObject <<";
            for (size_t index = 0; index < scene.images.size(); ++index) {
                pdf << " /Im" << index + 1 << " " << firstImage + index << " 0 R";
            }
            pdf << " >>";
        }
        pdf << " >> >>\nendobj\n";
        beginObject(4);
        pdf << "<< /Length " << content.size() << " >>\nstream\n"
            << content << "endstream\nendobj\n";
        for (size_t index = 0; index < scene.fonts.size(); ++index) {
            beginObject(firstFont + index);
            pdf << "<< /Type /Font /Subtype /Type1 /BaseFont /" << scene.fonts[index]
                << " /Encoding /WinAnsiEncoding >>\nendobj\n";
        }
        for (size_t index = 0; index < scene.images.size(); ++index) {
            beginObject(firstImage + index);
            const PdfImage& image = scene.images[index];
            pdf << "<< /Type /XObject /Subtype /Image /Width " << image.width << " /Height "
                << image.height
                << " /ColorSpace /DeviceRGB /BitsPerComponent 8 /Filter /FlateDecode /Length "
                << image.zrgb.size() << " >>\nstream\n";
            pdf.write(reinterpret_cast<const char*>(image.zrgb.data()),
                static_cast<std::streamsize>(image.zrgb.size()));
            pdf << "\nendstream\nendobj\n";
        }
        const size_t xref = static_cast<size_t>(pdf.tellp());
        pdf << "xref\n0 " << offsets.size() + 1 << "\n0000000000 65535 f \n";
        for (size_t offset : offsets) {
            char line[32];
#ifdef _MSC_VER
            sprintf_s(line, sizeof(line), "%010zu 00000 n \n", offset);
#else
            snprintf(line, sizeof(line), "%010zu 00000 n \n", offset);
#endif
            pdf << line;
        }
        pdf << "trailer\n<< /Size " << offsets.size() + 1 << " /Root 1 0 R >>\nstartxref\n"
            << xref << "\n%%EOF\n";
        return pdf.str();
    }
    //=============================================================================
} // namespace
//=============================================================================
NLSRENDERWEB_IMPEXP bool
webPrimitivesToPdfFile(const std::vector<WebDrawPrimitive>& primitives, int width, int height,
    const std::wstring& filename)
{
    if (width <= 0 || height <= 0) {
        return false;
    }
    PdfScene scene = buildPdfScene(primitives, height);
    const std::string bytes = assemblePdf(scene, width, height);
    std::ofstream output(std::filesystem::path(filename), std::ios::binary | std::ios::trunc);
    if (!output) {
        return false;
    }
    output.write(bytes.data(), static_cast<std::streamsize>(bytes.size()));
    return output.good();
}
//=============================================================================
// Every control is recorded in logical pixels: pixel-unit ones from the figure
// Position, the others converted against the logical canvas. All of them are
// lifted by the same factor as the primitives around them.
NLSRENDERWEB_IMPEXP std::vector<WebUIControl>
scaleControlsToSurface(const std::vector<WebUIControl>& controls, double scale)
{
    std::vector<WebUIControl> out(controls);
    for (WebUIControl& control : out) {
        control.x *= scale;
        control.y *= scale;
        control.w *= scale;
        control.h *= scale;
        control.fontSize *= scale;
    }
    return out;
}
//=============================================================================
// The figure is recorded at its logical size, so layout, text measurement and
// geometry use the same units as the interactive render: the export DPI never
// moves ticks, labels or margins. The whole display list is then scaled by the
// surface ratio (export DPI or device pixel ratio): positions, font sizes, line
// widths, marker radii and dash lengths together, so every anchor stays centered.
// The scale stays out of RenderWeb so that renderer remains device-independent
// (always 1.0); the on-screen webview records at 1.0 and never calls this.
NLSRENDERWEB_IMPEXP std::vector<WebDrawPrimitive>
scalePrimitivesToSurface(const std::vector<WebDrawPrimitive>& primitives, double scale)
{
    std::vector<WebDrawPrimitive> out(primitives);
    for (WebDrawPrimitive& primitive : out) {
        primitive.x *= scale;
        primitive.y *= scale;
        primitive.w *= scale;
        primitive.h *= scale;
        primitive.x1 *= scale;
        primitive.y1 *= scale;
        primitive.x2 *= scale;
        primitive.y2 *= scale;
        for (std::pair<double, double>& point : primitive.points) {
            point.first *= scale;
            point.second *= scale;
        }
        for (double& dashValue : primitive.dash) {
            dashValue *= scale;
        }
        primitive.width *= scale;
        primitive.size *= scale;
        // "points" batch: centers and radii scale with the export DPI like the
        // other coordinates, or a scatter would stay at the logical size.
        for (float& center : primitive.pointCenters) {
            center *= static_cast<float>(scale);
        }
        for (float& radius : primitive.pointRadii) {
            radius *= static_cast<float>(scale);
        }
        primitive.pointEdgeWidth *= scale;
        for (float& value : primitive.rectXYWH) {
            value *= static_cast<float>(scale);
        }
    }
    return out;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
