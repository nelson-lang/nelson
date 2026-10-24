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
#if defined(__APPLE__) && !defined(GL_SILENCE_DEPRECATION)
#define GL_SILENCE_DEPRECATION
#endif
#if defined(_WIN32) && !defined(NOMINMAX)
#define NOMINMAX
#endif
#include <algorithm>
#include <cmath>
#include <vector>
#if defined(_WIN32)
#include <windows.h>
#endif
#if defined(__APPLE__)
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif
#include "RenderOpenGL.hpp"
//=============================================================================
namespace {
//=============================================================================
// Glyph proportions, multiples of the half-size. They must stay in step with
// DrawSymbol (graphics RenderHelpers.cpp), markerShapeGeometry (RenderWeb)
// and figureMarkers.ts (webview), which draw the same shapes.
constexpr double squareHalf = 0.7778;
constexpr double diamondHalfW = 1.1021;
constexpr double diamondHalfH = 1.4685;
constexpr double triApex = 4.0 / 3.0;
constexpr double triBase = 2.0 / 3.0;
constexpr double triHalf = 1.1547005;
constexpr double pentagramInner = 0.38;
constexpr double hexagramInner = 0.5773503;
//=============================================================================
// One marker's geometry: fill triangles (3 vertices each) and outline segments
// (2 vertices each), as x,y pairs in pixel space.
struct MarkerGeometry
{
    std::vector<double> fill;
    std::vector<double> stroke;
};
//=============================================================================
void
addVertex(std::vector<double>& out, double x, double y)
{
    out.push_back(x);
    out.push_back(y);
}
//=============================================================================
void
addTriangle(MarkerGeometry& g, double x1, double y1, double x2, double y2, double x3, double y3)
{
    addVertex(g.fill, x1, y1);
    addVertex(g.fill, x2, y2);
    addVertex(g.fill, x3, y3);
}
//=============================================================================
void
addSegment(MarkerGeometry& g, double x1, double y1, double x2, double y2)
{
    addVertex(g.stroke, x1, y1);
    addVertex(g.stroke, x2, y2);
}
//=============================================================================
// A convex quad as the two triangles GL_QUADS rasterizes, and its outline.
void
addQuad(MarkerGeometry& g, bool fill, bool stroke, double x1, double y1, double x2, double y2,
    double x3, double y3, double x4, double y4)
{
    if (fill) {
        addTriangle(g, x1, y1, x2, y2, x3, y3);
        addTriangle(g, x1, y1, x3, y3, x4, y4);
    }
    if (stroke) {
        addSegment(g, x1, y1, x2, y2);
        addSegment(g, x2, y2, x3, y3);
        addSegment(g, x3, y3, x4, y4);
        addSegment(g, x4, y4, x1, y1);
    }
}
//=============================================================================
// A filled triangle and its outline, drawn as three segments like DrawSymbol.
void
addTri(MarkerGeometry& g, bool fill, bool stroke, double x1, double y1, double x2, double y2,
    double x3, double y3)
{
    if (fill) {
        addTriangle(g, x1, y1, x2, y2, x3, y3);
    }
    if (stroke) {
        addSegment(g, x1, y1, x2, y2);
        addSegment(g, x2, y2, x3, y3);
        addSegment(g, x3, y3, x1, y1);
    }
}
//=============================================================================
// A star of `points` outer vertices alternating with inner ones: a triangle fan
// from the centre for the fill (a star is star-shaped about its centre) and
// the closed path for the outline.
void
addStar(MarkerGeometry& g, bool fill, bool stroke, double x, double y, double outer, double inner,
    int points)
{
    const int count = 2 * points;
    std::vector<double> px(count), py(count);
    for (int i = 0; i < count; i++) {
        double angle = M_PI / 2 + i * M_PI / points;
        double radius = (i % 2 == 0) ? outer : inner;
        px[i] = x + radius * std::cos(angle);
        py[i] = y + radius * std::sin(angle);
    }
    for (int i = 0; i < count; i++) {
        int next = (i + 1) % count;
        if (fill) {
            addTriangle(g, x, y, px[i], py[i], px[next], py[next]);
        }
        if (stroke) {
            addSegment(g, px[i], py[i], px[next], py[next]);
        }
    }
}
//=============================================================================
// The disc circleFill fans out and the loop circle draws, at the same angles.
void
addCircle(MarkerGeometry& g, bool fill, bool stroke, double x, double y, double radius)
{
    const int segments = RenderOpenGL::circleSegmentCount(radius);
    std::vector<double> px(static_cast<size_t>(segments) + 1),
        py(static_cast<size_t>(segments) + 1);
    for (int i = 0; i <= segments; ++i) {
        double theta = 2.0 * M_PI * static_cast<double>(i) / static_cast<double>(segments);
        px[i] = x + radius * std::cos(theta);
        py[i] = y + radius * std::sin(theta);
    }
    for (int i = 0; i < segments; ++i) {
        if (fill) {
            addTriangle(g, x, y, px[i], py[i], px[i + 1], py[i + 1]);
        }
        if (stroke) {
            addSegment(g, px[i], py[i], px[i + 1], py[i + 1]);
        }
    }
}
//=============================================================================
// The vertices DrawSymbol emits for one marker; `half` is its half-size in
// pixels, `width` the outline width DrawSymbol receives.
void
markerGeometry(MarkerGeometry& g, int shape, double x, double y, double half, double width,
    bool fill, bool stroke)
{
    g.fill.clear();
    g.stroke.clear();
    switch (shape) {
    case RenderInterface::Plus: {
        if (stroke) {
            // Centring adjustment for odd line widths, as DrawSymbol does.
            double offset = (static_cast<int>(std::round(width)) % 2) ? 0.5 : 0.0;
            double cx = x + offset;
            double cy = y + offset;
            addSegment(g, cx, cy, cx, cy - half);
            addSegment(g, cx, cy, cx + half, cy);
            addSegment(g, cx, cy, cx, cy + half);
            addSegment(g, cx, cy, cx - half, cy);
        }
    } break;
    case RenderInterface::Circle:
        addCircle(g, fill, stroke, x, y, half);
        break;
    case RenderInterface::Dot:
        // A smaller disc in the edge colour; the caller passes it as the fill. The
        // point marker '.' renders at a third of the nominal MarkerSize.
        addCircle(g, fill, false, x, y, half / 3.0);
        break;
    case RenderInterface::Star: {
        if (stroke) {
            double diag = half / std::sqrt(2.0);
            addSegment(g, x - half, y, x + half, y);
            addSegment(g, x, y - half, x, y + half);
            addSegment(g, x - diag, y - diag, x + diag, y + diag);
            addSegment(g, x - diag, y + diag, x + diag, y - diag);
        }
    } break;
    case RenderInterface::Times:
        if (stroke) {
            addSegment(g, x - half, y - half, x + half, y + half);
            addSegment(g, x + half, y - half, x - half, y + half);
        }
        break;
    case RenderInterface::Square: {
        double sq = squareHalf * half;
        addQuad(g, fill, stroke, x - sq, y - sq, x + sq, y - sq, x + sq, y + sq, x - sq, y + sq);
    } break;
    case RenderInterface::Diamond: {
        double dw = diamondHalfW * half;
        double dh = diamondHalfH * half;
        addQuad(g, fill, stroke, x, y - dh, x + dw, y, x, y + dh, x - dw, y);
    } break;
    case RenderInterface::Up: {
        double ax = triHalf * half, ap = triApex * half, bs = triBase * half;
        addTri(g, fill, stroke, x, y + ap, x + ax, y - bs, x - ax, y - bs);
    } break;
    case RenderInterface::Down: {
        double ax = triHalf * half, ap = triApex * half, bs = triBase * half;
        addTri(g, fill, stroke, x, y - ap, x + ax, y + bs, x - ax, y + bs);
    } break;
    case RenderInterface::Left: {
        double ay = triHalf * half, ap = triApex * half, bs = triBase * half;
        addTri(g, fill, stroke, x - ap, y, x + bs, y + ay, x + bs, y - ay);
    } break;
    case RenderInterface::Right: {
        double ay = triHalf * half, ap = triApex * half, bs = triBase * half;
        addTri(g, fill, stroke, x + ap, y, x - bs, y + ay, x - bs, y - ay);
    } break;
    case RenderInterface::Pentagram:
        addStar(g, fill, stroke, x, y, half, half * pentagramInner, 5);
        break;
    case RenderInterface::Hexagram:
        addStar(g, fill, stroke, x, y, half, half * hexagramInner, 6);
        break;
    default:
        break;
    }
}
//=============================================================================
// Where one marker's vertices sit in the shared arrays: its fill triangles
// then its outline segments.
struct MarkerSpan
{
    GLint first = 0;
    GLsizei fillCount = 0;
    GLsizei strokeCount = 0;
};
//=============================================================================
// Append a marker's x,y vertex pairs to the shared array, as plain 2D pairs or,
// when depth-aware, as x,y,z triples that carry the marker's clip-space depth.
void
appendMarkerVertices(
    std::vector<GLdouble>& out, const std::vector<double>& xy, bool depthAware, double z)
{
    if (!depthAware) {
        out.insert(out.end(), xy.begin(), xy.end());
        return;
    }
    for (size_t k = 0; k + 1 < xy.size(); k += 2) {
        out.push_back(xy[k]);
        out.push_back(xy[k + 1]);
        out.push_back(z);
    }
}
//=============================================================================
void
pushColor(std::vector<GLubyte>& colors, const std::vector<double>& rgba, size_t marker,
    size_t vertexCount)
{
    GLubyte packed[4];
    for (size_t k = 0; k < 4; k++) {
        double value = std::clamp(rgba[marker * 4 + k], 0.0, 1.0);
        packed[k] = static_cast<GLubyte>(std::lround(value * 255.0));
    }
    for (size_t v = 0; v < vertexCount; v++) {
        colors.insert(colors.end(), packed, packed + 4);
    }
}
//=============================================================================
} // namespace
//=============================================================================
bool
RenderOpenGL::markerBatch(int shape, const std::vector<double>& xs, const std::vector<double>& ys,
    const std::vector<double>& markerSizes, const std::vector<double>& faceRgba,
    const std::vector<double>& edgeRgba, double edgeWidth, const std::vector<double>& depths)
{
    // Every marker of a series is drawn from one vertex array: per-marker primitives
    // and state toggles dominate the frame time on a dense scatter. The vertices are
    // the ones DrawSymbol emits, painted in the same order (fill then outline, marker
    // after marker).
    const size_t n = xs.size();
    if (n == 0 || ys.size() != n || markerSizes.size() != n) {
        return false;
    }
    const bool hasFace = faceRgba.size() == n * 4;
    const bool hasEdge = edgeRgba.size() == n * 4;
    if (!hasFace && !hasEdge) {
        return false;
    }
    // Direct draw runs with the depth test off so screen-space glyphs paint on top.
    // When the caller supplies a clip-space z per point, it is carried as a third
    // vertex component and the batch is depth-tested, so a marker behind an opaque
    // surface is occluded. The direct-draw projection is the orthographic
    // project(..., -1, 1): its clip z equals -z_eye, so a vertex at z = -depth
    // reproduces the window depth the surface wrote and GL_LEQUAL resolves occlusion.
    const bool depthAware = depths.size() == n;
    const int componentCount = depthAware ? 3 : 2;
    // Scratch arrays reused from one batch to the next. They live here rather
    // than in the class so its layout, which the figure widgets of other modules
    // construct, stays unchanged; drawing happens on the one GUI thread.
    static std::vector<GLdouble> vertices;
    static std::vector<GLubyte> colors;
    static std::vector<MarkerSpan> spans;
    vertices.clear();
    colors.clear();
    spans.clear();
    MarkerGeometry geometry;
    for (size_t i = 0; i < n; i++) {
        const double half = pointsToPixels(markerSizes[i]) / 2.0;
        if (half <= 0) {
            continue;
        }
        markerGeometry(geometry, shape, xs[i], ys[i], half, edgeWidth, hasFace, hasEdge);
        MarkerSpan span;
        span.first = static_cast<GLint>(vertices.size() / componentCount);
        span.fillCount = static_cast<GLsizei>(geometry.fill.size() / 2);
        span.strokeCount = static_cast<GLsizei>(geometry.stroke.size() / 2);
        appendMarkerVertices(vertices, geometry.fill, depthAware, depthAware ? -depths[i] : 0.0);
        appendMarkerVertices(vertices, geometry.stroke, depthAware, depthAware ? -depths[i] : 0.0);
        if (span.fillCount > 0) {
            pushColor(colors, faceRgba, i, static_cast<size_t>(span.fillCount));
        }
        if (span.strokeCount > 0) {
            pushColor(colors, edgeRgba, i, static_cast<size_t>(span.strokeCount));
        }
        spans.push_back(span);
    }
    if (vertices.empty()) {
        return true;
    }

    // The state DrawSymbol, color() and circleFill set before drawing a marker;
    // only the disc is drawn with polygon smoothing.
    lineWidth(edgeWidth);
    setLineStyle(L"-");
#if defined(GL_POLYGON_STIPPLE)
    glDisable(GL_POLYGON_STIPPLE);
#endif
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    const bool isDisc = (shape == RenderInterface::Circle) || (shape == RenderInterface::Dot);
    const GLboolean polygonSmoothEnabled = glIsEnabled(GL_POLYGON_SMOOTH);
    if (isDisc && graphicsSmoothing) {
        glEnable(GL_POLYGON_SMOOTH);
        glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
    }
    // Read the depth buffer (surfaces drawn before this batch) without writing it,
    // so markers occlude against the scene yet never z-fight each other or block
    // the text drawn afterwards in the same direct-draw pass.
    if (depthAware) {
        glEnable(GL_DEPTH_TEST);
        glDepthFunc(GL_LEQUAL);
        glDepthMask(GL_FALSE);
    }
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
    glVertexPointer(componentCount, GL_DOUBLE, 0, vertices.data());
    glColorPointer(4, GL_UNSIGNED_BYTE, 0, colors.data());
    const bool interleaved = hasFace && hasEdge && !spans.empty() && spans.front().fillCount > 0
        && spans.front().strokeCount > 0;
    if (interleaved) {
        // Fill then outline of each marker before the next one, as DrawSymbol
        // paints them, so overlapping markers keep their stacking.
        for (const MarkerSpan& span : spans) {
            glDrawArrays(GL_TRIANGLES, span.first, span.fillCount);
            glDrawArrays(GL_LINES, span.first + span.fillCount, span.strokeCount);
        }
    } else if (spans.front().fillCount > 0) {
        glDrawArrays(GL_TRIANGLES, 0, static_cast<GLsizei>(vertices.size() / componentCount));
    } else {
        glDrawArrays(GL_LINES, 0, static_cast<GLsizei>(vertices.size() / componentCount));
    }
    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);
    if (depthAware) {
        glDisable(GL_DEPTH_TEST);
    }
    if (!polygonSmoothEnabled) {
        glDisable(GL_POLYGON_SMOOTH);
    }
    // A colour array leaves the current colour at its last element.
    glColor4dv(currentColor);
    return true;
}
//=============================================================================
