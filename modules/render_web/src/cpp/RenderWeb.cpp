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
#include <cmath> // M_PI
#include "RenderWeb.hpp"
#include "RenderLighting.hpp"
#include "GOPropertyValues.hpp"
#include "FontMetrics.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::vector<std::wstring>
splitEoLText(const std::wstring& text);
//=============================================================================
static void
translatePrimitive(WebDrawPrimitive& primitive, double dx, double dy);
//=============================================================================
// Text metrics and glyph rasterization come from the shared Qt-free font
// engine (module font_metrics: stb_truetype over platform TTFs, AFM tables
// fallback), the same engine the graphics drawing model uses for Extent.
static double
webFontPixelSize(const FontSpec& fnt)
{
    if (fnt.pixelSize > 0) {
        return fnt.pixelSize;
    }
    if (fnt.pointSizeF > 0) {
        return fnt.pointSizeF * (96.0 / 72.0);
    }
    return 12.0;
}
//=============================================================================
void
RenderWeb::clear(std::vector<double> col)
{
    // Inside a region the surface belongs to the enclosing frame: only fill
    // the region, never drop what the frame has already emitted.
    if (m_regions.empty()) {
        m_prims.clear();
        m_pending3d.clear();
        // A new frame: forget the opaque depths the previous one recorded so stale
        // occlusion never hides this frame's markers.
        m_sceneDepth.clear();
        m_depthW = 0;
        m_depthH = 0;
        m_deferredTranslucentPolygons.clear();
        m_deferredTranslucentEdges.clear();
        // A begin/endSurfaceGroup pair always balances within a frame; reset the
        // nesting depth anyway so an interrupted frame cannot leak into the next.
        m_surfaceGroupDepth = 0;
    }
    if (col.size() >= 3) {
        WebDrawPrimitive p;
        p.kind = "rect";
        p.x = 0;
        p.y = 0;
        p.w = m_width;
        p.h = m_height;
        p.color = colorString(col);
        m_prims.push_back(p);
    }
}
//=============================================================================
bool
RenderWeb::pushRegion(double x, double y, double width, double height)
{
    if (width <= 0 || height <= 0) {
        return false;
    }
    // Depth-buffered primitives are ordered against the frame they were drawn
    // in, so close that batch before the frame changes.
    flush3d();
    SavedRegion region;
    region.primitiveMark = m_prims.size();
    region.meshMark = m_webglMeshes.size();
    region.x1 = m_x1;
    region.y1 = m_y1;
    region.width = m_width;
    region.height = m_height;
    region.offsetX = x;
    region.offsetTop = m_height - (y + height);
    region.offsetBottom = y;
    m_regions.push_back(region);
    m_x1 = 0;
    m_y1 = 0;
    m_width = width;
    m_height = height;
    WebDrawPrimitive save;
    save.kind = "clipSave";
    m_prims.push_back(save);
    WebDrawPrimitive clip;
    clip.kind = "clip";
    clip.x = 0;
    clip.y = 0;
    clip.w = width;
    clip.h = height;
    m_prims.push_back(clip);
    return true;
}
//=============================================================================
void
RenderWeb::popRegion()
{
    if (m_regions.empty()) {
        return;
    }
    flush3d();
    const SavedRegion region = m_regions.back();
    m_regions.pop_back();
    for (size_t index = region.primitiveMark; index < m_prims.size(); ++index) {
        translatePrimitive(m_prims[index], region.offsetX, region.offsetTop);
    }
    for (size_t index = region.meshMark; index < m_webglMeshes.size(); ++index) {
        m_webglMeshes[index].viewport[0] += (int)region.offsetX;
        m_webglMeshes[index].viewport[1] += (int)region.offsetBottom;
    }
    m_x1 = region.x1;
    m_y1 = region.y1;
    m_width = region.width;
    m_height = region.height;
    WebDrawPrimitive restore;
    restore.kind = "clipRestore";
    m_prims.push_back(restore);
}
//=============================================================================
// A screen-space clip rectangle for direct 2D drawing (e.g. bubblechart's
// axes-limit clip). Unlike pushRegion it does not move the coordinate origin:
// the clip is emitted in the current frame, with the bottom-left y0 flipped to
// the top-left origin of the display list. Later primitives keep their pixel
// positions and are only clipped.
void
RenderWeb::setClipBox(double x0, double y0, double width, double height)
{
    if (width <= 0 || height <= 0) {
        return;
    }
    flush3d();
    WebDrawPrimitive save;
    save.kind = "clipSave";
    m_prims.push_back(save);
    WebDrawPrimitive clip;
    clip.kind = "clip";
    clip.x = x0;
    clip.y = m_height - (y0 + height);
    clip.w = width;
    clip.h = height;
    m_prims.push_back(clip);
    ++m_clipBoxDepth;
}
//=============================================================================
void
RenderWeb::clearClipBox()
{
    if (m_clipBoxDepth <= 0) {
        return;
    }
    --m_clipBoxDepth;
    flush3d();
    WebDrawPrimitive restore;
    restore.kind = "clipRestore";
    m_prims.push_back(restore);
}
//=============================================================================
void
RenderWeb::mapPoint(double x, double y, double z, double& a, double& b, double& c)
{
    a = model[0] * x + model[4] * y + model[8] * z + model[12];
    b = model[1] * x + model[5] * y + model[9] * z + model[13];
    c = model[2] * x + model[6] * y + model[10] * z + model[14];
}
//=============================================================================
void
RenderWeb::toPixels(double x, double y, double z, double& a, double& b, bool& clipped)
{
    double c;
    toPixels(x, y, z, a, b, c, clipped);
}
//=============================================================================
void
RenderWeb::toPixels(double x, double y, double z, double& a, double& b, double& c, bool& clipped)
{
    double xprime = 0, yprime = 0, zprime = 0;
    mapPoint(x, y, z, xprime, yprime, zprime);
    double wclip = proj[3] * xprime + proj[7] * yprime + proj[11] * zprime + proj[15];
    if (wclip <= 0.0) {
        // At or behind the perspective near plane (orthographic keeps wclip == 1).
        clipped = true;
        a = viewp[0] - viewp[2];
        b = viewp[1] - viewp[3];
        c = 2.0;
        return;
    }
    double xclip = (proj[0] * xprime + proj[4] * yprime + proj[8] * zprime + proj[12]) / wclip;
    double yclip = (proj[1] * xprime + proj[5] * yprime + proj[9] * zprime + proj[13]) / wclip;
    double zclip = (proj[2] * xprime + proj[6] * yprime + proj[10] * zprime + proj[14]) / wclip;
    constexpr double eps = 1e-8;
    clipped = (xclip < -1. - eps) || (xclip > 1. + eps) || (yclip < -1. - eps) || (yclip > 1. + eps)
        || (zclip < -1. - eps) || (zclip > 1. + eps);
    a = viewp[0] + (1 + xclip) / 2.0 * viewp[2];
    b = viewp[1] + (1 + yclip) / 2.0 * viewp[3];
    c = zclip;
}
//=============================================================================
void
RenderWeb::toPixels(double x, double y, double z, double& a, double& b)
{
    bool clipped;
    toPixels(x, y, z, a, b, clipped);
}
//=============================================================================
void
RenderWeb::toPixels(double x, double y, double z, int& a, int& b)
{
    double av, bv;
    toPixels(x, y, z, av, bv);
    a = (int)av;
    b = (int)bv;
}
//=============================================================================
void
RenderWeb::mapXY(double x, double y, double z, double& px, double& py)
{
    double a, b;
    toPixels(x, y, z, a, b);
    // Screen space is y-down; the projection is y-up (as in RenderQt::map).
    px = a;
    py = m_height - 1 - b;
}
//=============================================================================
double
RenderWeb::mapXYZ(double x, double y, double z, double& px, double& py)
{
    double xprime = 0, yprime = 0, zprime = 0;
    mapPoint(x, y, z, xprime, yprime, zprime);
    double wclip = proj[3] * xprime + proj[7] * yprime + proj[11] * zprime + proj[15];
    if (wclip <= 0.0) {
        // At or behind the perspective near plane (orthographic keeps wclip == 1).
        px = viewp[0] - viewp[2];
        py = m_height - 1 - (viewp[1] - viewp[3]);
        return 2.0;
    }
    double xclip = (proj[0] * xprime + proj[4] * yprime + proj[8] * zprime + proj[12]) / wclip;
    double yclip = (proj[1] * xprime + proj[5] * yprime + proj[9] * zprime + proj[13]) / wclip;
    double zclip = (proj[2] * xprime + proj[6] * yprime + proj[10] * zprime + proj[14]) / wclip;
    px = viewp[0] + (1 + xclip) / 2.0 * viewp[2];
    px = px; // screen x
    double b = viewp[1] + (1 + yclip) / 2.0 * viewp[3];
    py = m_height - 1 - b;
    return zclip;
}
//=============================================================================
void
RenderWeb::scale(double sx, double sy, double sz)
{
    model[0] *= sx;
    model[4] *= sy;
    model[8] *= sz;
    model[1] *= sx;
    model[5] *= sy;
    model[9] *= sz;
    model[2] *= sx;
    model[6] *= sy;
    model[10] *= sz;
    model[3] *= sx;
    model[7] *= sy;
    model[11] *= sz;
}
//=============================================================================
void
RenderWeb::pushModelMatrix(const double* matrix16ColumnMajor)
{
    std::array<double, 16> saved;
    for (int i = 0; i < 16; ++i) {
        saved[i] = model[i];
    }
    m_modelStack.push_back(saved);
    m_dataClipEnabledStack.push_back(m_dataClip.enabled ? 1 : 0);
    m_dataClip.enabled = false;
    if (!matrix16ColumnMajor) {
        return;
    }
    // result = model * M, both column-major (index = col * 4 + row).
    double result[16];
    for (int col = 0; col < 4; ++col) {
        for (int row = 0; row < 4; ++row) {
            double sum = 0.0;
            for (int k = 0; k < 4; ++k) {
                sum += saved[k * 4 + row] * matrix16ColumnMajor[col * 4 + k];
            }
            result[col * 4 + row] = sum;
        }
    }
    for (int i = 0; i < 16; ++i) {
        model[i] = result[i];
    }
}
//=============================================================================
void
RenderWeb::popModelMatrix()
{
    if (m_modelStack.empty()) {
        return;
    }
    const std::array<double, 16>& saved = m_modelStack.back();
    for (int i = 0; i < 16; ++i) {
        model[i] = saved[i];
    }
    m_modelStack.pop_back();
    if (!m_dataClipEnabledStack.empty()) {
        m_dataClip.enabled = m_dataClipEnabledStack.back() != 0;
        m_dataClipEnabledStack.pop_back();
    }
}
//=============================================================================
static void
crossp(double ux, double uy, double uz, double vx, double vy, double vz, double& sx, double& sy,
    double& sz)
{
    sx = uy * vz - uz * vy;
    sy = uz * vx - ux * vz;
    sz = ux * vy - uy * vx;
}
//=============================================================================
void
RenderWeb::lookAt(double eyex, double eyey, double eyez, double centerx, double centery,
    double centerz, double upx, double upy, double upz)
{
    double fx = centerx - eyex, fy = centery - eyey, fz = centerz - eyez;
    double fnorm = std::sqrt(fx * fx + fy * fy + fz * fz);
    fx /= fnorm;
    fy /= fnorm;
    fz /= fnorm;
    double upnorm = std::sqrt(upx * upx + upy * upy + upz * upz);
    upx /= upnorm;
    upy /= upnorm;
    upz /= upnorm;
    double sx, sy, sz;
    crossp(fx, fy, fz, upx, upy, upz, sx, sy, sz);
    if (std::sqrt(sx * sx + sy * sy + sz * sz) < 1e-9) {
        // Up vector (nearly) parallel to the viewing direction, e.g. a camera looking
        // straight down an axis. Use the world axis least aligned with the forward
        // direction, otherwise the basis collapses to a null matrix.
        double auxx = 0.0, auxy = 0.0, auxz = 0.0;
        if (std::fabs(fx) <= std::fabs(fy) && std::fabs(fx) <= std::fabs(fz)) {
            auxx = 1.0;
        } else if (std::fabs(fy) <= std::fabs(fz)) {
            auxy = 1.0;
        } else {
            auxz = 1.0;
        }
        crossp(fx, fy, fz, auxx, auxy, auxz, sx, sy, sz);
    }
    double ux, uy, uz;
    crossp(sx, sy, sz, fx, fy, fz, ux, uy, uz);
    model[0] = sx;
    model[4] = sy;
    model[8] = sz;
    model[12] = -(sx * eyex + sy * eyey + sz * eyez);
    model[1] = ux;
    model[5] = uy;
    model[9] = uz;
    model[13] = -(ux * eyex + uy * eyey + uz * eyez);
    model[2] = -fx;
    model[6] = -fy;
    model[10] = -fz;
    model[14] = fx * eyex + fy * eyey + fz * eyez;
    model[3] = 0;
    model[7] = 0;
    model[11] = 0;
    model[15] = 1;
}
//=============================================================================
void
RenderWeb::project(double left, double right, double bottom, double top, double near, double far)
{
    double tx = -(right + left) / (right - left);
    double ty = -(top + bottom) / (top - bottom);
    double tz = -(far + near) / (far - near);
    for (double& i : proj) {
        i = 0;
    }
    proj[0] = 2 / (right - left);
    proj[5] = 2 / (top - bottom);
    proj[10] = -2 / (far - near);
    proj[12] = tx;
    proj[13] = ty;
    proj[14] = tz;
    proj[15] = 1;
    m_bounds[0] = left;
    m_bounds[1] = right;
    m_bounds[2] = bottom;
    m_bounds[3] = top;
    m_bounds[4] = near;
    m_bounds[5] = far;
}
//=============================================================================
void
RenderWeb::projectPerspective(
    double left, double right, double bottom, double top, double near, double far)
{
    buildFrustumProjectionMatrix(proj, left, right, bottom, top, near, far);
    m_bounds[0] = left;
    m_bounds[1] = right;
    m_bounds[2] = bottom;
    m_bounds[3] = top;
    m_bounds[4] = near;
    m_bounds[5] = far;
}
//=============================================================================
void
RenderWeb::viewport(double x0, double y0, double width, double height)
{
    viewp[0] = (int)x0;
    viewp[1] = (int)y0;
    viewp[2] = (int)width;
    viewp[3] = (int)height;
    // Axes children (the plotted data) are clipped to the axes box: emit a
    // replace-clip primitive in top-origin pixels. setupDirectDraw widens it back
    // to the whole figure before ticks and labels, so those are never clipped.
    WebDrawPrimitive p;
    p.kind = "clip";
    p.x = x0;
    p.y = m_height - (y0 + height);
    p.w = width;
    p.h = height;
    m_prims.push_back(p);
}
//=============================================================================
void
RenderWeb::clipToEntireFrame()
{
    // Axes Clipping == 'off': widen the clip from the axes rect (emitted by viewport)
    // to the whole frame so overflowing geometry spills into the figure. The viewport
    // mapping is untouched, so content still lands in the axes sub-rect. This
    // replace-clip primitive supersedes the axes-rect clip for the children drawn next.
    WebDrawPrimitive p;
    p.kind = "clip";
    p.x = m_x1;
    p.y = m_height - (m_y1 + m_height);
    p.w = m_width;
    p.h = m_height;
    m_prims.push_back(p);
}
//=============================================================================
void
RenderWeb::getModelviewMatrix(std::vector<double>& amodel)
{
    amodel.assign(std::begin(model), std::end(model));
}
//=============================================================================
void
RenderWeb::setLightingScene(const RenderLightingScene& scene)
{
    m_lightingScene = scene;
}
//=============================================================================
void
RenderWeb::setLightingMaterial(const RenderLightingMaterial& material)
{
    m_lightingMaterial = material;
}
//=============================================================================
std::string
RenderWeb::colorString(const std::vector<double>& col) const
{
    if (col.size() < 3) {
        return "rgba(0,0,0,1)";
    }
    int r = (int)(col[0] * 255), g = (int)(col[1] * 255), b = (int)(col[2] * 255);
    double a = col.size() >= 4 ? col[3] : 1.0;
    return "rgba(" + std::to_string(r) + "," + std::to_string(g) + "," + std::to_string(b) + ","
        + std::to_string(a) + ")";
}
//=============================================================================
void
RenderWeb::color(std::vector<double> col)
{
    m_color = colorString(col);
}
//=============================================================================
void
RenderWeb::setLineStyle(const std::wstring& style)
{
    m_lineStyle = style;
}
//=============================================================================
std::vector<double>
RenderWeb::dashArray() const
{
    // Same patterns as RenderQt::setLineStyle, scaled by the line width like Qt does.
    // Dashed and dash-dot both have a period of about 8 (dashed 4 + 4, dash-dot
    // 4 + 1 + 2 + 1). The dotted style keeps the short dot.
    const double w = m_lineWidth > 0 ? m_lineWidth : 1.0;
    if (m_lineStyle == L"--") {
        return { 4 * w, 4 * w };
    }
    if (m_lineStyle == L":") {
        return { 1 * w, 2 * w };
    }
    if (m_lineStyle == L"-.") {
        return { 4 * w, 1 * w, 2 * w, 1 * w };
    }
    return { };
}
//=============================================================================
void
RenderWeb::lineWidth(double n)
{
    // Keep the true converted width, in logical (device-independent) units. Each
    // consumer applies the 1px floor in FINAL device space (raster: max(1.0, width) *
    // SS; on-screen JSON and vector export: max(1.0, width)). Flooring here would
    // clamp before the export scale is applied and inflate thin strokes: a 0.5pt
    // line would become 1.0 logical px, then 1.5625 device px at 150 dpi instead of
    // 1.04.
    m_lineWidth = pointsToPixels(n);
}
//=============================================================================
double
RenderWeb::pointsToPixels(double points)
{
    return points * (96.0 / 72.0);
}
//=============================================================================
void
RenderWeb::setDataClipBox(const RenderClipBox& box)
{
    m_dataClip = box;
}
//=============================================================================
void
RenderWeb::clearDataClipBox()
{
    m_dataClip.enabled = false;
}
//=============================================================================
void
RenderWeb::line(double x1, double y1, double z1, double x2, double y2, double z2)
{
    if (lineHidden()) {
        return;
    }
    // See RenderQt::line: while direct drawing, the coordinates are pixels and the
    // clip cuboid is in axes data coordinates, so the test would discard every
    // line-drawn marker glyph instead of clipping data geometry.
    if (!m_inDirect && m_dataClip.enabled
        && !renderClipSegment(m_dataClip, x1, y1, z1, x2, y2, z2, x1, y1, z1, x2, y2, z2)) {
        return;
    }
    WebDrawPrimitive p;
    p.kind = "line";
    const double d1 = mapXYZ(x1, y1, z1, p.x1, p.y1);
    const double d2 = mapXYZ(x2, y2, z2, p.x2, p.y2);
    p.color = m_color;
    p.width = m_lineWidth;
    p.dash = dashArray();
    p.depth = (d1 + d2) / 2.0;
    p.solidEdge = m_surfaceGroupDepth > 0;
    (m_inDirect ? m_prims : m_pending3d).push_back(p);
}
//=============================================================================
void
RenderWeb::line(double x1, double y1, double x2, double y2)
{
    if (lineHidden()) {
        return;
    }
    WebDrawPrimitive p;
    p.kind = "line";
    // Must map like RenderQt::line, which draws map(x,y,0). Raw coordinates skip
    // the y-flip and mirror every direct-drawn segment (axis tick marks).
    mapXY(x1, y1, 0, p.x1, p.y1);
    mapXY(x2, y2, 0, p.x2, p.y2);
    p.color = m_color;
    p.width = m_lineWidth;
    p.dash = dashArray();
    m_prims.push_back(p);
}
//=============================================================================
void
RenderWeb::lineSegments3D(const std::vector<double>& xyz)
{
    if (lineHidden()) {
        return;
    }
    if (!m_inDirect) {
        // The only callers are GOAxis (grid, minor grid, box and ruler lines), which
        // lie on the data-volume boundary. Emit the batch as one depth-sorted
        // "segments" primitive placed behind the filled data using its farthest
        // sample depth, so a bar in front occludes the floor grid lines. A
        // per-segment AVERAGE depth would sort a long grid line ahead of a short
        // near bar and paint a streak across its top face.
        WebDrawPrimitive p;
        p.kind = "segments";
        p.color = m_color;
        p.width = m_lineWidth;
        p.dash = dashArray();
        p.points.reserve((xyz.size() / 6) * 2);
        double farthest = 0.0;
        bool haveDepth = false;
        for (size_t k = 0; k + 5 < xyz.size(); k += 6) {
            double px, py;
            const double d1 = mapXYZ(xyz[k], xyz[k + 1], xyz[k + 2], px, py);
            p.points.emplace_back(px, py);
            const double d2 = mapXYZ(xyz[k + 3], xyz[k + 4], xyz[k + 5], px, py);
            p.points.emplace_back(px, py);
            const double segmentDepth = std::max(d1, d2);
            farthest = haveDepth ? std::max(farthest, segmentDepth) : segmentDepth;
            haveDepth = true;
        }
        if (p.points.size() >= 2) {
            p.depth = farthest;
            m_pending3d.push_back(std::move(p));
        }
        return;
    }
    WebDrawPrimitive p;
    p.kind = "segments";
    p.color = m_color;
    p.width = m_lineWidth;
    p.dash = dashArray();
    p.points.reserve((xyz.size() / 6) * 2);
    for (size_t k = 0; k + 5 < xyz.size(); k += 6) {
        double px, py;
        mapXY(xyz[k], xyz[k + 1], xyz[k + 2], px, py);
        p.points.emplace_back(px, py);
        mapXY(xyz[k + 3], xyz[k + 4], xyz[k + 5], px, py);
        p.points.emplace_back(px, py);
    }
    if (p.points.size() >= 2) {
        m_prims.push_back(p);
    }
}
//=============================================================================
void
RenderWeb::lineSegments2D(const std::vector<double>& xy)
{
    if (lineHidden()) {
        return;
    }
    WebDrawPrimitive p;
    p.kind = "segments";
    p.color = m_color;
    p.width = m_lineWidth;
    p.dash = dashArray();
    p.points.reserve((xy.size() / 4) * 2);
    for (size_t k = 0; k + 3 < xy.size(); k += 4) {
        double px, py;
        mapXY(xy[k], xy[k + 1], 0, px, py);
        p.points.emplace_back(px, py);
        mapXY(xy[k + 2], xy[k + 3], 0, px, py);
        p.points.emplace_back(px, py);
    }
    if (p.points.size() >= 2) {
        m_prims.push_back(p);
    }
}
//=============================================================================
void
RenderWeb::lineSeries(std::vector<double> xs, std::vector<double> ys, std::vector<double> zs)
{
    if (lineHidden()) {
        return;
    }
    const size_t n = std::min(xs.size(), ys.size());
    WebDrawPrimitive p;
    p.kind = "polyline";
    p.color = m_color;
    p.width = m_lineWidth;
    p.dash = dashArray();
    // Depth-buffer polylines go through the same deferred queue as the filled
    // geometry emitters (quad/tri/drawPatch). Lines emitted straight to m_prims
    // while fills defer to m_pending3d would be covered by every filled patch,
    // hiding strokes such as the level lines of contourf. Sharing the queue lets
    // stable_sort keep the call order for coplanar primitives (a flat 2D plot is
    // all at one depth): fill first, lines on top.
    double depthSum = 0.0;
    size_t depthCount = 0;
    auto flushPolyline = [&]() {
        if (p.points.size() >= 2) {
            p.depth = depthCount ? depthSum / static_cast<double>(depthCount) : 0.0;
            (m_inDirect ? m_prims : m_pending3d).push_back(p);
        }
        p.points.clear();
        depthSum = 0.0;
        depthCount = 0;
    };
    if (m_dataClip.enabled && renderClipNeeded(m_dataClip, xs, ys, zs)) {
        // Clip every segment to the axes limits. A polyline that leaves and
        // re-enters the box is emitted as several sub-polylines, so nothing is
        // drawn outside the data box. Skipped entirely (fast path below) when
        // every point is already inside the box.
        auto emitClippedPoint = [&](double x, double y, double z) {
            double px, py;
            depthSum += mapXYZ(x, y, z, px, py);
            depthCount++;
            p.points.emplace_back(px, py);
        };
        bool prevFinite = false;
        double prevX = 0, prevY = 0, prevZ = 0;
        for (size_t i = 0; i < n; i++) {
            double z = i < zs.size() ? zs[i] : 0.0;
            bool finite = std::isfinite(xs[i]) && std::isfinite(ys[i]) && std::isfinite(z);
            if (!finite) {
                flushPolyline();
                prevFinite = false;
                continue;
            }
            if (prevFinite) {
                double cx0, cy0, cz0, cx1, cy1, cz1;
                if (renderClipSegment(m_dataClip, prevX, prevY, prevZ, xs[i], ys[i], z, cx0, cy0,
                        cz0, cx1, cy1, cz1)) {
                    if (p.points.empty()) {
                        emitClippedPoint(cx0, cy0, cz0);
                    }
                    emitClippedPoint(cx1, cy1, cz1);
                    // If the far endpoint lies outside the box the segment was
                    // cut there, so the current sub-polyline ends.
                    if (!renderClipPointInside(
                            m_dataClip, xs[i], ys[i], z, renderClipEpsilon(m_dataClip))) {
                        flushPolyline();
                    }
                } else {
                    flushPolyline();
                }
            }
            prevFinite = true;
            prevX = xs[i];
            prevY = ys[i];
            prevZ = z;
        }
        flushPolyline();
        return;
    }
    for (size_t i = 0; i < n; i++) {
        double z = i < zs.size() ? zs[i] : 0.0;
        if (!std::isfinite(xs[i]) || !std::isfinite(ys[i]) || !std::isfinite(z)) {
            // break the polyline on non-finite points
            flushPolyline();
            continue;
        }
        double px, py;
        depthSum += mapXYZ(xs[i], ys[i], z, px, py);
        depthCount++;
        p.points.emplace_back(px, py);
    }
    flushPolyline();
}
//=============================================================================
void
RenderWeb::quadline(double x1, double y1, double z1, double x2, double y2, double z2, double x3,
    double y3, double z3, double x4, double y4, double z4)
{
    // The closed outline goes through the same deferred painter's queue as its fill
    // (quad), not straight to m_prims. flush3d biases strokes a hair toward the
    // camera, so the edge lands on top of its coplanar fill. Emitted ahead of the
    // queue, the later-flushed fill overpaints the inner half of the edge. Depth is
    // the mean corner depth, as in quad, so a 3D outline still sorts by distance.
    WebDrawPrimitive p;
    p.kind = "polyline";
    p.color = m_color;
    p.width = m_lineWidth;
    double px, py, d = 0;
    d += mapXYZ(x1, y1, z1, px, py);
    p.points.emplace_back(px, py);
    d += mapXYZ(x2, y2, z2, px, py);
    p.points.emplace_back(px, py);
    d += mapXYZ(x3, y3, z3, px, py);
    p.points.emplace_back(px, py);
    d += mapXYZ(x4, y4, z4, px, py);
    p.points.emplace_back(px, py);
    mapXY(x1, y1, z1, px, py);
    p.points.emplace_back(px, py);
    p.depth = d / 4.0;
    p.solidEdge = m_surfaceGroupDepth > 0;
    (m_inDirect ? m_prims : m_pending3d).push_back(p);
}
//=============================================================================
void
RenderWeb::quad(double x1, double y1, double z1, double x2, double y2, double z2, double x3,
    double y3, double z3, double x4, double y4, double z4)
{
    // Filled polygon of the 4 projected corners (exact, unlike a bounding box).
    WebDrawPrimitive p;
    p.kind = "polygon";
    p.color = m_color;
    double px, py, d = 0;
    d += mapXYZ(x1, y1, z1, px, py);
    p.points.emplace_back(px, py);
    d += mapXYZ(x2, y2, z2, px, py);
    p.points.emplace_back(px, py);
    d += mapXYZ(x3, y3, z3, px, py);
    p.points.emplace_back(px, py);
    d += mapXYZ(x4, y4, z4, px, py);
    p.points.emplace_back(px, py);
    p.depth = d / 4.0;
    (m_inDirect ? m_prims : m_pending3d).push_back(p);
}
//=============================================================================
void
RenderWeb::tri(double x1, double y1, double z1, double x2, double y2, double z2, double x3,
    double y3, double z3)
{
    // RenderQt::tri FILLS the triangle. An outline would make the internal edges of
    // triangle-fanned fills visible (white rays across the 3D axes background).
    WebDrawPrimitive p;
    p.kind = "polygon";
    p.color = m_color;
    double px, py, d = 0;
    d += mapXYZ(x1, y1, z1, px, py);
    p.points.emplace_back(px, py);
    d += mapXYZ(x2, y2, z2, px, py);
    p.points.emplace_back(px, py);
    d += mapXYZ(x3, y3, z3, px, py);
    p.points.emplace_back(px, py);
    p.depth = d / 3.0;
    (m_inDirect ? m_prims : m_pending3d).push_back(p);
}
//=============================================================================
bool
RenderWeb::polygonFill(
    const std::vector<std::vector<double>>& points, const std::vector<double>& faceColor)
{
    // Emit the whole outline as a single filled polygon so a concave or multi-vertex
    // fill (box-chart notch, violin density) stays seam-free. A fan of separate
    // primitives shows its anti-aliased shared edges across a translucent fill.
    if (points.size() < 3 || faceColor.size() < 3) {
        return false;
    }
    WebDrawPrimitive p;
    p.kind = "polygon";
    p.color = colorString(faceColor);
    double depthSum = 0.0;
    for (const std::vector<double>& pt : points) {
        if (pt.size() < 2) {
            return false;
        }
        double px = 0.;
        double py = 0.;
        depthSum += mapXYZ(pt[0], pt[1], pt.size() > 2 ? pt[2] : 0.0, px, py);
        p.points.emplace_back(px, py);
    }
    p.depth = depthSum / static_cast<double>(points.size());
    (m_inDirect ? m_prims : m_pending3d).push_back(p);
    return true;
}
//=============================================================================
void
RenderWeb::triLine(double x1, double y1, double z1, double x2, double y2, double z2, double x3,
    double y3, double z3)
{
    WebDrawPrimitive p;
    p.kind = "polyline";
    p.color = m_color;
    p.width = m_lineWidth;
    double px, py;
    mapXY(x1, y1, z1, px, py);
    p.points.emplace_back(px, py);
    mapXY(x2, y2, z2, px, py);
    p.points.emplace_back(px, py);
    mapXY(x3, y3, z3, px, py);
    p.points.emplace_back(px, py);
    mapXY(x1, y1, z1, px, py);
    p.points.emplace_back(px, py);
    m_prims.push_back(p);
}
//=============================================================================
void
RenderWeb::rect(double x1, double y1, double x2, double y2)
{
    quadline(x1, y1, 0, x2, y1, 0, x2, y2, 0, x1, y2, 0);
}
//=============================================================================
void
RenderWeb::rectFill(double x1, double y1, double x2, double y2)
{
    quad(x1, y1, 0, x2, y1, 0, x2, y2, 0, x1, y2, 0);
}
//=============================================================================
void
RenderWeb::circle(double x1, double y1, double radius)
{
    WebDrawPrimitive p;
    p.kind = "polyline";
    p.color = m_color;
    p.width = m_lineWidth;
    constexpr int seg = 20;
    constexpr double twoPi = 6.28318530717958647692;
    for (int i = 0; i <= seg; i++) {
        double ang = twoPi * i / seg;
        double px, py;
        mapXY(x1 + radius * std::cos(ang), y1 + radius * std::sin(ang), 0, px, py);
        p.points.emplace_back(px, py);
    }
    m_prims.push_back(p);
}
//=============================================================================
void
RenderWeb::circleFill(double x1, double y1, double radius)
{
    // A filled disc: circle() only strokes, which leaves filled markers hollow.
    WebDrawPrimitive p;
    p.kind = "polygon";
    p.color = m_color;
    constexpr int seg = 24;
    constexpr double twoPi = 6.28318530717958647692;
    double depthSum = 0.0;
    for (int i = 0; i < seg; i++) {
        const double ang = twoPi * i / seg;
        double px, py;
        depthSum += mapXYZ(x1 + radius * std::cos(ang), y1 + radius * std::sin(ang), 0, px, py);
        p.points.emplace_back(px, py);
    }
    // Record the clip-space depth like every other deferred fill (quad/tri). A
    // circleFill in the depth-sorted queue (e.g. the polar axes background) must
    // stay coplanar with the polylines at the same z so stable_sort keeps the call
    // order. With the default depth 0 it sorts above strokes at a positive z and
    // hides the polar grid and data.
    p.depth = seg ? depthSum / static_cast<double>(seg) : 0.0;
    (m_inDirect ? m_prims : m_pending3d).push_back(p);
}
//=============================================================================
static void
packBatchColors(std::vector<uint8_t>& out, const std::vector<double>& rgba, size_t i)
{
    for (int k = 0; k < 4; k++) {
        double v = rgba[i * 4 + k];
        v = v < 0.0 ? 0.0 : (v > 1.0 ? 1.0 : v);
        out.push_back(static_cast<uint8_t>(std::lround(v * 255.0)));
    }
}
//=============================================================================
bool
RenderWeb::pointMarkers(const std::vector<double>& xs, const std::vector<double>& ys,
    const std::vector<double>& markerSizes, const std::vector<double>& rgbaColors)
{
    // Kept for the dot fast path: a filled disc with the given colour, no outline.
    return markerBatch(RenderInterface::Dot, xs, ys, markerSizes, rgbaColors, { }, 1.0);
}
//=============================================================================
void
RenderWeb::writeSceneDepth(int px, int py, double z)
{
    if (m_depthW <= 0 || m_depthH <= 0 || px < 0 || py < 0 || px >= m_depthW || py >= m_depthH) {
        return;
    }
    double& slot = m_sceneDepth[static_cast<size_t>(py) * m_depthW + px];
    if (z < slot) {
        slot = z;
    }
}
//=============================================================================
bool
RenderWeb::markerOccluded(double px, double py, double z) const
{
    if (m_sceneDepth.empty() || m_depthW <= 0 || m_depthH <= 0) {
        return false;
    }
    const int ix = static_cast<int>(std::lround(px));
    const int iy = static_cast<int>(std::lround(py));
    if (ix < 0 || iy < 0 || ix >= m_depthW || iy >= m_depthH) {
        return false;
    }
    const double surfaceZ = m_sceneDepth[static_cast<size_t>(iy) * m_depthW + ix];
    if (!std::isfinite(surfaceZ)) {
        return false;
    }
    // Clip-space z is in [-1, 1]; a bias well under any real layer gap keeps a
    // marker sitting on the surface visible while dropping one genuinely behind it.
    constexpr double depthBias = 2.0e-4;
    return z > surfaceZ + depthBias;
}
//=============================================================================
bool
RenderWeb::markerBatch(int shape, const std::vector<double>& xs, const std::vector<double>& ys,
    const std::vector<double>& markerSizes, const std::vector<double>& faceRgba,
    const std::vector<double>& edgeRgba, double edgeWidth, const std::vector<double>& depths)
{
    // A whole marker set as a single "points" primitive: the frontend replays it as
    // N canvas shapes and the software rasterizer draws N shapes. One polygon/stroke
    // set per marker would, past WEB_DISPLAY_LIST_RASTER_THRESHOLD, force a
    // full-figure PNG on every animation frame. Centres and radii are mapped like
    // DrawSymbol maps its vertices, so the batch stays pixel-aligned.
    const size_t n = xs.size();
    if (n == 0 || ys.size() != n || markerSizes.size() != n) {
        return false;
    }
    const bool hasFace = faceRgba.size() == n * 4;
    const bool hasEdge = edgeRgba.size() == n * 4;
    if (!hasFace && !hasEdge) {
        return false;
    }
    // A dot is a small filled disc drawn in the (edge) colour; fold it onto the
    // circle shape here so every consumer only knows the general shapes.
    const bool isDot = (shape == RenderInterface::Dot);
    // The point marker '.' renders at a third of the nominal MarkerSize.
    const double sizeScale = isDot ? (1.0 / 3.0) : 1.0;
    WebDrawPrimitive p;
    p.kind = "points";
    p.pointShape = isDot ? RenderInterface::Circle : shape;
    p.pointEdgeWidth = pointsToPixels(edgeWidth);
    p.pointCenters.reserve(n * 2);
    p.pointRadii.reserve(n);
    if (hasFace) {
        p.pointColors.reserve(n * 4);
    }
    if (hasEdge) {
        p.pointEdgeColors.reserve(n * 4);
    }
    // There is no per-pixel depth buffer for the painter-sorted display list, so a
    // marker carrying a clip-space z is tested against the opaque surfaces already
    // rasterized into the scene depth buffer and dropped when it sits behind one.
    const bool depthAware = depths.size() == n;
    for (size_t i = 0; i < n; i++) {
        double cx, cy, ex, ey;
        mapXYZ(xs[i], ys[i], 0, cx, cy);
        if (depthAware && markerOccluded(cx, cy, depths[i])) {
            continue;
        }
        const double halfSize = sizeScale * pointsToPixels(markerSizes[i]) / 2.0;
        mapXYZ(xs[i] + halfSize, ys[i], 0, ex, ey);
        const double radius = std::hypot(ex - cx, ey - cy);
        p.pointCenters.push_back(static_cast<float>(cx));
        p.pointCenters.push_back(static_cast<float>(cy));
        p.pointRadii.push_back(static_cast<float>(radius));
        if (hasFace) {
            packBatchColors(p.pointColors, faceRgba, i);
        }
        if (hasEdge) {
            packBatchColors(p.pointEdgeColors, edgeRgba, i);
        }
    }
    if (p.pointRadii.empty()) {
        return true;
    }
    (m_inDirect ? m_prims : m_pending3d).push_back(std::move(p));
    return true;
}
//=============================================================================
bool
RenderWeb::rectBatch(const std::vector<double>& corners, const std::vector<double>& faceColor,
    const std::vector<double>& edgeColor, double edgeWidth)
{
    // A whole bar/histogram series as one "rects" primitive: the two opposite
    // data-space corners of each bar are projected like quad() maps them and stored
    // as a screen rectangle. This keeps a many-bin histogram off the whole-figure
    // raster path.
    const size_t n = corners.size() / 4;
    if (n == 0 || corners.size() != n * 4) {
        return false;
    }
    const bool hasFace = faceColor.size() >= 3 && faceColor[0] != -1;
    const bool hasEdge = edgeColor.size() >= 3 && edgeColor[0] != -1;
    if (!hasFace && !hasEdge) {
        return false;
    }
    WebDrawPrimitive p;
    p.kind = "rects";
    if (hasFace) {
        p.color = colorString(faceColor);
    }
    if (hasEdge) {
        p.stroke = colorString(edgeColor);
        p.width = pointsToPixels(edgeWidth);
    }
    p.rectXYWH.reserve(n * 4);
    for (size_t i = 0; i < n; i++) {
        double ax, ay, bx, by;
        mapXYZ(corners[i * 4], corners[i * 4 + 1], 0, ax, ay);
        mapXYZ(corners[i * 4 + 2], corners[i * 4 + 3], 0, bx, by);
        const double x = std::min(ax, bx);
        const double y = std::min(ay, by);
        p.rectXYWH.push_back(static_cast<float>(x));
        p.rectXYWH.push_back(static_cast<float>(y));
        p.rectXYWH.push_back(static_cast<float>(std::abs(bx - ax)));
        p.rectXYWH.push_back(static_cast<float>(std::abs(by - ay)));
    }
    (m_inDirect ? m_prims : m_pending3d).push_back(std::move(p));
    return true;
}
//=============================================================================
void
RenderWeb::drawPieSlice(double cx, double cy, double outerR, double innerR, double startAngle,
    double spanAngle, const std::vector<double>& faceColor, const std::vector<double>& edgeColor,
    double lineWidth)
{
    // Wedge (or annular segment when innerR > 0) as a filled polygon. Angles are in
    // degrees.
    if (outerR <= 0. || spanAngle == 0.) {
        return;
    }
    const double deg2rad = 3.14159265358979323846 / 180.0;
    const int seg = std::max(8, (int)std::ceil(std::fabs(spanAngle) / 4.0));
    std::vector<std::pair<double, double>> ring;
    ring.reserve(2 * (seg + 1) + 1);
    for (int i = 0; i <= seg; i++) {
        const double a = (startAngle + spanAngle * i / seg) * deg2rad;
        double px, py;
        mapXY(cx + outerR * std::cos(a), cy + outerR * std::sin(a), 0, px, py);
        ring.emplace_back(px, py);
    }
    if (innerR > 0.) {
        for (int i = seg; i >= 0; i--) {
            const double a = (startAngle + spanAngle * i / seg) * deg2rad;
            double px, py;
            mapXY(cx + innerR * std::cos(a), cy + innerR * std::sin(a), 0, px, py);
            ring.emplace_back(px, py);
        }
    } else {
        double px, py;
        mapXY(cx, cy, 0, px, py);
        ring.emplace_back(px, py);
    }
    WebDrawPrimitive p;
    p.kind = "polygon";
    p.points = ring;
    p.color = colorString(faceColor);
    if (edgeColor.size() >= 3 && lineWidth > 0.) {
        p.stroke = colorString(edgeColor);
        p.width = lineWidth;
    }
    (m_inDirect ? m_prims : m_pending3d).push_back(p);
}
//=============================================================================
void
RenderWeb::setupDirectDraw()
{
    // Save the current clip so releaseDirectDraw can undo the widening to the whole
    // figure below. Otherwise the axes-box clip is lost and the data spills outside
    // the axes after a pan/zoom.
    {
        WebDrawPrimitive p;
        p.kind = "clipSave";
        m_prims.push_back(p);
    }
    // Direct draw paints on top of the 3D scene: emit what is buffered first,
    // then stop buffering so screen-space primitives keep their call order.
    flush3d();
    m_inDirect = true;
    for (int i = 0; i < 16; i++) {
        save_model[i] = model[i];
        model[i] = 0;
        save_proj[i] = proj[i];
    }
    for (int i = 0; i < 4; i++) {
        save_viewp[i] = viewp[i];
    }
    // Direct draw: identity modelview plus an ORTHOGRAPHIC projection in pixel
    // units, as RenderQt does. An identity proj sends tick labels, axis labels and
    // titles far off-canvas.
    model[0] = model[5] = model[10] = model[15] = 1;
    viewport(m_x1, m_y1, m_width, m_height);
    project(m_x1, m_x1 + m_width, m_y1, m_y1 + m_height, -1, 1);
}
//=============================================================================
void
RenderWeb::releaseDirectDraw()
{
    m_inDirect = false;
    for (int i = 0; i < 16; i++) {
        model[i] = save_model[i];
        proj[i] = save_proj[i];
    }
    for (int i = 0; i < 4; i++) {
        viewp[i] = save_viewp[i];
    }
    // Restore the clip saved in setupDirectDraw.
    WebDrawPrimitive p;
    p.kind = "clipRestore";
    m_prims.push_back(p);
}
//=============================================================================
void
RenderWeb::beginSurfaceGroup()
{
    // Mark that the opaque faces emitted here (3D bar cuboids) must occlude the
    // bar edges drawn alongside them. The nesting counter lets a histogram2
    // group sit inside the axis depth-sorted group without a mismatched reset.
    m_surfaceGroupDepth++;
}
//=============================================================================
void
RenderWeb::endSurfaceGroup()
{
    if (m_surfaceGroupDepth > 0) {
        m_surfaceGroupDepth--;
    }
    if (m_surfaceGroupDepth == 0) {
        // The axes' children are all drawn, so the opaque scene depth is complete:
        // rasterize the translucent patch faces against it now.
        flushDeferredTranslucent();
    }
}
//=============================================================================
void
RenderWeb::putText(double x, double y, std::wstring txt, std::vector<double> col,
    AlignmentFlag xflag, AlignmentFlag yflag, const FontSpec& fnt, double rotation)
{
    // Anchor as RenderQt::putText: shift by the measured extent for the requested
    // alignment (plus the font descent), rotate, then map. The result is a
    // baseline-anchored, left-aligned draw on the client. Metrics are Qt-free
    // (core-font advance tables by family).
    const bool bold = fnt.bold;
    const bool italic = fnt.italic;
    const double pixelSize = webFontPixelSize(fnt);
    const double width = FontMetrics::textWidthPixels(txt, fnt.family, pixelSize, bold, italic);
    const double height = FontMetrics::textHeightPixels(txt, fnt.family, pixelSize, bold, italic);
    const std::vector<std::wstring> lines = (txt.find(L'\n') == std::wstring::npos)
        ? std::vector<std::wstring> { txt }
        : splitEoLText(txt);
    const double lineHeight = (lines.size() > 1)
        ? FontMetrics::lineHeightPixels(fnt.family, pixelSize, bold, italic)
        : 0.0;
    double xdelta = 0.0;
    double ydelta = 0.0;
    if (xflag == Mean) {
        xdelta = -width / 2.0;
    } else if (xflag == Max) {
        xdelta = -width;
    }
    if (yflag == Mean) {
        ydelta = -height / 2.0;
    } else if (yflag == Max) {
        ydelta = -height;
    }
    ydelta += FontMetrics::descentPixels(fnt.family, pixelSize, bold, italic);
    const double costhet = std::cos(rotation * M_PI / 180.0);
    const double sinthet = std::sin(rotation * M_PI / 180.0);

    // The client draws one line of text per primitive, so a multi-line string
    // is emitted line by line. The last line keeps the anchor a single line
    // would have had and earlier lines stack above it (the projection is
    // y-up here), which is how RenderQt lays the same block out.
    for (size_t k = 0; k < lines.size(); ++k) {
        const double lineDelta = ydelta + (lines.size() - 1 - k) * lineHeight;
        const double xpos = x + xdelta * costhet - lineDelta * sinthet;
        const double ypos = y + xdelta * sinthet + lineDelta * costhet;

        WebDrawPrimitive p;
        p.kind = "text";
        double px, py;
        mapXY(xpos, ypos, 0, px, py);
        p.x = px;
        p.y = py;
        p.text = wstring_to_utf8(lines[k]);
        p.color = colorString(col);
        p.size = pixelSize;
        p.fontFamily = FontMetrics::genericFamily(fnt.family);
        p.fontBold = bold;
        p.fontItalic = italic;
        p.align = "left"; // alignment is already baked into the anchor
        p.rotation = rotation;
        m_prims.push_back(p);
    }
}
//=============================================================================
void
RenderWeb::measureText(std::wstring txt, const FontSpec& fnt, AlignmentFlag xflag,
    AlignmentFlag yflag, int& width, int& height, int& xoffset, int& yoffset)
{
    // Must match RenderQt::measureText closely: the axes use these offsets to lay
    // out labels AND to decide how many ticks fit. Computed without Qt, from a
    // Helvetica advance-width table scaled by the pixel size.
    const double px = webFontPixelSize(fnt);
    width = static_cast<int>(
        std::lround(FontMetrics::textWidthPixels(txt, fnt.family, px, fnt.bold, fnt.italic)));
    height = static_cast<int>(
        std::lround(FontMetrics::textHeightPixels(txt, fnt.family, px, fnt.bold, fnt.italic)));
    yoffset = -height;
    xoffset = 0;
    if (xflag == Mean) {
        xoffset -= width / 2;
    } else if (xflag == Max) {
        xoffset -= width;
    }
    if (yflag == Mean) {
        yoffset += height / 2;
    } else if (yflag == Min) {
        yoffset += height;
    }
}
//=============================================================================
void
RenderWeb::setSurfaceTexture(const RGBAImageView& texture)
{
    if (texture.pixels == nullptr || texture.width <= 0 || texture.height <= 0) {
        clearSurfaceTexture();
        return;
    }
    m_surfaceTextureW = texture.width;
    m_surfaceTextureH = texture.height;
    const int stride = texture.bytesPerLine > 0 ? texture.bytesPerLine : texture.width * 4;
    m_surfaceTextureRgba.resize(static_cast<size_t>(texture.width) * texture.height * 4);
    for (int row = 0; row < texture.height; ++row) {
        const unsigned char* src = texture.pixels + static_cast<size_t>(row) * stride;
        unsigned char* dst
            = m_surfaceTextureRgba.data() + static_cast<size_t>(row) * texture.width * 4;
        std::memcpy(dst, src, static_cast<size_t>(texture.width) * 4);
    }
    m_surfaceTextureActive = true;
}
//=============================================================================
void
RenderWeb::clearSurfaceTexture()
{
    m_surfaceTextureActive = false;
    m_surfaceTextureW = 0;
    m_surfaceTextureH = 0;
    m_surfaceTextureRgba.clear();
}
//=============================================================================
void
RenderWeb::quadStrips(const std::vector<std::vector<coloredPoint>>& faces, bool flatfaces,
    const std::vector<std::vector<coloredPoint>>& edges, bool, meshStyle meshstyle,
    double lineWidth, const std::wstring& lineStyle, enum LightingMode::LightingMode faceLighting,
    enum LightingMode::LightingMode edgeLighting, uint64_t geometryToken)
{
    // The face and edge strips of a pair can have different lengths (meshc mixes a
    // mesh with contour strips), so every access into the edge strip is
    // bounds-checked.
    // Each entry of `faces`/`edges` is a STRIP of points, not a single quad: a quad
    // is (j-2, j-1, j+1, j) in ring order, coloured flat from index j-2, as RenderQt
    // walks them. One polygon per strip would self-intersect. Quads are
    // painter-sorted far-to-near, which replaces the depth buffer on the 2D canvas.
    m_has3d = true;
    const bool strokeEdges = lineStyle != GO_PROPERTY_VALUE_NONE_STR && lineWidth > 0;
    // Bake per-vertex Gouraud lighting once, as RenderOpenGL::quadStrips does, and
    // feed the lit strips to every consumer. The browser WebGL shader is a color
    // pass-through, which stays correct because the webview re-renders server-side
    // on every view change. The software z-buffer and painter fallback used by
    // getframe / saveas consume the same lit strips.
    const bool hasLights = m_lightingScene.enabled && !m_lightingScene.lights.empty();
    const bool lightFaces = hasLights && faceLighting != LightingMode::None;
    const bool lightEdges = hasLights && edgeLighting != LightingMode::None;
    const std::vector<std::vector<coloredPoint>>* faceStrips = &faces;
    const std::vector<std::vector<coloredPoint>>* edgeStrips = &edges;
    std::vector<std::vector<coloredPoint>> litFaces;
    std::vector<std::vector<coloredPoint>> litEdges;
    if (lightFaces || lightEdges) {
        const RenderLighting::PreparedLighting prepared
            = RenderLighting::prepareLighting(model, m_lightingScene, m_lightingMaterial);
        auto bakeLighting = [&prepared](std::vector<std::vector<coloredPoint>>& strips) {
            for (auto& strip : strips) {
                for (auto& pt : strip) {
                    RGBAColorData lit
                        = RenderLighting::computeLitColor(RGBAColorData(pt.r, pt.g, pt.b, pt.a),
                            point(pt.nx, pt.ny, pt.nz), point(pt.x, pt.y, pt.z), prepared);
                    pt.r = lit.r;
                    pt.g = lit.g;
                    pt.b = lit.b;
                    pt.a = lit.a;
                }
            }
        };
        if (lightFaces) {
            litFaces = faces;
            bakeLighting(litFaces);
            faceStrips = &litFaces;
        }
        if (lightEdges) {
            litEdges = edges;
            bakeLighting(litEdges);
            edgeStrips = &litEdges;
        }
    }
    // Lighting is baked into the vertex colours above and therefore depends on
    // the current view and light transforms. Reusing the object's raw geometry
    // token would also reuse stale colours after a camera or light change.
    const uint64_t webGeometryToken = (lightFaces || lightEdges) ? 0 : geometryToken;
    // A textured surface is not pushed as WebGL mesh geometry: the color-only shader
    // cannot sample the texture and the browser would draw the white base mesh. The
    // software z-buffer image below keeps the texture crisp in the interactive figure
    // and in the capture.
    const bool webglSurface = m_surfaceTextureActive
        ? false
        : tryPushWebGlSurface(*faceStrips, flatfaces, *edgeStrips, meshstyle, lineWidth, lineStyle,
              webGeometryToken);
    if (webglSurface && !m_softwareFallbackEnabled) {
        return;
    }
    // Gouraud lighting must interpolate the per-vertex lit colors even when the
    // face color mode is flat; flat lighting keeps the provoking-vertex color.
    const bool flatShade = flatfaces && !(lightFaces && faceLighting == LightingMode::Gouraud);
    // The z-buffered software raster interpolates vertex colours per pixel, so it is
    // the correct capture for smoothly-shaded surfaces (`shading interp`, Gouraud-lit
    // faces) under getframe / saveas: the flat-polygon fallback below collapses each
    // quad to its provoking-vertex colour. tryPushWebGlSurface always succeeds (it
    // only records geometry), so this path cannot be gated on !webglSurface. Restrict
    // it to interpolated shading: a flat surface bakes to one full-frame image, which
    // drops the per-quad depths the painter fallback needs to interleave intersecting
    // flat surfaces. The image is tagged as a WebGL fallback so the browser keeps
    // using the mesh and never double-draws.
    // A per-pixel z-buffer is the only correct capture for an opaque surface: the
    // mean-z painter fallback below cannot order large adjacent quads (a meshz skirt,
    // a coarse mesh). Every opaque surface, faces and wireframe edges, goes through
    // it. Semi-transparent surfaces stay on the painter path, which blends their
    // quads back-to-front.
    auto surfaceIsOpaque = [](const std::vector<std::vector<coloredPoint>>& strips) {
        for (const auto& strip : strips) {
            for (const coloredPoint& p : strip) {
                if (p.a < 0.999) {
                    return false;
                }
            }
        }
        return true;
    };
    const bool surfaceOpaque = !faces.empty() && surfaceIsOpaque(*faceStrips);
    // A transparent surface takes the z-buffer image path only once an opaque surface
    // has recorded its depth this frame, so its faces can be culled behind that
    // geometry and blended over it in front. A lone transparent surface keeps the
    // painter path, which blends its quads back-to-front and interleaves them with
    // other primitives.
    const bool transparentOverOpaque = !faces.empty() && !surfaceOpaque && !m_sceneDepth.empty();
    if (surfaceOpaque || transparentOverOpaque) {
        // Oversample only when there are thin edge lines to keep crisp.
        const int oversample = strokeEdges ? 2 : 1;
        if (tryPushZBufferedSurface(*faceStrips, flatfaces, flatShade, *edgeStrips, strokeEdges,
                meshstyle, lineWidth, webglSurface, oversample, !surfaceOpaque)) {
            return;
        }
    }
    // The painter-sorted 2D fallback (lit surfaces with stroked edges) reads its
    // flat quad color from the provoking vertex, so consume the lit copies too;
    // this yields flat-lit quads, the best a per-quad canvas polygon can do.
    const std::vector<std::vector<coloredPoint>>& facesLit = *faceStrips;
    const std::vector<std::vector<coloredPoint>>& edgesLit = *edgeStrips;
    if (flatfaces && edgesLit.empty() && facesLit.size() == 1 && facesLit[0].size() > 4
        && (facesLit[0].size() % 4) == 0) {
        for (size_t j = 0; j + 3 < facesLit[0].size(); j += 4) {
            double px[4], py[4];
            double depth = 0;
            for (int k = 0; k < 4; ++k) {
                const coloredPoint& cp = facesLit[0][j + k];
                depth += mapXYZ(cp.x, cp.y, cp.z, px[k], py[k]);
            }
            WebDrawPrimitive p;
            p.webglFallback = webglSurface;
            p.kind = "polygon";
            const int ring[4] = { 0, 1, 3, 2 };
            for (int k : ring) {
                p.points.emplace_back(px[k], py[k]);
            }
            const coloredPoint& fc = facesLit[0][j];
            p.color = colorString({ fc.r, fc.g, fc.b, fc.a });
            p.depth = depth / 4.0;
            m_pending3d.push_back(std::move(p));
        }
        return;
    }
    const size_t stripCount = std::max(facesLit.size(), edgesLit.size());
    for (size_t i = 0; i < stripCount; ++i) {
        const std::vector<coloredPoint>* faceStrip = i < facesLit.size() ? &facesLit[i] : nullptr;
        const std::vector<coloredPoint>* edgeStrip = i < edgesLit.size() ? &edgesLit[i] : nullptr;
        const std::vector<coloredPoint>& source = faceStrip ? *faceStrip : *edgeStrip;
        for (size_t j = 2; j + 1 < source.size(); j += 2) {
            const size_t idx[4] = { j - 2, j - 1, j + 1, j };
            double px[4], py[4];
            double depth = 0;
            for (int k = 0; k < 4; ++k) {
                const coloredPoint& cp = source[idx[k]];
                depth += mapXYZ(cp.x, cp.y, cp.z, px[k], py[k]);
            }
            depth /= 4.0;
            if (faceStrip != nullptr) {
                const coloredPoint& fc = (*faceStrip)[j - 2];
                WebDrawPrimitive p;
                p.webglFallback = webglSurface;
                p.kind = "polygon";
                for (int k = 0; k < 4; ++k) {
                    p.points.emplace_back(px[k], py[k]);
                }
                p.color = colorString({ fc.r, fc.g, fc.b, fc.a });
                p.depth = depth;
                // Mesh drawn as the quad's own outline (like RenderQt) so it can
                // never z-fight with its face.
                if (edgeStrip != nullptr && strokeEdges && meshstyle == meshStyle::Both
                    && j - 2 < edgeStrip->size()) {
                    const coloredPoint& ec = (*edgeStrip)[j - 2];
                    if (ec.a > 0) {
                        p.stroke = colorString({ ec.r, ec.g, ec.b, ec.a });
                        p.width = lineWidth;
                    }
                }
                m_pending3d.push_back(std::move(p));
            }
            if (edgeStrip != nullptr && strokeEdges && j - 2 < edgeStrip->size()
                && !(faceStrip != nullptr && meshstyle == meshStyle::Both)) {
                const coloredPoint& ec = (*edgeStrip)[j - 2];
                if (ec.a > 0) {
                    const std::string col = colorString({ ec.r, ec.g, ec.b, ec.a });
                    auto segment = [&](int a, int b) {
                        WebDrawPrimitive e;
                        e.webglFallback = webglSurface;
                        e.kind = "line";
                        e.x1 = px[a];
                        e.y1 = py[a];
                        e.x2 = px[b];
                        e.y2 = py[b];
                        e.color = col;
                        e.width = lineWidth;
                        e.depth = depth;
                        m_pending3d.push_back(std::move(e));
                    };
                    if (meshstyle == meshStyle::Both || meshstyle == meshStyle::Column) {
                        segment(0, 1);
                        segment(2, 3);
                    }
                    if (meshstyle == meshStyle::Both || meshstyle == meshStyle::Row) {
                        segment(1, 2);
                        segment(3, 0);
                    }
                }
            }
        }
    }
}
//=============================================================================
std::vector<std::wstring>
splitEoLText(const std::wstring& text)
{
    std::vector<std::wstring> lines;
    std::wstring::size_type start = 0;
    while (true) {
        std::wstring::size_type stop = text.find(L'\n', start);
        std::wstring line
            = text.substr(start, (stop == std::wstring::npos) ? std::wstring::npos : stop - start);
        if (!line.empty() && line.back() == L'\r') {
            line.pop_back();
        }
        lines.push_back(line);
        if (stop == std::wstring::npos) {
            break;
        }
        start = stop + 1;
    }
    return lines;
}
//=============================================================================
// Move one display-list entry by (dx, dy) screen pixels. Every coordinate a
// primitive can carry is listed here: a kind that grows a new one must be
// added, or a container would paint it at the figure origin.
void
translatePrimitive(WebDrawPrimitive& primitive, double dx, double dy)
{
    primitive.x += dx;
    primitive.y += dy;
    primitive.x1 += dx;
    primitive.y1 += dy;
    primitive.x2 += dx;
    primitive.y2 += dy;
    for (std::pair<double, double>& point : primitive.points) {
        point.first += dx;
        point.second += dy;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
