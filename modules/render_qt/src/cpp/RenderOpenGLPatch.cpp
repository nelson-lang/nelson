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
#include <array>
#include <cmath>
#include <cstddef>
#include <functional>
#include <set>
#include <tuple>
#include <vector>
#if defined(_WIN32)
#include <windows.h>
#ifdef min
#undef min
#endif
#ifdef max
#undef max
#endif
#endif
#if defined(__APPLE__)
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif
#include "RenderOpenGL.hpp"
#include "PatchTriangulate.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
using PatchPointKey = std::tuple<long long, long long, long long>;
using PatchEdgeKey = std::tuple<PatchPointKey, PatchPointKey>;
//=============================================================================
static void
combinePatchRenderCacheHash(uint64_t& seed, uint64_t value)
{
    seed ^= value + 0x9e3779b97f4a7c15ULL + (seed << 6) + (seed >> 2);
}
//=============================================================================
static uint64_t
patchDisplayListCacheKey(const PatchRenderOptions& options)
{
    uint64_t seed = options.geometryToken;
    combinePatchRenderCacheHash(seed, 0x70617463684c6973ULL);
    combinePatchRenderCacheHash(seed, options.uniqueEdges ? 1 : 0);
    combinePatchRenderCacheHash(
        seed, static_cast<uint64_t>(std::llround(options.lineWidth * 1000.0)));
    combinePatchRenderCacheHash(
        seed, static_cast<uint64_t>(std::hash<std::wstring> {}(options.lineStyle)));
    combinePatchRenderCacheHash(seed, static_cast<uint64_t>(options.faceLighting));
    combinePatchRenderCacheHash(seed, static_cast<uint64_t>(options.edgeLighting));
    return seed;
}
//=============================================================================
static long long
quantizePatchCoordinate(double value)
{
    return static_cast<long long>(std::llround(value * 1e12));
}
//=============================================================================
static PatchPointKey
patchPointKey(const point& value)
{
    return PatchPointKey(quantizePatchCoordinate(value.x), quantizePatchCoordinate(value.y),
        quantizePatchCoordinate(value.z));
}
//=============================================================================
static bool
insertPatchEdge(std::set<PatchEdgeKey>& edges, const point& a, const point& b)
{
    PatchPointKey first = patchPointKey(a);
    PatchPointKey second = patchPointKey(b);
    if (second < first) {
        std::swap(first, second);
    }
    return edges.insert(PatchEdgeKey(first, second)).second;
}
//=============================================================================
static bool
isFinitePoint(const point& value)
{
    return std::isfinite(value.x) && std::isfinite(value.y) && std::isfinite(value.z);
}
//=============================================================================
static point
patchNormal(const Face& face)
{
    if (face.vertices.size() < 3) {
        return point(0, 0, 1);
    }
    const point& a = face.vertices[0];
    const point& b = face.vertices[1];
    const point& c = face.vertices[2];
    point normal((b.y - a.y) * (c.z - a.z) - (b.z - a.z) * (c.y - a.y),
        (b.z - a.z) * (c.x - a.x) - (b.x - a.x) * (c.z - a.z),
        (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x));
    double norm = std::sqrt(normal.x * normal.x + normal.y * normal.y + normal.z * normal.z);
    if (norm == 0 || !std::isfinite(norm)) {
        return point(0, 0, 1);
    }
    normal.x /= norm;
    normal.y /= norm;
    normal.z /= norm;
    return normal;
}
//=============================================================================
// The face fill colour's opacity. A patch is translucent when any face carries
// alpha < 1 (FaceAlpha < 1, or a per-vertex alpha channel below one).
static double
patchFaceAlpha(const Face& face)
{
    if ((face.FaceColorMode == ColorMode::Flat || face.FaceColorMode == ColorMode::Interp)
        && !face.vertexcolors.empty()) {
        return face.vertexcolors[0].a;
    }
    return face.FaceColor.a;
}
//=============================================================================
// Mean eye-space z of a face, using the current modelview (column-major). The
// camera looks down -z, so a farther face has a more negative value: sorting
// ascending draws farthest first, the back-to-front order translucency needs.
static double
patchFaceEyeZ(const Face& face, const double* model)
{
    if (face.vertices.empty()) {
        return 0.;
    }
    double z = 0.;
    for (const point& v : face.vertices) {
        z += model[2] * v.x + model[6] * v.y + model[10] * v.z + model[14];
    }
    return z / static_cast<double>(face.vertices.size());
}
//=============================================================================
// Resolves the fill colour for a face vertex. Returns false when the mode
// carries no per-vertex colour (so the draw keeps the current GL colour); the
// vertex-array builder then declines the fast path for that patch.
static bool
resolvePatchFaceColor(const Face& face, size_t vertexIndex, RGBAColorData& color)
{
    if (face.FaceColorMode == ColorMode::ColorSpec) {
        color = face.FaceColor;
        return true;
    }
    if (face.vertexcolors.empty()) {
        return false;
    }
    color = face.FaceColorMode == ColorMode::Interp && vertexIndex < face.vertexcolors.size()
        ? face.vertexcolors[vertexIndex]
        : face.vertexcolors[0];
    return true;
}
//=============================================================================
static bool
resolvePatchEdgeColor(const Face& face, size_t vertexIndex, RGBAColorData& color)
{
    if (face.EdgeColorMode == ColorMode::ColorSpec) {
        color = face.EdgeColor;
        return true;
    }
    if (face.edgecolors.empty()) {
        return false;
    }
    color = face.EdgeColorMode == ColorMode::Interp && vertexIndex < face.edgecolors.size()
        ? face.edgecolors[vertexIndex]
        : face.edgecolors[0];
    return true;
}
//=============================================================================
static void
setPatchFaceVertexColor(const Face& face, size_t vertexIndex)
{
    RGBAColorData color;
    if (resolvePatchFaceColor(face, vertexIndex, color)) {
        glColor4d(color.r, color.g, color.b, color.a);
    }
}
//=============================================================================
static void
setPatchEdgeVertexColor(const Face& face, size_t vertexIndex)
{
    RGBAColorData color;
    if (resolvePatchEdgeColor(face, vertexIndex, color)) {
        glColor4d(color.r, color.g, color.b, color.a);
    }
}
//=============================================================================
static void
emitPatchFaceVertex(
    const Face& face, size_t vertexIndex, const point& fallbackNormal, bool emitNormal)
{
    setPatchFaceVertexColor(face, vertexIndex);
    if (emitNormal) {
        const point& normal
            = vertexIndex < face.normals.size() ? face.normals[vertexIndex] : fallbackNormal;
        glNormal3d(normal.x, normal.y, normal.z);
    }
    const point& vertex = face.vertices[vertexIndex];
    glVertex3d(vertex.x, vertex.y, vertex.z);
}
//=============================================================================
static void
emitPatchEdgeVertex(
    const Face& face, size_t vertexIndex, const point& fallbackNormal, bool emitNormal)
{
    setPatchEdgeVertexColor(face, vertexIndex);
    if (emitNormal) {
        const point& normal
            = vertexIndex < face.normals.size() ? face.normals[vertexIndex] : fallbackNormal;
        glNormal3d(normal.x, normal.y, normal.z);
    }
    const point& vertex = face.vertices[vertexIndex];
    glVertex3d(vertex.x, vertex.y, vertex.z);
}
//=============================================================================
static void
drawPatchFillTriangles(const Face& face, const std::vector<std::array<size_t, 3>>& triangles,
    const point& fallbackNormal, bool emitNormals)
{
    glBegin(GL_TRIANGLES);
    for (const auto& triangle : triangles) {
        emitPatchFaceVertex(face, triangle[0], fallbackNormal, emitNormals);
        emitPatchFaceVertex(face, triangle[1], fallbackNormal, emitNormals);
        emitPatchFaceVertex(face, triangle[2], fallbackNormal, emitNormals);
    }
    glEnd();
}
//=============================================================================
static void
drawPatchFillFan(const Face& face, const std::vector<size_t>& subPath, const point& fallbackNormal,
    bool emitNormals)
{
    glBegin(GL_TRIANGLE_FAN);
    for (size_t index : subPath) {
        emitPatchFaceVertex(face, index, fallbackNormal, emitNormals);
    }
    glEnd();
}
//=============================================================================
static void
drawPatchFilledSubPath(const Face& face, const std::vector<size_t>& subPath,
    const point& fallbackNormal, bool emitNormals)
{
    if (subPath.size() < 3) {
        return;
    }
    if (subPath.size() == 3) {
        std::vector<std::array<size_t, 3>> triangle
            = { { { subPath[0], subPath[1], subPath[2] } } };
        drawPatchFillTriangles(face, triangle, fallbackNormal, emitNormals);
        return;
    }
    std::vector<std::array<size_t, 3>> triangles;
    triangles.reserve(subPath.size() - 2);
    if (triangulatePatchSubPath(face, subPath, fallbackNormal, triangles)) {
        drawPatchFillTriangles(face, triangles, fallbackNormal, emitNormals);
    } else {
        drawPatchFillFan(face, subPath, fallbackNormal, emitNormals);
    }
}
//=============================================================================
static void
drawPatchFilledFace(
    const Face& face, const point& fallbackNormal, bool emitNormals, bool smoothLighting)
{
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    glShadeModel((face.FaceColorMode == ColorMode::Interp || smoothLighting) ? GL_SMOOTH : GL_FLAT);

    std::vector<size_t> subPath;
    subPath.reserve(face.vertices.size());
    for (size_t i = 0; i < face.vertices.size(); ++i) {
        if (!isFinitePoint(face.vertices[i])) {
            drawPatchFilledSubPath(face, subPath, fallbackNormal, emitNormals);
            subPath.clear();
            continue;
        }
        subPath.push_back(i);
    }
    drawPatchFilledSubPath(face, subPath, fallbackNormal, emitNormals);
}
//=============================================================================
static void
drawPatchEdgeSubPath(const Face& face, const std::vector<size_t>& subPath,
    const point& fallbackNormal, bool emitNormals)
{
    if (subPath.size() < 2) {
        return;
    }
    glBegin(subPath.size() == 2 ? GL_LINES : GL_LINE_LOOP);
    for (size_t index : subPath) {
        emitPatchEdgeVertex(face, index, fallbackNormal, emitNormals);
    }
    glEnd();
}
//=============================================================================
static void
drawPatchEdgesFace(const Face& face, const point& fallbackNormal, bool emitNormals)
{
    std::vector<size_t> subPath;
    subPath.reserve(face.vertices.size());
    for (size_t i = 0; i < face.vertices.size(); ++i) {
        if (!isFinitePoint(face.vertices[i])) {
            drawPatchEdgeSubPath(face, subPath, fallbackNormal, emitNormals);
            subPath.clear();
            continue;
        }
        subPath.push_back(i);
    }
    drawPatchEdgeSubPath(face, subPath, fallbackNormal, emitNormals);
}
//=============================================================================
static void
drawPatchUniqueEdgeSubPath(const Face& face, const std::vector<size_t>& subPath,
    const point& fallbackNormal, bool emitNormals, std::set<PatchEdgeKey>& edges)
{
    if (subPath.size() < 2) {
        return;
    }
    size_t edgeCount = subPath.size() == 2 ? 1 : subPath.size();
    for (size_t i = 0; i < edgeCount; ++i) {
        size_t first = subPath[i];
        size_t second = subPath[(i + 1) % subPath.size()];
        if (!insertPatchEdge(edges, face.vertices[first], face.vertices[second])) {
            continue;
        }
        glBegin(GL_LINES);
        emitPatchEdgeVertex(face, first, fallbackNormal, emitNormals);
        emitPatchEdgeVertex(face, second, fallbackNormal, emitNormals);
        glEnd();
    }
}
//=============================================================================
static void
drawPatchUniqueEdgesFace(
    const Face& face, const point& fallbackNormal, bool emitNormals, std::set<PatchEdgeKey>& edges)
{
    std::vector<size_t> subPath;
    subPath.reserve(face.vertices.size());
    for (size_t i = 0; i < face.vertices.size(); ++i) {
        if (!isFinitePoint(face.vertices[i])) {
            drawPatchUniqueEdgeSubPath(face, subPath, fallbackNormal, emitNormals, edges);
            subPath.clear();
            continue;
        }
        subPath.push_back(i);
    }
    drawPatchUniqueEdgeSubPath(face, subPath, fallbackNormal, emitNormals, edges);
}
//=============================================================================
static void
collectPatchFaceTriangles(
    const Face& face, const point& fallbackNormal, std::vector<std::array<size_t, 3>>& out)
{
    std::vector<size_t> subPath;
    subPath.reserve(face.vertices.size());
    for (size_t i = 0; i < face.vertices.size(); ++i) {
        if (!isFinitePoint(face.vertices[i])) {
            collectPatchSubPathTriangles(face, subPath, fallbackNormal, out);
            subPath.clear();
            continue;
        }
        subPath.push_back(i);
    }
    collectPatchSubPathTriangles(face, subPath, fallbackNormal, out);
}
//=============================================================================
static void
appendPatchArrayVertex(std::vector<float>& pos, std::vector<float>& col, const point& vertex,
    const RGBAColorData& color)
{
    pos.push_back(static_cast<float>(vertex.x));
    pos.push_back(static_cast<float>(vertex.y));
    pos.push_back(static_cast<float>(vertex.z));
    col.push_back(static_cast<float>(color.r));
    col.push_back(static_cast<float>(color.g));
    col.push_back(static_cast<float>(color.b));
    col.push_back(static_cast<float>(color.a));
}
//=============================================================================
// Collects one boundary sub-path's edges into the line list, de-duplicating
// shared edges by position. An opaque shared edge looks the same drawn once or
// twice. Returns false when a vertex carries no edge colour or an edge is
// translucent: de-duplication would change the blend, so the caller falls back
// to the immediate path.
static bool
collectPatchEdgeSubPath(const Face& face, const std::vector<size_t>& subPath,
    std::set<PatchEdgeKey>& edges, PatchVertexArrays& out)
{
    if (subPath.size() < 2) {
        return true;
    }
    const size_t edgeCount = subPath.size() == 2 ? 1 : subPath.size();
    for (size_t i = 0; i < edgeCount; ++i) {
        const size_t first = subPath[i];
        const size_t second = subPath[(i + 1) % subPath.size()];
        if (!insertPatchEdge(edges, face.vertices[first], face.vertices[second])) {
            continue;
        }
        RGBAColorData c0, c1;
        if (!resolvePatchEdgeColor(face, first, c0) || !resolvePatchEdgeColor(face, second, c1)) {
            return false;
        }
        if (c0.a < 0.999 || c1.a < 0.999) {
            return false;
        }
        appendPatchArrayVertex(out.edgePos, out.edgeColor, face.vertices[first], c0);
        appendPatchArrayVertex(out.edgePos, out.edgeColor, face.vertices[second], c1);
    }
    return true;
}
//=============================================================================
// Flattens the patch into client-side triangle and line arrays. Returns false
// when a face/edge relies on the current GL colour (no per-vertex data) or an
// edge is translucent, cases the flat opaque arrays cannot reproduce faithfully.
static bool
buildPatchVertexArrays(const FaceList& faces, bool wantEdges, PatchVertexArrays& out)
{
    std::set<PatchEdgeKey> edges;
    std::vector<std::array<size_t, 3>> triangles;
    for (const auto& face : faces) {
        if (face.vertices.size() < 3) {
            continue;
        }
        const point fallbackNormal = patchNormal(face);
        if (face.FaceColorMode != ColorMode::None) {
            triangles.clear();
            collectPatchFaceTriangles(face, fallbackNormal, triangles);
            for (const auto& triangle : triangles) {
                for (size_t k = 0; k < 3; ++k) {
                    RGBAColorData color;
                    if (!resolvePatchFaceColor(face, triangle[k], color)) {
                        return false;
                    }
                    appendPatchArrayVertex(
                        out.facePos, out.faceColor, face.vertices[triangle[k]], color);
                }
            }
        }
        if (wantEdges && face.EdgeColorMode != ColorMode::None) {
            std::vector<size_t> subPath;
            subPath.reserve(face.vertices.size());
            for (size_t i = 0; i < face.vertices.size(); ++i) {
                if (!isFinitePoint(face.vertices[i])) {
                    if (!collectPatchEdgeSubPath(face, subPath, edges, out)) {
                        return false;
                    }
                    subPath.clear();
                    continue;
                }
                subPath.push_back(i);
            }
            if (!collectPatchEdgeSubPath(face, subPath, edges, out)) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
void
RenderOpenGL::drawPatch(const FaceList& faces, const PatchRenderOptions& options)
{
    const double lineWidthValue = options.lineWidth;
    const std::wstring& lineStyle = options.lineStyle;
    const bool useFaceLighting
        = options.faceLighting != LightingMode::None && !lightingScene.lights.empty();
    const bool useEdgeLighting
        = options.edgeLighting != LightingMode::None && !lightingScene.lights.empty();
    // A translucent patch (FaceAlpha < 1) cannot rely on the depth buffer to order
    // its faces: with depth writes on, a near face occludes the ones behind it and
    // the back faces never blend through. Draw the faces back-to-front (farthest
    // eye-z first) with depth writes disabled. Depth testing still clips against
    // opaque geometry.
    bool translucentFaces = false;
    for (const auto& face : faces) {
        if (face.vertices.size() >= 3 && face.FaceColorMode != ColorMode::None
            && patchFaceAlpha(face) < 0.999) {
            translucentFaces = true;
            break;
        }
    }
    std::vector<size_t> faceOrder(faces.size());
    for (size_t i = 0; i < faces.size(); ++i) {
        faceOrder[i] = i;
    }
    if (translucentFaces) {
        std::vector<double> eyeZ(faces.size());
        for (size_t i = 0; i < faces.size(); ++i) {
            eyeZ[i] = patchFaceEyeZ(faces[i], model);
        }
        std::stable_sort(faceOrder.begin(), faceOrder.end(),
            [&](size_t a, size_t b) { return eyeZ[a] < eyeZ[b]; });
    }
    const GLboolean fillDepthMask = translucentFaces ? GL_FALSE : GL_TRUE;
    auto drawPatchNow = [&]() {
        glDisable(GL_TEXTURE_2D);
        glDisable(GL_CULL_FACE);
        if (translucentFaces) {
            // A translucent patch must composite with standard source-over blending;
            // an earlier primitive may have left a different blend func active.
            glEnable(GL_BLEND);
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
            // The frame is grabbed as RGBA and flattened over a white page. Source-over
            // blending drives the destination alpha below one (0.4*0.4 + 1*0.6 = 0.76
            // after one face, lower as faces stack) and the surface washes out. The
            // buffer was cleared opaque: mask the alpha channel off while blending so
            // the stored alpha stays 1.
            glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_FALSE);
        }
        glDepthMask(fillDepthMask);
        glDepthFunc(GL_LEQUAL);
        glEnable(GL_DEPTH_TEST);
        glEnable(GL_POLYGON_OFFSET_FILL);
        glPolygonOffset(2, 2);
        lineWidth(lineWidthValue);
        setLineStyleGL(lineStyle);
        for (size_t faceIndex : faceOrder) {
            const auto& face = faces[faceIndex];
            if (face.vertices.size() < 3) {
                continue;
            }
            point fallbackNormal = patchNormal(face);
            if (face.FaceColorMode != ColorMode::None) {
                configureLighting(useFaceLighting);
                drawPatchFilledFace(face, fallbackNormal, useFaceLighting,
                    options.faceLighting == LightingMode::Gouraud && useFaceLighting);
                disableLighting();
            }

            if (!options.uniqueEdges && lineStyle != GO_PROPERTY_VALUE_NONE_STR
                && face.EdgeColorMode != ColorMode::None) {
                glDisable(GL_POLYGON_OFFSET_FILL);
                configureLighting(useEdgeLighting);
                glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
                glDepthMask(GL_FALSE);
                glShadeModel(face.EdgeColorMode == ColorMode::Interp ? GL_SMOOTH : GL_FLAT);
                drawPatchEdgesFace(face, fallbackNormal, useEdgeLighting);
                glDepthMask(fillDepthMask);
                disableLighting();
                glEnable(GL_POLYGON_OFFSET_FILL);
            }
        }
        if (options.uniqueEdges && lineStyle != GO_PROPERTY_VALUE_NONE_STR) {
            glDisable(GL_POLYGON_OFFSET_FILL);
            std::set<PatchEdgeKey> edges;
            for (const auto& face : faces) {
                if (face.vertices.size() < 3 || face.EdgeColorMode == ColorMode::None) {
                    continue;
                }
                point fallbackNormal = patchNormal(face);
                configureLighting(useEdgeLighting);
                glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
                glDepthMask(GL_FALSE);
                glShadeModel(face.EdgeColorMode == ColorMode::Interp ? GL_SMOOTH : GL_FLAT);
                drawPatchUniqueEdgesFace(face, fallbackNormal, useEdgeLighting, edges);
                glDepthMask(GL_TRUE);
                disableLighting();
            }
            glEnable(GL_POLYGON_OFFSET_FILL);
        }
        glDisable(GL_POLYGON_OFFSET_FILL);
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
        glDisable(GL_LINE_STIPPLE);
        // Leave depth writes and the full colour mask enabled for whatever the axes
        // draws next, even when a translucent patch changed them for its own fill.
        glDepthMask(GL_TRUE);
        if (translucentFaces) {
            glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
        }
        disableLighting();
    };
    auto drawPatchArrays = [&](const PatchVertexArrays& va) {
        glDisable(GL_TEXTURE_2D);
        glDisable(GL_CULL_FACE);
        glDepthFunc(GL_LEQUAL);
        glEnable(GL_DEPTH_TEST);
        // Flat and colour-spec faces carry one colour on every vertex, so smooth
        // shading renders them uniformly, matching the immediate path's GL_FLAT.
        glShadeModel(GL_SMOOTH);
        glEnableClientState(GL_VERTEX_ARRAY);
        glEnableClientState(GL_COLOR_ARRAY);
        if (!va.facePos.empty()) {
            glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
            glEnable(GL_POLYGON_OFFSET_FILL);
            glPolygonOffset(2, 2);
            glDepthMask(GL_TRUE);
            glVertexPointer(3, GL_FLOAT, 0, va.facePos.data());
            glColorPointer(4, GL_FLOAT, 0, va.faceColor.data());
            glDrawArrays(GL_TRIANGLES, 0, static_cast<GLsizei>(va.facePos.size() / 3));
            glDisable(GL_POLYGON_OFFSET_FILL);
        }
        if (!va.edgePos.empty()) {
            lineWidth(lineWidthValue);
            setLineStyleGL(lineStyle);
            glDepthMask(GL_FALSE);
            glVertexPointer(3, GL_FLOAT, 0, va.edgePos.data());
            glColorPointer(4, GL_FLOAT, 0, va.edgeColor.data());
            glDrawArrays(GL_LINES, 0, static_cast<GLsizei>(va.edgePos.size() / 3));
            glDepthMask(GL_TRUE);
            glDisable(GL_LINE_STIPPLE);
        }
        glDisableClientState(GL_COLOR_ARRAY);
        glDisableClientState(GL_VERTEX_ARRAY);
    };
    // Legacy display lists replay the per-vertex command stream every frame on common
    // drivers, so a dense mesh stays CPU-bound. An opaque, unlit patch is flattened
    // once per geometryToken into client-side vertex arrays and drawn with two
    // glDrawArrays calls. Lit or translucent patches keep the immediate path.
    const bool canUseVertexArrays
        = options.geometryToken != 0 && !useFaceLighting && !useEdgeLighting && !translucentFaces;
    if (canUseVertexArrays) {
        const uint64_t cacheKey = patchDisplayListCacheKey(options);
        auto found = patchVertexArrayCache.find(cacheKey);
        if (found == patchVertexArrayCache.end()) {
            const bool wantEdges = lineStyle != GO_PROPERTY_VALUE_NONE_STR;
            PatchVertexArrays arrays;
            if (buildPatchVertexArrays(faces, wantEdges, arrays)) {
                // Each entry holds the whole mesh geometry; cap by count to bound
                // memory when many large patches are drawn.
                if (patchVertexArrayCache.size() > 64) {
                    patchVertexArrayCache.clear();
                }
                found = patchVertexArrayCache.emplace(cacheKey, std::move(arrays)).first;
            }
        }
        if (found != patchVertexArrayCache.end()) {
            drawPatchArrays(found->second);
            restoreCurrentColor();
            return;
        }
        // Build declined (current-colour dependence or translucent edges): fall
        // through to the immediate path, which reproduces those cases exactly.
    }
    drawPatchNow();
    restoreCurrentColor();
}
//=============================================================================
