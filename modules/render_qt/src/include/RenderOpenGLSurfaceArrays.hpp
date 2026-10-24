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
#include "RenderInterface.hpp"
#include <algorithm>
#include <cmath>
#include <cstddef>
#include <vector>
//=============================================================================
// Draws a flat list of GL_QUADS (4 vertices per quad) back-to-front by mean
// eye-space z, using the current modelview (column-major). A translucent surface
// cannot order its quads through the depth buffer, so it is painted farthest
// first while the caller keeps depth writes disabled. The vertices stay in place;
// only a four-byte index per vertex is reordered and fed to glDrawElements, so a
// dense mesh is not copied (96 bytes per vertex) on every view.
inline void
drawSurfaceQuadsSortedByDepth(const std::vector<coloredPoint>& quadVertices, const double* model)
{
    const size_t quadCount = quadVertices.size() / 4;
    if (quadCount == 0) {
        return;
    }
    // Per-quad key: the sum of its vertices' eye-space z (row 2 of the modelview).
    // The /4 mean is dropped -- the sum is monotonic in the same order. The camera
    // looks down -z, so a farther quad is more negative and sorts first.
    std::vector<double> quadDepth(quadCount);
    for (size_t q = 0; q < quadCount; ++q) {
        double z = 0.;
        for (size_t k = 0; k < 4; ++k) {
            const coloredPoint& p = quadVertices[q * 4 + k];
            z += model[2] * p.x + model[6] * p.y + model[10] * p.z + model[14];
        }
        quadDepth[q] = z;
    }
    std::vector<GLuint> quadOrder(quadCount);
    for (size_t q = 0; q < quadCount; ++q) {
        quadOrder[q] = static_cast<GLuint>(q);
    }
    std::stable_sort(quadOrder.begin(), quadOrder.end(),
        [&](GLuint a, GLuint b) { return quadDepth[a] < quadDepth[b]; });
    std::vector<GLuint> indices;
    indices.reserve(quadCount * 4);
    for (GLuint q : quadOrder) {
        const GLuint vertexBase = q * 4;
        indices.push_back(vertexBase);
        indices.push_back(vertexBase + 1);
        indices.push_back(vertexBase + 2);
        indices.push_back(vertexBase + 3);
    }
    const char* base = reinterpret_cast<const char*>(quadVertices.data());
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
    glEnableClientState(GL_NORMAL_ARRAY);
    glVertexPointer(3, GL_DOUBLE, sizeof(coloredPoint), base + offsetof(coloredPoint, x));
    glColorPointer(4, GL_DOUBLE, sizeof(coloredPoint), base + offsetof(coloredPoint, r));
    glNormalPointer(GL_DOUBLE, sizeof(coloredPoint), base + offsetof(coloredPoint, nx));
    glDrawElements(GL_QUADS, static_cast<GLsizei>(indices.size()), GL_UNSIGNED_INT, indices.data());
    glDisableClientState(GL_NORMAL_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);
}
//=============================================================================
inline void
drawSurfaceQuadsArray(const std::vector<std::vector<coloredPoint>>& strips)
{
    size_t vertexCount = 0;
    for (const auto& strip : strips) {
        vertexCount += strip.size() == 4 ? 4 : 0;
    }
    if (vertexCount == 0) {
        return;
    }
    std::vector<coloredPoint> vertices;
    vertices.reserve(vertexCount);
    for (const auto& strip : strips) {
        if (strip.size() == 4) {
            vertices.push_back(strip[0]);
            vertices.push_back(strip[1]);
            vertices.push_back(strip[3]);
            vertices.push_back(strip[2]);
        }
    }
    const char* base = reinterpret_cast<const char*>(vertices.data());
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
    glEnableClientState(GL_NORMAL_ARRAY);
    glVertexPointer(3, GL_DOUBLE, sizeof(coloredPoint), base + offsetof(coloredPoint, x));
    glColorPointer(4, GL_DOUBLE, sizeof(coloredPoint), base + offsetof(coloredPoint, r));
    glNormalPointer(GL_DOUBLE, sizeof(coloredPoint), base + offsetof(coloredPoint, nx));
    glDrawArrays(GL_QUADS, 0, static_cast<GLsizei>(vertices.size()));
    glDisableClientState(GL_NORMAL_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);
}
//=============================================================================
inline bool
isFiniteSurfaceArrayPoint(const coloredPoint& pt)
{
    return std::isfinite(pt.x) && std::isfinite(pt.y) && std::isfinite(pt.z);
}
//=============================================================================
inline void
appendSurfaceArrayVertex(std::vector<float>& pos, std::vector<float>& col, const coloredPoint& p)
{
    pos.push_back(static_cast<float>(p.x));
    pos.push_back(static_cast<float>(p.y));
    pos.push_back(static_cast<float>(p.z));
    col.push_back(static_cast<float>(p.r));
    col.push_back(static_cast<float>(p.g));
    col.push_back(static_cast<float>(p.b));
    col.push_back(static_cast<float>(p.a));
}
//=============================================================================
// Flattens an opaque, unlit surface into tightly packed float triangle (faces)
// and line (mesh edges) position + RGBA streams, built once per geometryToken
// and drawn directly. Each quad A,B,C,D becomes two triangles (A,B,D),(B,C,D):
// GL_QUADS is slow on common drivers, and both triangles end on D, the provoking
// vertex of GL_QUADS, so GL_FLAT shading picks the same colour. Mesh edges follow
// the row/column walk of the immediate path, de-duplicated at the quad boundary.
inline void
buildSurfaceVertexArrays(const std::vector<std::vector<coloredPoint>>& faces,
    const std::vector<std::vector<coloredPoint>>& edges, meshStyle meshstyle, bool wantEdges,
    std::vector<float>& facePos, std::vector<float>& faceColor, std::vector<float>& edgePos,
    std::vector<float>& edgeColor)
{
    const bool packedFaceQuads
        = faces.size() == 1 && faces[0].size() > 4 && (faces[0].size() % 4) == 0;
    auto addQuad = [&](const coloredPoint& a, const coloredPoint& b, const coloredPoint& c,
                       const coloredPoint& d) {
        appendSurfaceArrayVertex(facePos, faceColor, a);
        appendSurfaceArrayVertex(facePos, faceColor, b);
        appendSurfaceArrayVertex(facePos, faceColor, d);
        appendSurfaceArrayVertex(facePos, faceColor, b);
        appendSurfaceArrayVertex(facePos, faceColor, c);
        appendSurfaceArrayVertex(facePos, faceColor, d);
    };
    if (packedFaceQuads) {
        const std::vector<coloredPoint>& packed = faces[0];
        for (size_t k = 0; k + 3 < packed.size(); k += 4) {
            addQuad(packed[k], packed[k + 1], packed[k + 3], packed[k + 2]);
        }
    } else {
        for (const std::vector<coloredPoint>& strip : faces) {
            for (size_t b = 0; b + 3 < strip.size(); b += 2) {
                addQuad(strip[b], strip[b + 1], strip[b + 3], strip[b + 2]);
            }
        }
    }
    if (!wantEdges) {
        return;
    }
    for (const std::vector<coloredPoint>& qlist : edges) {
        if (meshstyle == meshStyle::Both || meshstyle == meshStyle::Row) {
            for (size_t start = 0; start < 2; ++start) {
                const coloredPoint* previous = nullptr;
                for (size_t j = start; j < qlist.size(); j += 2) {
                    const coloredPoint& point = qlist[j];
                    if (!isFiniteSurfaceArrayPoint(point)) {
                        previous = nullptr;
                        continue;
                    }
                    if (previous != nullptr) {
                        appendSurfaceArrayVertex(edgePos, edgeColor, *previous);
                        appendSurfaceArrayVertex(edgePos, edgeColor, point);
                    }
                    previous = &point;
                }
            }
        }
        if (meshstyle == meshStyle::Both || meshstyle == meshStyle::Column) {
            for (size_t j = 0; j + 1 < qlist.size(); j += 2) {
                const coloredPoint& a = qlist[j];
                const coloredPoint& b = qlist[j + 1];
                if (isFiniteSurfaceArrayPoint(a) && isFiniteSurfaceArrayPoint(b)) {
                    appendSurfaceArrayVertex(edgePos, edgeColor, a);
                    appendSurfaceArrayVertex(edgePos, edgeColor, b);
                }
            }
        }
    }
}
//=============================================================================
// Builds the mesh wireframe as a single GL_LINES list and draws it in one call.
// Row lines are a line strip along each half-row, expanded to independent
// segments and broken at non-finite points; column lines connect the two vertices
// of each quad column. Vertex, colour and normal come interleaved from
// coloredPoint, like the face arrays.
inline void
drawSurfaceQuadStripEdgesArray(
    const std::vector<std::vector<coloredPoint>>& edges, meshStyle meshstyle)
{
    std::vector<coloredPoint> vertices;
    for (const auto& qlist : edges) {
        if (meshstyle == meshStyle::Both || meshstyle == meshStyle::Row) {
            for (size_t start = 0; start < 2; ++start) {
                const coloredPoint* previous = nullptr;
                for (size_t j = start; j < qlist.size(); j += 2) {
                    const coloredPoint& point = qlist[j];
                    if (!isFiniteSurfaceArrayPoint(point)) {
                        previous = nullptr;
                        continue;
                    }
                    if (previous != nullptr) {
                        vertices.push_back(*previous);
                        vertices.push_back(point);
                    }
                    previous = &point;
                }
            }
        }
        if (meshstyle == meshStyle::Both || meshstyle == meshStyle::Column) {
            for (size_t j = 0; j + 1 < qlist.size(); j += 2) {
                const coloredPoint& a = qlist[j];
                const coloredPoint& b = qlist[j + 1];
                if (isFiniteSurfaceArrayPoint(a) && isFiniteSurfaceArrayPoint(b)) {
                    vertices.push_back(a);
                    vertices.push_back(b);
                }
            }
        }
    }
    if (vertices.empty()) {
        return;
    }
    const char* base = reinterpret_cast<const char*>(vertices.data());
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
    glEnableClientState(GL_NORMAL_ARRAY);
    glVertexPointer(3, GL_DOUBLE, sizeof(coloredPoint), base + offsetof(coloredPoint, x));
    glColorPointer(4, GL_DOUBLE, sizeof(coloredPoint), base + offsetof(coloredPoint, r));
    glNormalPointer(GL_DOUBLE, sizeof(coloredPoint), base + offsetof(coloredPoint, nx));
    glDrawArrays(GL_LINES, 0, static_cast<GLsizei>(vertices.size()));
    glDisableClientState(GL_NORMAL_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);
}
//=============================================================================
inline void
drawSurfacePackedQuadsArray(const std::vector<coloredPoint>& packedQuads)
{
    if (packedQuads.empty()) {
        return;
    }
    std::vector<coloredPoint> vertices;
    vertices.reserve(packedQuads.size());
    for (size_t k = 0; k + 3 < packedQuads.size(); k += 4) {
        vertices.push_back(packedQuads[k]);
        vertices.push_back(packedQuads[k + 1]);
        vertices.push_back(packedQuads[k + 3]);
        vertices.push_back(packedQuads[k + 2]);
    }
    const char* base = reinterpret_cast<const char*>(vertices.data());
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
    glEnableClientState(GL_NORMAL_ARRAY);
    glVertexPointer(3, GL_DOUBLE, sizeof(coloredPoint), base + offsetof(coloredPoint, x));
    glColorPointer(4, GL_DOUBLE, sizeof(coloredPoint), base + offsetof(coloredPoint, r));
    glNormalPointer(GL_DOUBLE, sizeof(coloredPoint), base + offsetof(coloredPoint, nx));
    glDrawArrays(GL_QUADS, 0, static_cast<GLsizei>(vertices.size()));
    glDisableClientState(GL_NORMAL_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);
}
//=============================================================================
