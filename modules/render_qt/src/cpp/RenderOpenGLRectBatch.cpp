//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if defined(__APPLE__) && !defined(GL_SILENCE_DEPRECATION)
#define GL_SILENCE_DEPRECATION
#endif
#if defined(_WIN32) && !defined(NOMINMAX)
#define NOMINMAX
#endif
#include <algorithm>
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
void
addVertex(std::vector<GLdouble>& out, double x, double y)
{
    out.push_back(x);
    out.push_back(y);
    out.push_back(0.0);
}
//=============================================================================
// Vertices of one rectangle: the four corners quad() receives for a bar, then
// the four outline segments quadline() draws around it.
constexpr GLsizei quadVertexCount = 4;
constexpr GLsizei outlineVertexCount = 8;
constexpr GLsizei rectVertexCount = quadVertexCount + outlineVertexCount;
//=============================================================================
void
addRect(std::vector<GLdouble>& out, double x1, double y1, double x2, double y2)
{
    addVertex(out, x1, y1);
    addVertex(out, x1, y2);
    addVertex(out, x2, y2);
    addVertex(out, x2, y1);
    addVertex(out, x1, y1);
    addVertex(out, x1, y2);
    addVertex(out, x1, y2);
    addVertex(out, x2, y2);
    addVertex(out, x2, y2);
    addVertex(out, x2, y1);
    addVertex(out, x2, y1);
    addVertex(out, x1, y1);
}
//=============================================================================
} // namespace
//=============================================================================
bool
RenderOpenGL::rectBatch(const std::vector<double>& corners, const std::vector<double>& faceColor,
    const std::vector<double>& edgeColor, double)
{
    // A whole bar or histogram series is drawn from one vertex array. The corners
    // are data-space, drawn through the current matrices like quad() and quadline().
    // Each bar is filled then outlined before the next one, so adjacent bars keep
    // the stacking of outlines over fills.
    const size_t n = corners.size() / 4;
    if (n == 0 || corners.size() != n * 4) {
        return false;
    }
    const bool hasFace = faceColor.size() >= 3 && faceColor[0] != -1;
    const bool hasEdge = edgeColor.size() >= 3 && edgeColor[0] != -1;
    if (!hasFace && !hasEdge) {
        return false;
    }
    // Scratch array reused between batches. It is kept out of the class because
    // figure widgets of other modules construct it and depend on its layout.
    // Drawing happens on the one GUI thread.
    static std::vector<GLdouble> vertices;
    vertices.clear();
    vertices.reserve(n * rectVertexCount * 3);
    for (size_t i = 0; i < n; i++) {
        addRect(
            vertices, corners[i * 4], corners[i * 4 + 1], corners[i * 4 + 2], corners[i * 4 + 3]);
    }
    // color() also sets the blend state a translucent colour needs.
    GLdouble face[4] = { 0, 0, 0, 1 };
    GLdouble edge[4] = { 0, 0, 0, 1 };
    if (hasFace) {
        color(faceColor);
        std::copy(currentColor, currentColor + 4, face);
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    }
    if (hasEdge) {
        color(edgeColor);
        std::copy(currentColor, currentColor + 4, edge);
    }
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(3, GL_DOUBLE, 0, vertices.data());
    for (size_t i = 0; i < n; i++) {
        const GLint first = static_cast<GLint>(i) * rectVertexCount;
        if (hasFace) {
            glColor4dv(face);
            glDrawArrays(GL_QUADS, first, quadVertexCount);
        }
        if (hasEdge) {
            glColor4dv(edge);
            glDrawArrays(GL_LINES, first + quadVertexCount, outlineVertexCount);
        }
    }
    glDisableClientState(GL_VERTEX_ARRAY);
    glColor4dv(currentColor);
    return true;
}
//=============================================================================
