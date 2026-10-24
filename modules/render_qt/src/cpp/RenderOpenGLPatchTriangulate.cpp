//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <array>
#include <cmath>
#include <cstddef>
#include <vector>
#include "RenderOpenGLPatchTriangulate.hpp"
//=============================================================================
namespace {
struct ProjectedPatchVertex
{
    double x;
    double y;
    std::size_t sourceIndex;
};
//=============================================================================
int
patchProjectionAxis(const point& normal)
{
    const double ax = std::fabs(normal.x);
    const double ay = std::fabs(normal.y);
    const double az = std::fabs(normal.z);
    if (ax >= ay && ax >= az) {
        return 0;
    }
    if (ay >= ax && ay >= az) {
        return 1;
    }
    return 2;
}
//=============================================================================
ProjectedPatchVertex
projectPatchVertex(const Face& face, std::size_t index, int axis)
{
    const point& vertex = face.vertices[index];
    if (axis == 0) {
        return { vertex.y, vertex.z, index };
    }
    if (axis == 1) {
        return { vertex.x, vertex.z, index };
    }
    return { vertex.x, vertex.y, index };
}
//=============================================================================
bool
sameProjectedPatchPoint(const ProjectedPatchVertex& a, const ProjectedPatchVertex& b)
{
    constexpr double tolerance = 1e-12;
    return std::fabs(a.x - b.x) <= tolerance && std::fabs(a.y - b.y) <= tolerance;
}
//=============================================================================
double
projectedPatchArea(const std::vector<ProjectedPatchVertex>& vertices)
{
    double area = 0;
    for (std::size_t i = 0; i < vertices.size(); ++i) {
        const ProjectedPatchVertex& a = vertices[i];
        const ProjectedPatchVertex& b = vertices[(i + 1) % vertices.size()];
        area += a.x * b.y - b.x * a.y;
    }
    return area * 0.5;
}
//=============================================================================
double
projectedPatchCross(
    const ProjectedPatchVertex& a, const ProjectedPatchVertex& b, const ProjectedPatchVertex& c)
{
    return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
}
//=============================================================================
bool
projectedPointInTriangle(const ProjectedPatchVertex& p, const ProjectedPatchVertex& a,
    const ProjectedPatchVertex& b, const ProjectedPatchVertex& c)
{
    constexpr double tolerance = 1e-12;
    const double c1 = projectedPatchCross(a, b, p);
    const double c2 = projectedPatchCross(b, c, p);
    const double c3 = projectedPatchCross(c, a, p);
    return (c1 >= -tolerance && c2 >= -tolerance && c3 >= -tolerance)
        || (c1 <= tolerance && c2 <= tolerance && c3 <= tolerance);
}
//=============================================================================
void
removeDuplicatePatchVertices(std::vector<ProjectedPatchVertex>& vertices)
{
    if (vertices.empty()) {
        return;
    }
    std::vector<ProjectedPatchVertex> filtered;
    filtered.reserve(vertices.size());
    for (const auto& vertex : vertices) {
        if (filtered.empty() || !sameProjectedPatchPoint(filtered.back(), vertex)) {
            filtered.push_back(vertex);
        }
    }
    if (filtered.size() > 1 && sameProjectedPatchPoint(filtered.front(), filtered.back())) {
        filtered.pop_back();
    }
    vertices.swap(filtered);
}
//=============================================================================
} // namespace
//=============================================================================
bool
triangulatePatchSubPath(const Face& face, const std::vector<std::size_t>& subPath,
    const point& fallbackNormal, std::vector<std::array<std::size_t, 3>>& triangles)
{
    if (subPath.size() < 3) {
        return false;
    }
    const int axis = patchProjectionAxis(fallbackNormal);
    std::vector<ProjectedPatchVertex> vertices;
    vertices.reserve(subPath.size());
    for (std::size_t index : subPath) {
        vertices.push_back(projectPatchVertex(face, index, axis));
    }
    removeDuplicatePatchVertices(vertices);
    if (vertices.size() < 3) {
        return false;
    }

    const double area = projectedPatchArea(vertices);
    if (std::fabs(area) < 1e-12) {
        return false;
    }
    const double orientation = area > 0 ? 1. : -1.;

    std::vector<std::size_t> polygon(vertices.size());
    for (std::size_t i = 0; i < vertices.size(); ++i) {
        polygon[i] = i;
    }

    std::size_t guard = 0;
    while (polygon.size() > 3 && guard < vertices.size() * vertices.size()) {
        bool clipped = false;
        for (std::size_t i = 0; i < polygon.size(); ++i) {
            const std::size_t previous = polygon[(i + polygon.size() - 1) % polygon.size()];
            const std::size_t current = polygon[i];
            const std::size_t next = polygon[(i + 1) % polygon.size()];

            if (orientation
                    * projectedPatchCross(vertices[previous], vertices[current], vertices[next])
                <= 1e-12) {
                continue;
            }

            bool containsVertex = false;
            for (std::size_t candidate : polygon) {
                if (candidate == previous || candidate == current || candidate == next) {
                    continue;
                }
                if (projectedPointInTriangle(vertices[candidate], vertices[previous],
                        vertices[current], vertices[next])) {
                    containsVertex = true;
                    break;
                }
            }
            if (containsVertex) {
                continue;
            }

            triangles.push_back({ { vertices[previous].sourceIndex, vertices[current].sourceIndex,
                vertices[next].sourceIndex } });
            polygon.erase(polygon.begin() + static_cast<std::ptrdiff_t>(i));
            clipped = true;
            break;
        }
        if (!clipped) {
            return false;
        }
        ++guard;
    }

    if (polygon.size() == 3) {
        triangles.push_back({ { vertices[polygon[0]].sourceIndex, vertices[polygon[1]].sourceIndex,
            vertices[polygon[2]].sourceIndex } });
        return true;
    }
    return false;
}
//=============================================================================
