//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__isosurface__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <limits>
#include <tuple>
#include <unordered_map>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
struct Point3
{
    double x;
    double y;
    double z;
};
//=============================================================================
struct SurfacePoint
{
    Point3 point;
    double color;
};
//=============================================================================
struct Triangle
{
    size_t a;
    size_t b;
    size_t c;
};
//=============================================================================
struct Mesh
{
    std::vector<Point3> vertices;
    std::vector<Triangle> faces;
    std::vector<double> colors;
    bool hasColors { false };
};
//=============================================================================
struct Grid
{
    std::vector<double> x;
    std::vector<double> y;
    std::vector<double> z;
    Dimensions dims;
    indexType rows { 0 };
    indexType columns { 0 };
    indexType planes { 0 };
};
//=============================================================================
struct VertexKey
{
    long long x;
    long long y;
    long long z;

    bool
    operator==(const VertexKey& other) const
    {
        return x == other.x && y == other.y && z == other.z;
    }
};
//=============================================================================
struct VertexKeyHash
{
    size_t
    operator()(const VertexKey& key) const
    {
        size_t h1 = std::hash<long long> {}(key.x);
        size_t h2 = std::hash<long long> {}(key.y);
        size_t h3 = std::hash<long long> {}(key.z);
        return h1 ^ (h2 + 0x9e3779b97f4a7c15ULL + (h1 << 6U) + (h1 >> 2U))
            ^ (h3 + 0x9e3779b97f4a7c15ULL + (h2 << 6U) + (h2 >> 2U));
    }
};
//=============================================================================
std::vector<double>
doubleVector(const ArrayOf& value)
{
    if (!value.isNumeric() || value.isComplex()) {
        Error(L"Nelson:graphics:realNumericExpected", L"Real numeric data expected.");
    }
    ArrayOf copy(value);
    copy.promoteType(NLS_DOUBLE);
    const double* data = static_cast<const double*>(copy.dataPointer());
    return std::vector<double>(data, data + copy.elementCount());
}
//=============================================================================
Grid
parseGrid(const ArrayOfVector& argIn)
{
    Grid grid;
    grid.x = doubleVector(argIn[0]);
    grid.y = doubleVector(argIn[1]);
    grid.z = doubleVector(argIn[2]);
    grid.dims = argIn[3].dimensions();
    grid.rows = grid.dims.dimensionLength(0);
    grid.columns = grid.dims.dimensionLength(1);
    grid.planes = grid.dims.dimensionLength(2);
    if (static_cast<indexType>(grid.x.size()) != grid.columns
        || static_cast<indexType>(grid.y.size()) != grid.rows
        || static_cast<indexType>(grid.z.size()) != grid.planes) {
        Error(L"Nelson:isosurface:gridMismatch", L"Grid vectors must match volume dimensions.");
    }
    if (grid.rows < 2 || grid.columns < 2 || grid.planes < 2) {
        Error(L"Nelson:isosurface:invalidVolumeSize",
            L"Volume data must have at least two points in each dimension.");
    }
    return grid;
}
//=============================================================================
std::vector<double>
volumeData(const ArrayOf& value, const Grid& grid)
{
    if (!value.isNumeric() || value.isComplex() || !value.dimensions().equals(grid.dims)) {
        Error(L"Nelson:isosurface:invalidVolumeType",
            L"Volume data must be a real numeric 3-D array.");
    }
    return doubleVector(value);
}
//=============================================================================
std::vector<double>
colorData(const ArrayOf& value, const Grid& grid, bool& hasColors)
{
    hasColors = !value.isEmpty();
    if (!hasColors) {
        return {};
    }
    if (!value.isNumeric() || value.isComplex() || !value.dimensions().equals(grid.dims)) {
        Error(L"Nelson:isosurface:colorMismatch",
            L"Color data must have the same size as the volume data.");
    }
    return doubleVector(value);
}
//=============================================================================
indexType
valueIndex(const Grid& grid, indexType row, indexType col, indexType dep)
{
    return row + col * grid.rows + dep * grid.rows * grid.columns;
}
//=============================================================================
Point3
gridPoint(const Grid& grid, indexType row, indexType col, indexType dep)
{
    return { grid.x[static_cast<size_t>(col)], grid.y[static_cast<size_t>(row)],
        grid.z[static_cast<size_t>(dep)] };
}
//=============================================================================
bool
samePoint(const SurfacePoint& first, const SurfacePoint& second)
{
    double scale
        = std::max({ 1., std::abs(first.point.x), std::abs(first.point.y), std::abs(first.point.z),
            std::abs(second.point.x), std::abs(second.point.y), std::abs(second.point.z) });
    double tol = scale * std::numeric_limits<double>::epsilon() * 64.;
    return std::abs(first.point.x - second.point.x) <= tol
        && std::abs(first.point.y - second.point.y) <= tol
        && std::abs(first.point.z - second.point.z) <= tol;
}
//=============================================================================
void
appendUniqueSurfacePoint(std::vector<SurfacePoint>& points, const SurfacePoint& point)
{
    for (const SurfacePoint& current : points) {
        if (samePoint(current, point)) {
            return;
        }
    }
    points.push_back(point);
}
//=============================================================================
SurfacePoint
interpolateSurfacePoint(const SurfacePoint& first, const SurfacePoint& second, double firstValue,
    double secondValue, double isovalue, bool hasColors)
{
    double denominator = secondValue - firstValue;
    double t = denominator == 0. ? 0. : (isovalue - firstValue) / denominator;
    t = std::max(0., std::min(1., t));
    double color = std::numeric_limits<double>::quiet_NaN();
    if (hasColors) {
        color = first.color + t * (second.color - first.color);
    }
    return { { first.point.x + t * (second.point.x - first.point.x),
                 first.point.y + t * (second.point.y - first.point.y),
                 first.point.z + t * (second.point.z - first.point.z) },
        color };
}
//=============================================================================
void
appendEdgeIntersection(const SurfacePoint& first, const SurfacePoint& second, double firstValue,
    double secondValue, double isovalue, bool hasColors, std::vector<SurfacePoint>& points)
{
    double firstDelta = firstValue - isovalue;
    double secondDelta = secondValue - isovalue;
    double scale
        = std::max({ 1., std::abs(firstValue), std::abs(secondValue), std::abs(isovalue) });
    double tol = scale * std::numeric_limits<double>::epsilon() * 64.;
    bool firstOn = std::abs(firstDelta) <= tol;
    bool secondOn = std::abs(secondDelta) <= tol;
    if (firstOn && secondOn) {
        appendUniqueSurfacePoint(points, first);
        appendUniqueSurfacePoint(points, second);
    } else if (firstOn) {
        appendUniqueSurfacePoint(points, first);
    } else if (secondOn) {
        appendUniqueSurfacePoint(points, second);
    } else if ((firstDelta < 0. && secondDelta > 0.) || (firstDelta > 0. && secondDelta < 0.)) {
        appendUniqueSurfacePoint(points,
            interpolateSurfacePoint(first, second, firstValue, secondValue, isovalue, hasColors));
    }
}
//=============================================================================
Point3
surfacePointNormal(const std::vector<SurfacePoint>& points)
{
    for (size_t i = 0; i < points.size(); ++i) {
        for (size_t j = i + 1; j < points.size(); ++j) {
            for (size_t k = j + 1; k < points.size(); ++k) {
                const Point3& a = points[i].point;
                const Point3& b = points[j].point;
                const Point3& c = points[k].point;
                double ux = b.x - a.x, uy = b.y - a.y, uz = b.z - a.z;
                double vx = c.x - a.x, vy = c.y - a.y, vz = c.z - a.z;
                Point3 n { uy * vz - uz * vy, uz * vx - ux * vz, ux * vy - uy * vx };
                if (n.x * n.x + n.y * n.y + n.z * n.z
                    > std::numeric_limits<double>::epsilon() * 64.) {
                    return n;
                }
            }
        }
    }
    return { 0., 0., 0. };
}
//=============================================================================
std::vector<SurfacePoint>
orderedPolygon(std::vector<SurfacePoint> points)
{
    if (points.size() <= 3) {
        return points;
    }
    Point3 center { 0., 0., 0. };
    for (const SurfacePoint& point : points) {
        center.x += point.point.x;
        center.y += point.point.y;
        center.z += point.point.z;
    }
    center.x /= static_cast<double>(points.size());
    center.y /= static_cast<double>(points.size());
    center.z /= static_cast<double>(points.size());
    Point3 normal = surfacePointNormal(points);
    int droppedAxis = 0;
    if (std::abs(normal.y) >= std::abs(normal.x) && std::abs(normal.y) >= std::abs(normal.z)) {
        droppedAxis = 1;
    } else if (std::abs(normal.z) >= std::abs(normal.x)
        && std::abs(normal.z) >= std::abs(normal.y)) {
        droppedAxis = 2;
    }
    std::sort(points.begin(), points.end(),
        [center, droppedAxis](const SurfacePoint& a, const SurfacePoint& b) {
            double ax = droppedAxis == 0 ? a.point.y - center.y : a.point.x - center.x;
            double ay = droppedAxis == 2 ? a.point.y - center.y : a.point.z - center.z;
            double bx = droppedAxis == 0 ? b.point.y - center.y : b.point.x - center.x;
            double by = droppedAxis == 2 ? b.point.y - center.y : b.point.z - center.z;
            return std::atan2(ay, ax) < std::atan2(by, bx);
        });
    return points;
}
//=============================================================================
VertexKey
vertexKey(const Point3& point)
{
    constexpr double scale = 1e12;
    return { static_cast<long long>(std::llround(point.x * scale)),
        static_cast<long long>(std::llround(point.y * scale)),
        static_cast<long long>(std::llround(point.z * scale)) };
}
//=============================================================================
size_t
appendVertex(Mesh& mesh, const SurfacePoint& point, bool noshare,
    std::unordered_map<VertexKey, size_t, VertexKeyHash>& shared)
{
    if (!noshare) {
        VertexKey key = vertexKey(point.point);
        auto found = shared.find(key);
        if (found != shared.end()) {
            return found->second;
        }
        size_t index = mesh.vertices.size() + 1;
        shared[key] = index;
        mesh.vertices.push_back(point.point);
        if (mesh.hasColors) {
            mesh.colors.push_back(point.color);
        }
        return index;
    }
    mesh.vertices.push_back(point.point);
    if (mesh.hasColors) {
        mesh.colors.push_back(point.color);
    }
    return mesh.vertices.size();
}
//=============================================================================
double
triangleAreaScale(const Point3& a, const Point3& b, const Point3& c)
{
    double ux = b.x - a.x, uy = b.y - a.y, uz = b.z - a.z;
    double vx = c.x - a.x, vy = c.y - a.y, vz = c.z - a.z;
    double cx = uy * vz - uz * vy;
    double cy = uz * vx - ux * vz;
    double cz = ux * vy - uy * vx;
    return std::sqrt(cx * cx + cy * cy + cz * cz);
}
//=============================================================================
void
appendTriangle(Mesh& mesh, const SurfacePoint& a, const SurfacePoint& b, const SurfacePoint& c,
    bool noshare, std::unordered_map<VertexKey, size_t, VertexKeyHash>& shared)
{
    if (triangleAreaScale(a.point, b.point, c.point)
        <= std::numeric_limits<double>::epsilon() * 64.) {
        return;
    }
    size_t ia = appendVertex(mesh, a, noshare, shared);
    size_t ib = appendVertex(mesh, b, noshare, shared);
    size_t ic = appendVertex(mesh, c, noshare, shared);
    if (ia != ib && ia != ic && ib != ic) {
        mesh.faces.push_back({ ia, ib, ic });
    }
}
//=============================================================================
void
reverseIfOppositeGradient(
    std::vector<SurfacePoint>& points, const Point3& gradient, const Point3& normal)
{
    double dot = normal.x * gradient.x + normal.y * gradient.y + normal.z * gradient.z;
    if (dot < 0.) {
        std::reverse(points.begin(), points.end());
    }
}
//=============================================================================
Point3
polygonNormal(const std::vector<SurfacePoint>& points)
{
    return surfacePointNormal(points);
}
//=============================================================================
Point3
cubeGradient(const std::array<SurfacePoint, 8>& points, const std::array<double, 8>& values)
{
    double dx = points[1].point.x - points[0].point.x;
    double dy = points[3].point.y - points[0].point.y;
    double dz = points[4].point.z - points[0].point.z;
    double lowX = (values[0] + values[3] + values[4] + values[7]) * 0.25;
    double highX = (values[1] + values[2] + values[5] + values[6]) * 0.25;
    double lowY = (values[0] + values[1] + values[4] + values[5]) * 0.25;
    double highY = (values[2] + values[3] + values[6] + values[7]) * 0.25;
    double lowZ = (values[0] + values[1] + values[2] + values[3]) * 0.25;
    double highZ = (values[4] + values[5] + values[6] + values[7]) * 0.25;
    return { dx == 0. ? 0. : (highX - lowX) / dx, dy == 0. ? 0. : (highY - lowY) / dy,
        dz == 0. ? 0. : (highZ - lowZ) / dz };
}
//=============================================================================
void
appendCubePolygon(const std::array<SurfacePoint, 8>& points, const std::array<double, 8>& values,
    double isovalue, bool noshare, Mesh& mesh,
    std::unordered_map<VertexKey, size_t, VertexKeyHash>& shared, const Point3& gradient)
{
    if (std::any_of(
            values.begin(), values.end(), [](double value) { return !std::isfinite(value); })) {
        return;
    }
    constexpr std::array<std::array<int, 2>, 12> edges = { { { 0, 1 }, { 1, 2 }, { 2, 3 }, { 3, 0 },
        { 4, 5 }, { 5, 6 }, { 6, 7 }, { 7, 4 }, { 0, 4 }, { 1, 5 }, { 2, 6 }, { 3, 7 } } };
    std::vector<SurfacePoint> intersections;
    intersections.reserve(6);
    for (const auto& edge : edges) {
        appendEdgeIntersection(points[edge[0]], points[edge[1]], values[edge[0]], values[edge[1]],
            isovalue, mesh.hasColors, intersections);
    }
    if (intersections.size() < 3) {
        return;
    }
    intersections = orderedPolygon(intersections);
    Point3 normal = polygonNormal(intersections);
    reverseIfOppositeGradient(intersections, gradient, normal);
    for (size_t k = 1; k + 1 < intersections.size(); ++k) {
        appendTriangle(
            mesh, intersections[0], intersections[k], intersections[k + 1], noshare, shared);
    }
}
//=============================================================================
void
appendCube(const Grid& grid, const std::vector<double>& values, const std::vector<double>& colors,
    indexType row, indexType col, indexType dep, double isovalue, bool noshare, Mesh& mesh,
    std::unordered_map<VertexKey, size_t, VertexKeyHash>& shared)
{
    constexpr std::array<std::array<int, 3>, 8> offsets = { { { 0, 0, 0 }, { 0, 1, 0 }, { 1, 1, 0 },
        { 1, 0, 0 }, { 0, 0, 1 }, { 0, 1, 1 }, { 1, 1, 1 }, { 1, 0, 1 } } };
    std::array<SurfacePoint, 8> cubePoints;
    std::array<double, 8> cubeValues;
    for (size_t k = 0; k < offsets.size(); ++k) {
        indexType rr = row + offsets[k][0];
        indexType cc = col + offsets[k][1];
        indexType dd = dep + offsets[k][2];
        indexType idx = valueIndex(grid, rr, cc, dd);
        double color = mesh.hasColors ? colors[static_cast<size_t>(idx)] : 0.;
        cubePoints[k] = { gridPoint(grid, rr, cc, dd), color };
        cubeValues[k] = values[static_cast<size_t>(idx)];
    }
    appendCubePolygon(cubePoints, cubeValues, isovalue, noshare, mesh, shared,
        cubeGradient(cubePoints, cubeValues));
}
//=============================================================================
Mesh
extractSurface(const Grid& grid, const std::vector<double>& values,
    const std::vector<double>& colors, double isovalue, bool hasColors, bool noshare)
{
    Mesh mesh;
    mesh.hasColors = hasColors;
    mesh.vertices.reserve(static_cast<size_t>(grid.rows * grid.columns * grid.planes));
    mesh.faces.reserve(static_cast<size_t>(grid.rows * grid.columns * grid.planes));
    if (hasColors) {
        mesh.colors.reserve(mesh.vertices.capacity());
    }
    std::unordered_map<VertexKey, size_t, VertexKeyHash> shared;
    for (indexType dep = 0; dep < grid.planes - 1; ++dep) {
        for (indexType col = 0; col < grid.columns - 1; ++col) {
            for (indexType row = 0; row < grid.rows - 1; ++row) {
                appendCube(grid, values, colors, row, col, dep, isovalue, noshare, mesh, shared);
            }
        }
    }
    return mesh;
}
//=============================================================================
ArrayOf
facesArray(const Mesh& mesh)
{
    ArrayOf out = ArrayOf::doubleMatrix2d(static_cast<indexType>(mesh.faces.size()), 3);
    double* data = static_cast<double*>(out.readWriteDataPointer());
    for (indexType row = 0; row < static_cast<indexType>(mesh.faces.size()); ++row) {
        data[row] = static_cast<double>(mesh.faces[static_cast<size_t>(row)].a);
        data[row + mesh.faces.size()] = static_cast<double>(mesh.faces[static_cast<size_t>(row)].b);
        data[row + 2 * mesh.faces.size()]
            = static_cast<double>(mesh.faces[static_cast<size_t>(row)].c);
    }
    return out;
}
//=============================================================================
ArrayOf
verticesArray(const Mesh& mesh)
{
    ArrayOf out = ArrayOf::doubleMatrix2d(static_cast<indexType>(mesh.vertices.size()), 3);
    double* data = static_cast<double*>(out.readWriteDataPointer());
    for (indexType row = 0; row < static_cast<indexType>(mesh.vertices.size()); ++row) {
        const Point3& point = mesh.vertices[static_cast<size_t>(row)];
        data[row] = point.x;
        data[row + mesh.vertices.size()] = point.y;
        data[row + 2 * mesh.vertices.size()] = point.z;
    }
    return out;
}
//=============================================================================
ArrayOf
colorsArray(const Mesh& mesh)
{
    if (!mesh.hasColors) {
        return ArrayOf::empty();
    }
    ArrayOf out = ArrayOf::doubleMatrix2d(static_cast<indexType>(mesh.colors.size()), 1);
    double* data = static_cast<double*>(out.readWriteDataPointer());
    std::copy(mesh.colors.begin(), mesh.colors.end(), data);
    return out;
}
//=============================================================================
Point3
faceNormal(const Point3& a, const Point3& b, const Point3& c)
{
    double ux = b.x - a.x, uy = b.y - a.y, uz = b.z - a.z;
    double vx = c.x - a.x, vy = c.y - a.y, vz = c.z - a.z;
    return { uy * vz - uz * vy, uz * vx - ux * vz, ux * vy - uy * vx };
}
//=============================================================================
ArrayOf
normalsArray(const Mesh& mesh)
{
    std::vector<Point3> normals(mesh.vertices.size(), { 0., 0., 0. });
    for (const Triangle& face : mesh.faces) {
        size_t ia = face.a - 1, ib = face.b - 1, ic = face.c - 1;
        Point3 n = faceNormal(mesh.vertices[ia], mesh.vertices[ib], mesh.vertices[ic]);
        for (size_t index : { ia, ib, ic }) {
            normals[index].x += n.x;
            normals[index].y += n.y;
            normals[index].z += n.z;
        }
    }
    ArrayOf out = ArrayOf::doubleMatrix2d(static_cast<indexType>(normals.size()), 3);
    double* data = static_cast<double*>(out.readWriteDataPointer());
    for (indexType row = 0; row < static_cast<indexType>(normals.size()); ++row) {
        Point3 n = normals[static_cast<size_t>(row)];
        double len = std::sqrt(n.x * n.x + n.y * n.y + n.z * n.z);
        if (len > 0.) {
            n.x /= len;
            n.y /= len;
            n.z /= len;
        }
        data[row] = n.x;
        data[row + normals.size()] = n.y;
        data[row + 2 * normals.size()] = n.z;
    }
    return out;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::__isosurface__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 4);
    nargincheck(argIn, 7, 7);
    Grid grid = parseGrid(argIn);
    std::vector<double> values = volumeData(argIn[3], grid);
    double isovalue = argIn[4].asDoubleScalar();
    bool hasColors = false;
    std::vector<double> colors = colorData(argIn[5], grid, hasColors);
    bool noshare = argIn[6].asLogicalScalar();
    Mesh mesh = extractSurface(grid, values, colors, isovalue, hasColors, noshare);
    ArrayOfVector retval;
    retval << facesArray(mesh) << verticesArray(mesh) << colorsArray(mesh) << normalsArray(mesh);
    return retval;
}
//=============================================================================
