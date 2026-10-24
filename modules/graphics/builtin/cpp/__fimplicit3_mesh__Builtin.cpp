//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__fimplicit3_mesh__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "FunctionDef.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <limits>
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
struct Triangle
{
    size_t a;
    size_t b;
    size_t c;
};
//=============================================================================
struct EdgeData
{
    std::vector<double> x;
    std::vector<double> y;
    std::vector<double> z;
};
//=============================================================================
double
levelTolerance(const std::array<double, 4>& values)
{
    double scale = 1.;
    for (double value : values) {
        if (std::isfinite(value)) {
            scale = std::max(scale, std::abs(value));
        }
    }
    return scale * std::numeric_limits<double>::epsilon() * 64.;
}
//=============================================================================
int
levelSign(double value, double tolerance)
{
    if (std::abs(value) <= tolerance) {
        return 0;
    }
    return value < 0. ? -1 : 1;
}
//=============================================================================
bool
samePoint(const Point3& first, const Point3& second)
{
    double scale = std::max({ 1., std::abs(first.x), std::abs(first.y), std::abs(first.z),
        std::abs(second.x), std::abs(second.y), std::abs(second.z) });
    double tolerance = scale * std::numeric_limits<double>::epsilon() * 64.;
    return std::abs(first.x - second.x) <= tolerance && std::abs(first.y - second.y) <= tolerance
        && std::abs(first.z - second.z) <= tolerance;
}
//=============================================================================
void
appendUniquePoint(std::vector<Point3>& points, const Point3& point)
{
    for (const auto& current : points) {
        if (samePoint(current, point)) {
            return;
        }
    }
    points.push_back(point);
}
//=============================================================================
Point3
interpolateLevel(const Point3& first, const Point3& second, double firstValue, double secondValue)
{
    double denominator = firstValue - secondValue;
    if (denominator == 0.) {
        return first;
    }
    double t = firstValue / denominator;
    t = std::max(0., std::min(1., t));
    return { first.x + t * (second.x - first.x), first.y + t * (second.y - first.y),
        first.z + t * (second.z - first.z) };
}
//=============================================================================
void
appendEdgeIntersection(const Point3& first, const Point3& second, double firstValue,
    double secondValue, double tolerance, std::vector<Point3>& intersections)
{
    int firstSign = levelSign(firstValue, tolerance);
    int secondSign = levelSign(secondValue, tolerance);
    if (firstSign == 0 && secondSign == 0) {
        appendUniquePoint(intersections, first);
        appendUniquePoint(intersections, second);
    } else if (firstSign == 0) {
        appendUniquePoint(intersections, first);
    } else if (secondSign == 0) {
        appendUniquePoint(intersections, second);
    } else if (firstSign != secondSign) {
        appendUniquePoint(intersections, interpolateLevel(first, second, firstValue, secondValue));
    }
}
//=============================================================================
double
triangleCrossNorm(const Point3& first, const Point3& second, const Point3& third)
{
    double ux = second.x - first.x;
    double uy = second.y - first.y;
    double uz = second.z - first.z;
    double vx = third.x - first.x;
    double vy = third.y - first.y;
    double vz = third.z - first.z;
    double cx = uy * vz - uz * vy;
    double cy = uz * vx - ux * vz;
    double cz = ux * vy - uy * vx;
    return std::sqrt(cx * cx + cy * cy + cz * cz);
}
//=============================================================================
bool
isDegenerateTriangle(const Point3& first, const Point3& second, const Point3& third)
{
    double edge12 = (second.x - first.x) * (second.x - first.x)
        + (second.y - first.y) * (second.y - first.y) + (second.z - first.z) * (second.z - first.z);
    double edge23 = (third.x - second.x) * (third.x - second.x)
        + (third.y - second.y) * (third.y - second.y) + (third.z - second.z) * (third.z - second.z);
    double edge31 = (first.x - third.x) * (first.x - third.x)
        + (first.y - third.y) * (first.y - third.y) + (first.z - third.z) * (first.z - third.z);
    double scale = std::max({ edge12, edge23, edge31, 1. });
    return triangleCrossNorm(first, second, third)
        <= scale * std::numeric_limits<double>::epsilon() * 64.;
}
//=============================================================================
void
appendTriangle(const Point3& first, const Point3& second, const Point3& third,
    std::vector<Point3>& vertices, std::vector<Triangle>& faces)
{
    if (isDegenerateTriangle(first, second, third)) {
        return;
    }
    size_t base = vertices.size() + 1;
    vertices.insert(vertices.end(), { first, second, third });
    faces.push_back({ base, base + 1, base + 2 });
}
//=============================================================================
std::vector<Point3>
orderedPolygon(std::vector<Point3> points)
{
    if (points.size() <= 3) {
        return points;
    }
    Point3 center { 0., 0., 0. };
    for (const auto& point : points) {
        center.x += point.x;
        center.y += point.y;
        center.z += point.z;
    }
    center.x /= static_cast<double>(points.size());
    center.y /= static_cast<double>(points.size());
    center.z /= static_cast<double>(points.size());
    double nx = 0.;
    double ny = 0.;
    double nz = 0.;
    for (size_t k = 0; k < points.size(); ++k) {
        const Point3& current = points[k];
        const Point3& next = points[(k + 1) % points.size()];
        nx += (current.y - center.y) * (next.z - center.z)
            - (current.z - center.z) * (next.y - center.y);
        ny += (current.z - center.z) * (next.x - center.x)
            - (current.x - center.x) * (next.z - center.z);
        nz += (current.x - center.x) * (next.y - center.y)
            - (current.y - center.y) * (next.x - center.x);
    }
    int droppedAxis = 0;
    if (std::abs(ny) >= std::abs(nx) && std::abs(ny) >= std::abs(nz)) {
        droppedAxis = 1;
    } else if (std::abs(nz) >= std::abs(nx) && std::abs(nz) >= std::abs(ny)) {
        droppedAxis = 2;
    }
    std::sort(
        points.begin(), points.end(), [center, droppedAxis](const Point3& a, const Point3& b) {
            double ax = droppedAxis == 0 ? a.y - center.y : a.x - center.x;
            double ay = droppedAxis == 2 ? a.y - center.y : a.z - center.z;
            double bx = droppedAxis == 0 ? b.y - center.y : b.x - center.x;
            double by = droppedAxis == 2 ? b.y - center.y : b.z - center.z;
            return std::atan2(ay, ax) < std::atan2(by, bx);
        });
    return points;
}
//=============================================================================
FunctionDef*
functionDefinition(const ArrayOf& value)
{
    if (!value.isFunctionHandle()) {
        Error(
            L"Nelson:graphics:functionHandleExpected", L"%s must be a function handle.", L"Value");
    }
    function_handle handle = value.asFunctionHandle();
    if (handle.anonymousHandle == nullptr) {
        Error(
            L"Nelson:graphics:functionHandleExpected", L"%s must be a function handle.", L"Value");
    }
    return reinterpret_cast<FunctionDef*>(handle.anonymousHandle);
}
//=============================================================================
double
gridValue(double first, double last, int count, int index)
{
    return first + (last - first) * static_cast<double>(index) / static_cast<double>(count - 1);
}
//=============================================================================
ArrayOf
gridArray(const std::vector<double>& range, int density, int coordinate)
{
    const indexType n = static_cast<indexType>(density);
    double* data = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, n * n * n, stringVector(), false));
    for (indexType dep = 0; dep < n; ++dep) {
        for (indexType col = 0; col < n; ++col) {
            for (indexType row = 0; row < n; ++row) {
                indexType index = row + col * n + dep * n * n;
                if (coordinate == 0) {
                    data[index] = gridValue(range[0], range[1], density, static_cast<int>(col));
                } else if (coordinate == 1) {
                    data[index] = gridValue(range[2], range[3], density, static_cast<int>(row));
                } else {
                    data[index] = gridValue(range[4], range[5], density, static_cast<int>(dep));
                }
            }
        }
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(std::vector<indexType> { n, n, n }), data);
}
//=============================================================================
ArrayOf
evaluateVectorized(
    Evaluator* eval, FunctionDef* funcDef, const std::vector<double>& range, int density)
{
    ArrayOfVector inputs;
    inputs << gridArray(range, density, 0) << gridArray(range, density, 1)
           << gridArray(range, density, 2);
    ArrayOfVector outputs = funcDef->evaluateFunction(eval, inputs, 1);
    if (outputs.size() != 1 || !outputs[0].isNumeric() || outputs[0].isComplex()) {
        Error(
            L"Nelson:graphics:invalidFunctionOutput", L"Function must return real numeric values.");
    }
    ArrayOf values(outputs[0]);
    if (values.isScalar()) {
        double scalar = values.asDoubleScalar();
        ArrayOf expanded
            = ArrayOf::doubleMatrix2d(static_cast<indexType>(density * density * density), 1);
        double* data = static_cast<double*>(expanded.readWriteDataPointer());
        std::fill(data, data + density * density * density, scalar);
        return expanded;
    }
    if (values.elementCount() != static_cast<indexType>(density * density * density)) {
        Error(L"Nelson:fimplicit3:invalidFunctionOutputSize",
            L"Function must return real numeric values with the sample grid size.");
    }
    values.promoteType(NLS_DOUBLE);
    return values;
}
//=============================================================================
ArrayOf
evaluateScalar(Evaluator* eval, FunctionDef* funcDef, const std::vector<double>& range, int density)
{
    const indexType total = static_cast<indexType>(density * density * density);
    ArrayOf values = ArrayOf::doubleMatrix2d(total, 1);
    double* data = static_cast<double*>(values.readWriteDataPointer());
    for (int dep = 0; dep < density; ++dep) {
        double z = gridValue(range[4], range[5], density, dep);
        for (int col = 0; col < density; ++col) {
            double x = gridValue(range[0], range[1], density, col);
            for (int row = 0; row < density; ++row) {
                double y = gridValue(range[2], range[3], density, row);
                ArrayOfVector inputs;
                inputs << ArrayOf::doubleScalar(x) << ArrayOf::doubleScalar(y)
                       << ArrayOf::doubleScalar(z);
                ArrayOfVector outputs = funcDef->evaluateFunction(eval, inputs, 1);
                if (outputs.size() != 1 || !outputs[0].isNumeric() || outputs[0].isComplex()
                    || !outputs[0].isScalar()) {
                    Error(L"Nelson:graphics:invalidFunctionOutput",
                        L"Function must return real numeric values.");
                }
                data[row + col * density + dep * density * density] = outputs[0].asDoubleScalar();
            }
        }
    }
    return values;
}
//=============================================================================
ArrayOf
evaluateValues(Evaluator* eval, FunctionDef* funcDef, const std::vector<double>& range, int density)
{
    try {
        return evaluateVectorized(eval, funcDef, range, density);
    } catch (const NelsonException&) {
        return evaluateScalar(eval, funcDef, range, density);
    }
}
//=============================================================================
void
appendTetra(const std::array<Point3, 4>& points, const std::array<double, 4>& values,
    std::vector<Point3>& vertices, std::vector<Triangle>& faces)
{
    if (std::any_of(
            values.begin(), values.end(), [](double value) { return !std::isfinite(value); })) {
        return;
    }
    const std::array<std::array<int, 2>, 6> edges
        = { { { 0, 1 }, { 0, 2 }, { 0, 3 }, { 1, 2 }, { 1, 3 }, { 2, 3 } } };
    std::vector<Point3> intersections;
    intersections.reserve(4);
    double tolerance = levelTolerance(values);
    for (const auto& edge : edges) {
        appendEdgeIntersection(points[edge[0]], points[edge[1]], values[edge[0]], values[edge[1]],
            tolerance, intersections);
    }
    if (intersections.size() < 3) {
        return;
    }
    intersections = orderedPolygon(intersections);
    for (size_t k = 1; k + 1 < intersections.size(); ++k) {
        appendTriangle(intersections[0], intersections[k], intersections[k + 1], vertices, faces);
    }
}
//=============================================================================
void
appendCube(int row, int col, int dep, const double* values, int density,
    const std::vector<double>& range, std::vector<Point3>& vertices, std::vector<Triangle>& faces)
{
    const std::array<std::array<int, 3>, 8> offsets = { { { 0, 0, 0 }, { 0, 1, 0 }, { 1, 1, 0 },
        { 1, 0, 0 }, { 0, 0, 1 }, { 0, 1, 1 }, { 1, 1, 1 }, { 1, 0, 1 } } };
    const std::array<std::array<int, 4>, 6> tetrahedra = { { { 0, 1, 2, 6 }, { 0, 2, 3, 6 },
        { 0, 3, 7, 6 }, { 0, 7, 4, 6 }, { 0, 4, 5, 6 }, { 0, 5, 1, 6 } } };
    std::array<Point3, 8> cubePoints;
    std::array<double, 8> cubeValues;
    for (size_t k = 0; k < offsets.size(); ++k) {
        int rr = row + offsets[k][0];
        int cc = col + offsets[k][1];
        int dd = dep + offsets[k][2];
        cubePoints[k] = { gridValue(range[0], range[1], density, cc),
            gridValue(range[2], range[3], density, rr),
            gridValue(range[4], range[5], density, dd) };
        cubeValues[k] = values[rr + cc * density + dd * density * density];
    }
    for (const auto& tetra : tetrahedra) {
        appendTetra({ cubePoints[tetra[0]], cubePoints[tetra[1]], cubePoints[tetra[2]],
                        cubePoints[tetra[3]] },
            { cubeValues[tetra[0]], cubeValues[tetra[1]], cubeValues[tetra[2]],
                cubeValues[tetra[3]] },
            vertices, faces);
    }
}
//=============================================================================
bool
crossesZero(double first, double second)
{
    return std::isfinite(first) && std::isfinite(second)
        && ((first <= 0 && second > 0) || (first > 0 && second <= 0));
}
//=============================================================================
Point3
interpolateZero(const Point3& first, const Point3& second, double firstValue, double secondValue)
{
    double t = std::abs(firstValue) / (std::abs(firstValue) + std::abs(secondValue));
    return { first.x + t * (second.x - first.x), first.y + t * (second.y - first.y),
        first.z + t * (second.z - first.z) };
}
//=============================================================================
void
appendEdgeSegment(EdgeData& edges, const Point3& first, const Point3& second)
{
    double nanValue = std::numeric_limits<double>::quiet_NaN();
    edges.x.insert(edges.x.end(), { first.x, second.x, nanValue });
    edges.y.insert(edges.y.end(), { first.y, second.y, nanValue });
    edges.z.insert(edges.z.end(), { first.z, second.z, nanValue });
}
//=============================================================================
void
appendContourCell(
    const std::array<Point3, 4>& points, const std::array<double, 4>& values, EdgeData& edges)
{
    const std::array<std::array<int, 2>, 4> cellEdges
        = { { { 0, 1 }, { 1, 2 }, { 2, 3 }, { 3, 0 } } };
    std::vector<Point3> intersections;
    intersections.reserve(4);
    for (const auto& edge : cellEdges) {
        if (crossesZero(values[edge[0]], values[edge[1]])) {
            intersections.push_back(interpolateZero(
                points[edge[0]], points[edge[1]], values[edge[0]], values[edge[1]]));
        }
    }
    if (intersections.size() == 2) {
        appendEdgeSegment(edges, intersections[0], intersections[1]);
    } else if (intersections.size() == 4) {
        appendEdgeSegment(edges, intersections[0], intersections[1]);
        appendEdgeSegment(edges, intersections[2], intersections[3]);
    }
}
//=============================================================================
double
valueAt(const double* values, int density, int row, int col, int dep)
{
    return values[row + col * density + dep * density * density];
}
//=============================================================================
void
appendXPlaneContours(
    int col, const double* values, int density, const std::vector<double>& range, EdgeData& edges)
{
    double x = gridValue(range[0], range[1], density, col);
    for (int dep = 0; dep < density - 1; ++dep) {
        for (int row = 0; row < density - 1; ++row) {
            appendContourCell({ Point3 { x, gridValue(range[2], range[3], density, row),
                                    gridValue(range[4], range[5], density, dep) },
                                  Point3 { x, gridValue(range[2], range[3], density, row + 1),
                                      gridValue(range[4], range[5], density, dep) },
                                  Point3 { x, gridValue(range[2], range[3], density, row + 1),
                                      gridValue(range[4], range[5], density, dep + 1) },
                                  Point3 { x, gridValue(range[2], range[3], density, row),
                                      gridValue(range[4], range[5], density, dep + 1) } },
                { valueAt(values, density, row, col, dep),
                    valueAt(values, density, row + 1, col, dep),
                    valueAt(values, density, row + 1, col, dep + 1),
                    valueAt(values, density, row, col, dep + 1) },
                edges);
        }
    }
}
//=============================================================================
void
appendYPlaneContours(
    int row, const double* values, int density, const std::vector<double>& range, EdgeData& edges)
{
    double y = gridValue(range[2], range[3], density, row);
    for (int dep = 0; dep < density - 1; ++dep) {
        for (int col = 0; col < density - 1; ++col) {
            appendContourCell({ Point3 { gridValue(range[0], range[1], density, col), y,
                                    gridValue(range[4], range[5], density, dep) },
                                  Point3 { gridValue(range[0], range[1], density, col + 1), y,
                                      gridValue(range[4], range[5], density, dep) },
                                  Point3 { gridValue(range[0], range[1], density, col + 1), y,
                                      gridValue(range[4], range[5], density, dep + 1) },
                                  Point3 { gridValue(range[0], range[1], density, col), y,
                                      gridValue(range[4], range[5], density, dep + 1) } },
                { valueAt(values, density, row, col, dep),
                    valueAt(values, density, row, col + 1, dep),
                    valueAt(values, density, row, col + 1, dep + 1),
                    valueAt(values, density, row, col, dep + 1) },
                edges);
        }
    }
}
//=============================================================================
void
appendZPlaneContours(
    int dep, const double* values, int density, const std::vector<double>& range, EdgeData& edges)
{
    double z = gridValue(range[4], range[5], density, dep);
    for (int col = 0; col < density - 1; ++col) {
        for (int row = 0; row < density - 1; ++row) {
            appendContourCell({ Point3 { gridValue(range[0], range[1], density, col),
                                    gridValue(range[2], range[3], density, row), z },
                                  Point3 { gridValue(range[0], range[1], density, col + 1),
                                      gridValue(range[2], range[3], density, row), z },
                                  Point3 { gridValue(range[0], range[1], density, col + 1),
                                      gridValue(range[2], range[3], density, row + 1), z },
                                  Point3 { gridValue(range[0], range[1], density, col),
                                      gridValue(range[2], range[3], density, row + 1), z } },
                { valueAt(values, density, row, col, dep),
                    valueAt(values, density, row, col + 1, dep),
                    valueAt(values, density, row + 1, col + 1, dep),
                    valueAt(values, density, row + 1, col, dep) },
                edges);
        }
    }
}
//=============================================================================
EdgeData
contourEdges(const double* values, int density, const std::vector<double>& range)
{
    EdgeData edges;
    edges.x.reserve(static_cast<size_t>(density * density * density));
    edges.y.reserve(static_cast<size_t>(density * density * density));
    edges.z.reserve(static_cast<size_t>(density * density * density));
    for (int index = 0; index < density; ++index) {
        appendXPlaneContours(index, values, density, range, edges);
        appendYPlaneContours(index, values, density, range, edges);
        appendZPlaneContours(index, values, density, range, edges);
    }
    return edges;
}
//=============================================================================
ArrayOf
facesArray(const std::vector<Triangle>& faces)
{
    ArrayOf out = ArrayOf::doubleMatrix2d(static_cast<indexType>(faces.size()), 3);
    double* data = static_cast<double*>(out.readWriteDataPointer());
    for (indexType row = 0; row < static_cast<indexType>(faces.size()); ++row) {
        data[row] = static_cast<double>(faces[row].a);
        data[row + faces.size()] = static_cast<double>(faces[row].b);
        data[row + 2 * faces.size()] = static_cast<double>(faces[row].c);
    }
    return out;
}
//=============================================================================
ArrayOf
verticesArray(const std::vector<Point3>& vertices)
{
    ArrayOf out = ArrayOf::doubleMatrix2d(static_cast<indexType>(vertices.size()), 3);
    double* data = static_cast<double*>(out.readWriteDataPointer());
    for (indexType row = 0; row < static_cast<indexType>(vertices.size()); ++row) {
        data[row] = vertices[row].x;
        data[row + vertices.size()] = vertices[row].y;
        data[row + 2 * vertices.size()] = vertices[row].z;
    }
    return out;
}
//=============================================================================
ArrayOf
edgeArray(const EdgeData& edges, int coordinate)
{
    const std::vector<double>& source
        = coordinate == 0 ? edges.x : (coordinate == 1 ? edges.y : edges.z);
    ArrayOf out = ArrayOf::doubleRowVector(static_cast<indexType>(source.size()));
    double* data = static_cast<double*>(out.readWriteDataPointer());
    std::copy(source.begin(), source.end(), data);
    return out;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::__fimplicit3_mesh__Builtin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 5);
    nargincheck(argIn, 3, 3);
    FunctionDef* funcDef = functionDefinition(argIn[0]);
    std::vector<double> range = argIn[1].asDoubleVector();
    if (range.size() != 6) {
        Error(L"Nelson:fimplicit3:invalidRange", L"Range must have six elements.");
    }
    int density = static_cast<int>(argIn[2].asDoubleScalar());
    ArrayOf values = evaluateValues(eval, funcDef, range, density);
    values.promoteType(NLS_DOUBLE);
    const double* valueData = static_cast<const double*>(values.dataPointer());
    std::vector<Point3> vertices;
    std::vector<Triangle> faces;
    vertices.reserve(static_cast<size_t>(density * density * density));
    faces.reserve(static_cast<size_t>(density * density * density));
    for (int row = 0; row < density - 1; ++row) {
        for (int col = 0; col < density - 1; ++col) {
            for (int dep = 0; dep < density - 1; ++dep) {
                appendCube(row, col, dep, valueData, density, range, vertices, faces);
            }
        }
    }
    EdgeData edges = contourEdges(valueData, density, range);
    ArrayOfVector retval;
    retval << facesArray(faces) << verticesArray(vertices) << edgeArray(edges, 0)
           << edgeArray(edges, 1) << edgeArray(edges, 2);
    return retval;
}
//=============================================================================
