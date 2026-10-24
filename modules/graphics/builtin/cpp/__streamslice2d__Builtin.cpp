//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__streamslice2d__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <memory>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
struct Point
{
    double x;
    double y;
};
//=============================================================================
struct Field2D
{
    std::vector<double> x;
    std::vector<double> y;
    std::vector<double> u;
    std::vector<double> v;
    indexType rows { 0 };
    indexType cols { 0 };
};
//=============================================================================
struct StreamSliceResult
{
    std::vector<std::vector<Point>> lines;
    std::vector<std::vector<Point>> arrows;
};
//=============================================================================
ArrayOf
toDoubleMatrix(const ArrayOf& value)
{
    ArrayOf result(value);
    if (!result.isNumeric() || result.isComplex() || !result.is2D()) {
        Error("Nelson:streamslice:invalidData", "Expected real 2-D numeric arrays.");
    }
    result.promoteType(NLS_DOUBLE);
    return result;
}
//=============================================================================
double
at(const std::vector<double>& values, indexType rows, indexType row, indexType col)
{
    return values[static_cast<size_t>(row + col * rows)];
}
//=============================================================================
std::vector<double>
matrixData(const ArrayOf& value)
{
    const double* data = static_cast<const double*>(value.dataPointer());
    return std::vector<double>(data, data + value.elementCount());
}
//=============================================================================
Field2D
fieldFromArgs(const ArrayOfVector& argIn)
{
    ArrayOf x = toDoubleMatrix(argIn[0]);
    ArrayOf y = toDoubleMatrix(argIn[1]);
    ArrayOf u = toDoubleMatrix(argIn[2]);
    ArrayOf v = toDoubleMatrix(argIn[3]);
    if (!x.dimensions().equals(y.dimensions()) || !x.dimensions().equals(u.dimensions())
        || !x.dimensions().equals(v.dimensions())) {
        Error("Nelson:streamlineBase:invalidGridArrays",
            "Grid arrays must have the same size as vector components.");
    }
    Field2D field;
    field.rows = x.rows();
    field.cols = x.columns();
    field.u = matrixData(u);
    field.v = matrixData(v);
    std::vector<double> xData = matrixData(x);
    std::vector<double> yData = matrixData(y);
    field.x.resize(static_cast<size_t>(field.cols));
    field.y.resize(static_cast<size_t>(field.rows));
    for (indexType col = 0; col < field.cols; ++col) {
        field.x[static_cast<size_t>(col)] = at(xData, field.rows, 0, col);
    }
    for (indexType row = 0; row < field.rows; ++row) {
        field.y[static_cast<size_t>(row)] = at(yData, field.rows, row, 0);
    }
    return field;
}
//=============================================================================
bool
insideRange(double value, double first, double last)
{
    double lo = std::min(first, last);
    double hi = std::max(first, last);
    return value >= lo && value <= hi;
}
//=============================================================================
bool
findInterval(const std::vector<double>& grid, double value, indexType& index, double& fraction)
{
    const indexType count = static_cast<indexType>(grid.size());
    if (count < 2 || !insideRange(value, grid.front(), grid.back())) {
        return false;
    }
    const bool ascending = grid.back() >= grid.front();
    for (indexType k = 0; k < count - 1; ++k) {
        double a = grid[static_cast<size_t>(k)];
        double b = grid[static_cast<size_t>(k + 1)];
        if ((ascending && value >= a && value <= b) || (!ascending && value <= a && value >= b)) {
            index = k;
            fraction = (b == a) ? 0. : (value - a) / (b - a);
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
sample(const Field2D& field, const Point& point, Point& vector)
{
    indexType col = 0;
    indexType row = 0;
    double tx = 0.;
    double ty = 0.;
    if (!findInterval(field.x, point.x, col, tx) || !findInterval(field.y, point.y, row, ty)) {
        return false;
    }
    double u00 = at(field.u, field.rows, row, col);
    double u10 = at(field.u, field.rows, row, col + 1);
    double u01 = at(field.u, field.rows, row + 1, col);
    double u11 = at(field.u, field.rows, row + 1, col + 1);
    double v00 = at(field.v, field.rows, row, col);
    double v10 = at(field.v, field.rows, row, col + 1);
    double v01 = at(field.v, field.rows, row + 1, col);
    double v11 = at(field.v, field.rows, row + 1, col + 1);
    vector.x = (1. - ty) * ((1. - tx) * u00 + tx * u10) + ty * ((1. - tx) * u01 + tx * u11);
    vector.y = (1. - ty) * ((1. - tx) * v00 + tx * v10) + ty * ((1. - tx) * v01 + tx * v11);
    return std::isfinite(vector.x) && std::isfinite(vector.y);
}
//=============================================================================
double
streamStep(const Field2D& field)
{
    double dx = std::numeric_limits<double>::infinity();
    double dy = std::numeric_limits<double>::infinity();
    for (size_t k = 1; k < field.x.size(); ++k) {
        double d = std::abs(field.x[k] - field.x[k - 1]);
        if (d > 0.) {
            dx = std::min(dx, d);
        }
    }
    for (size_t k = 1; k < field.y.size(); ++k) {
        double d = std::abs(field.y[k] - field.y[k - 1]);
        if (d > 0.) {
            dy = std::min(dy, d);
        }
    }
    double step = std::min(dx, dy);
    return std::isfinite(step) ? std::min(0.1, 0.1 * step) : 0.1;
}
//=============================================================================
std::vector<Point>
trace(const Field2D& field, const Point& start, double direction)
{
    std::vector<Point> points;
    points.push_back(start);
    Point current = start;
    const double step = streamStep(field);
    for (int k = 0; k < 10000; ++k) {
        Point vec;
        if (!sample(field, current, vec)) {
            break;
        }
        vec.x *= direction;
        vec.y *= direction;
        double len = std::hypot(vec.x, vec.y);
        if (!std::isfinite(len) || len < std::numeric_limits<double>::epsilon()) {
            break;
        }
        current.x += step * vec.x / len;
        current.y += step * vec.y / len;
        if (!insideRange(current.x, field.x.front(), field.x.back())
            || !insideRange(current.y, field.y.front(), field.y.back())) {
            break;
        }
        points.push_back(current);
    }
    return points;
}
//=============================================================================
bool
gridCell(const Point& point, double xmin, double xmax, double ymin, double ymax, int rows, int cols,
    int& row, int& col)
{
    if (point.x < xmin || point.x > xmax || point.y < ymin || point.y > ymax) {
        return false;
    }
    col = static_cast<int>(std::floor((point.x - xmin) / (xmax - xmin) * cols));
    row = static_cast<int>(std::floor((point.y - ymin) / (ymax - ymin) * rows));
    col = std::clamp(col, 0, cols - 1);
    row = std::clamp(row, 0, rows - 1);
    return true;
}
//=============================================================================
std::vector<Point>
arrowAt(const Point& previous, const Point& point, double flowSign, double scale)
{
    Point tangent { flowSign * (point.x - previous.x), flowSign * (point.y - previous.y) };
    double len = std::hypot(tangent.x, tangent.y);
    if (len == 0.) {
        return {};
    }
    tangent.x /= len;
    tangent.y /= len;
    Point normal { -tangent.y, tangent.x };
    Point base { point.x - scale * tangent.x, point.y - scale * tangent.y };
    double width = 0.6 * scale;
    return { { base.x - width * normal.x, base.y - width * normal.y }, point,
        { base.x + width * normal.x, base.y + width * normal.y } };
}
//=============================================================================
std::vector<Point>
trim(const std::vector<Point>& input, bool forward, std::vector<bool>& startGrid,
    std::vector<bool>& endGrid, std::vector<bool>& arrowGrid, StreamSliceResult& result,
    bool arrows, const std::vector<int>& gridSizes, const std::vector<double>& bounds)
{
    std::vector<Point> output;
    int previousEnd = -1;
    const double arrowScale = 0.01 * ((bounds[1] - bounds[0]) + (bounds[3] - bounds[2]));
    for (size_t k = 0; k < input.size(); ++k) {
        int row = 0;
        int col = 0;
        if (gridCell(input[k], bounds[0], bounds[1], bounds[2], bounds[3], gridSizes[0],
                gridSizes[0], row, col)) {
            startGrid[static_cast<size_t>(row * gridSizes[0] + col)] = true;
        }
        if (!gridCell(input[k], bounds[0], bounds[1], bounds[2], bounds[3], gridSizes[1],
                gridSizes[1], row, col)) {
            break;
        }
        int endIndex = row * gridSizes[1] + col;
        if (endGrid[static_cast<size_t>(endIndex)] && endIndex != previousEnd) {
            break;
        }
        endGrid[static_cast<size_t>(endIndex)] = true;
        previousEnd = endIndex;
        output.push_back(input[k]);
        if (arrows && k > 0
            && gridCell(input[k], bounds[0], bounds[1], bounds[2], bounds[3], gridSizes[2],
                gridSizes[2], row, col)) {
            int arrowIndex = row * gridSizes[2] + col;
            if (arrowGrid[static_cast<size_t>(arrowIndex)]) {
                arrowGrid[static_cast<size_t>(arrowIndex)] = false;
                result.arrows.push_back(
                    arrowAt(input[k - 1], input[k], forward ? 1. : -1., arrowScale));
            }
        }
    }
    return output;
}
//=============================================================================
StreamSliceResult
computeStreamSlice2D(const Field2D& field, double density, bool arrows)
{
    StreamSliceResult result;
    density = std::sqrt(density);
    // Seed, occupancy and arrow grids scale with the data resolution, so the
    // automatic streamline coverage holds as density is fixed. The scaling is capped
    // on very large grids to keep the seed sweep bounded. A 5-point axis gives
    // 20/80/20 at density 1.
    const double baseRes
        = std::min(20., static_cast<double>(std::max<indexType>(field.cols, field.rows)));
    int startCount = std::max(2, static_cast<int>(std::ceil(4. * baseRes * density)));
    int endCount = std::max(2, static_cast<int>(std::ceil(16. * baseRes * density)));
    int arrowCount = std::max(2, static_cast<int>(std::ceil(4. * baseRes * density)));
    std::vector<bool> startGrid(static_cast<size_t>(startCount * startCount), false);
    std::vector<bool> endGrid(static_cast<size_t>(endCount * endCount), false);
    std::vector<bool> arrowGrid(static_cast<size_t>(arrowCount * arrowCount), false);
    for (int row = 1; row < arrowCount; row += 3) {
        for (int col = 1; col < arrowCount; col += 3) {
            arrowGrid[static_cast<size_t>(row * arrowCount + col)] = true;
        }
    }
    std::vector<double> bounds { std::min(field.x.front(), field.x.back()),
        std::max(field.x.front(), field.x.back()), std::min(field.y.front(), field.y.back()),
        std::max(field.y.front(), field.y.back()) };
    std::vector<int> gridSizes { startCount, endCount, arrowCount };
    double dx = (bounds[1] - bounds[0]) / startCount;
    double dy = (bounds[3] - bounds[2]) / startCount;
    for (int row = 0; row < startCount; ++row) {
        for (int col = 0; col < startCount; ++col) {
            size_t idx = static_cast<size_t>(row * startCount + col);
            if (startGrid[idx]) {
                continue;
            }
            Point start { bounds[0] + (col + 0.5) * dx, bounds[2] + (row + 0.5) * dy };
            startGrid[idx] = true;
            auto forward = trim(trace(field, start, 1.), true, startGrid, endGrid, arrowGrid,
                result, arrows, gridSizes, bounds);
            auto backward = trim(trace(field, start, -1.), false, startGrid, endGrid, arrowGrid,
                result, arrows, gridSizes, bounds);
            std::vector<Point> line;
            line.reserve(backward.size() + forward.size());
            for (auto it = backward.rbegin(); it != backward.rend(); ++it) {
                line.push_back(*it);
            }
            if (!forward.empty()) {
                line.insert(line.end(), forward.begin() + 1, forward.end());
            }
            if (line.size() >= 2) {
                result.lines.push_back(std::move(line));
            }
        }
    }
    return result;
}
//=============================================================================
ArrayOf
pointsMatrix(const std::vector<Point>& points)
{
    ArrayOf matrix = ArrayOf::doubleMatrix2d(static_cast<indexType>(points.size()), 2);
    double* data = static_cast<double*>(const_cast<void*>(matrix.dataPointer()));
    for (size_t k = 0; k < points.size(); ++k) {
        data[k] = points[k].x;
        data[k + points.size()] = points[k].y;
    }
    return matrix;
}
//=============================================================================
ArrayOf
pointsCell(const std::vector<std::vector<Point>>& lines)
{
    Dimensions dims(static_cast<indexType>(lines.size()), 1);
    ArrayOf* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.elementCount(), stringVector(), false));
    for (size_t k = 0; k < lines.size(); ++k) {
        elements[k] = pointsMatrix(lines[k]);
    }
    return ArrayOf(NLS_CELL_ARRAY, dims, elements);
}
//=============================================================================
} // namespace
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
__streamslice2d__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 6, 6);
    nargoutcheck(nLhs, 0, 2);
    double density = argIn[4].asDoubleScalar();
    bool arrows = argIn[5].asLogicalScalar();
    if (!(density > 0.) || !std::isfinite(density)) {
        Error("Nelson:streamslice:invalidDensity", "Density must be a positive scalar.");
    }
    StreamSliceResult result = computeStreamSlice2D(fieldFromArgs(argIn), density, arrows);
    ArrayOfVector retval;
    retval << pointsCell(result.lines) << pointsCell(result.arrows);
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
