//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RegionProps.hpp"
#include "BWConnComp.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NewWithException.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cctype>
#include <cmath>
#include <limits>
#include <map>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
static const stringVector SUPPORTED_PROPS { "Area", "Centroid", "BoundingBox", "PixelIdxList",
    "PixelList", "Image", "MeanIntensity", "MinIntensity", "MaxIntensity", "PixelValues",
    "WeightedCentroid", "SubarrayIdx", "Extent", "EquivDiameter", "Perimeter", "Orientation",
    "MajorAxisLength", "MinorAxisLength", "Eccentricity", "ConvexHull", "ConvexImage", "ConvexArea",
    "Solidity" };
//=============================================================================
static const stringVector NEEDS_INTENSITY { "MeanIntensity", "MinIntensity", "MaxIntensity",
    "PixelValues", "WeightedCentroid" };
//=============================================================================
struct RegionData
{
    indexType rows { 0 };
    indexType columns { 0 };
    std::vector<std::vector<indexType>> components;
};
//=============================================================================
struct EllipseProps
{
    double orientation { 0 };
    double majorAxisLength { 0 };
    double minorAxisLength { 0 };
    double eccentricity { 0 };
};
//=============================================================================
struct Point2D
{
    double x { 0 };
    double y { 0 };
};
//=============================================================================
struct IntensityStats
{
    double sum { 0 };
    double minValue { 0 };
    double maxValue { 0 };
    double weightedX { 0 };
    double weightedY { 0 };
};
//=============================================================================
struct ComponentCache
{
    bool boundsReady { false };
    bool geometryReady { false };
    std::vector<indexType> rr;
    std::vector<indexType> cc;
    indexType minRow { 0 };
    indexType maxRow { 0 };
    indexType minCol { 0 };
    indexType maxCol { 0 };
    indexType height { 0 };
    indexType width { 0 };
    double sumX { 0 };
    double sumY { 0 };
    double sumXX { 0 };
    double sumYY { 0 };
    double sumXY { 0 };
    bool convexHullReady { false };
    std::vector<Point2D> convexHull;
    bool convexMaskReady { false };
    std::vector<uint8_t> convexMask;
    bool ellipseReady { false };
    EllipseProps ellipse;
    bool intensityStatsReady { false };
    IntensityStats intensityStats;
};
//=============================================================================
struct IntensityData
{
    std::vector<double> storage;
    const double* values { nullptr };

    bool
    isEmpty() const
    {
        return values == nullptr;
    }

    double
    at(indexType index) const
    {
        return values[index];
    }
};
//=============================================================================
static std::string
lowerAscii(std::string text)
{
    std::transform(text.begin(), text.end(), text.begin(),
        [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return text;
}
//=============================================================================
static std::string
textValue(const ArrayOf& value)
{
    if (!(value.isRowVectorCharacterArray() || value.isScalarStringArray())) {
        Error(L"Nelson:image_processing:unsupportedRegionProperty", L"Unsupported \"%s\" option.",
            L"region property");
    }
    std::wstring wide = value.asWideString();
    std::string text;
    text.reserve(wide.size());
    for (wchar_t c : wide) {
        text.push_back(static_cast<char>(c));
    }
    return text;
}
//=============================================================================
static std::string
canonicalProperty(const ArrayOf& value)
{
    std::string lower = lowerAscii(textValue(value));
    for (const auto& prop : SUPPORTED_PROPS) {
        if (lowerAscii(prop) == lower) {
            return prop;
        }
    }
    Error(L"Nelson:image_processing:unsupportedRegionProperty", L"Unsupported \"%s\" option.",
        L"region property");
    return {};
}
//=============================================================================
static bool
needsIntensity(const std::string& prop)
{
    return std::find(NEEDS_INTENSITY.begin(), NEEDS_INTENSITY.end(), prop) != NEEDS_INTENSITY.end();
}
//=============================================================================
static std::vector<double>
asDoubleVector(const ArrayOf& value)
{
    ArrayOf work(value);
    work.promoteType(NLS_DOUBLE);
    const auto* ptr = static_cast<const double*>(work.dataPointer());
    return std::vector<double>(ptr, ptr + work.elementCount());
}
//=============================================================================
static IntensityData
intensityDataFromArray(const ArrayOf& intensity)
{
    IntensityData data;
    if (intensity.isEmpty()) {
        return data;
    }
    if (intensity.dataClass() == NLS_DOUBLE) {
        data.values = static_cast<const double*>(intensity.dataPointer());
        return data;
    }
    data.storage = asDoubleVector(intensity);
    data.values = data.storage.data();
    return data;
}
//=============================================================================
static void
validate2DImage(const ArrayOf& value, const std::wstring& dimensionMessage)
{
    if (!value.isNumeric() && !value.isLogical()) {
        Error(L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical.",
            L"Image");
    }
    if (value.isSparse()) {
        Error(L"Nelson:image_processing:sparseImagesNotSupported", L"%s are not supported.",
            L"Sparse images");
    }
    if (value.dimensions().length() > 2) {
        Error(L"Nelson:regionprops:invalidDimension", dimensionMessage);
    }
}
//=============================================================================
static void
validateLabelMatrix(const ArrayOf& value)
{
    validate2DImage(value,
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:image_processing:imageDArray", L"%s must be a 2-D array."),
            L"Image"));
    std::vector<double> data = asDoubleVector(value);
    for (double x : data) {
        if (!std::isfinite(x) || x < 0 || std::floor(x) != x) {
            Error(L"Nelson:image_processing:labelMatrixValuesNonnegativeFiniteIntegers",
                L"%s values must be nonnegative finite integers.", L"Label matrix");
        }
    }
}
//=============================================================================
static std::vector<indexType>
indicesFromArray(const ArrayOf& value)
{
    std::vector<double> data = asDoubleVector(value);
    std::vector<indexType> indices;
    indices.reserve(data.size());
    for (double x : data) {
        if (x >= 1 && std::isfinite(x)) {
            indices.push_back(static_cast<indexType>(x - 1));
        }
    }
    std::sort(indices.begin(), indices.end());
    return indices;
}
//=============================================================================
static RegionData
regionDataFromCC(const ArrayOf& cc)
{
    if (!cc.isStruct() || !cc.isField("PixelIdxList") || !cc.isField("ImageSize")) {
        Error(L"Nelson:image_processing:unsupportedRegionProperty", L"Unsupported \"%s\" option.",
            L"region property");
    }
    std::vector<double> imageSize = asDoubleVector(cc.field("ImageSize"));
    if (imageSize.size() > 2) {
        Error(L"Nelson:image_processing:imageDArray", L"%s must be a 2-D array.", L"Image");
    }
    ArrayOf pixelList = cc.field("PixelIdxList");
    if (!pixelList.isCell()) {
        Error(L"Nelson:image_processing:unsupportedRegionProperty", L"Unsupported \"%s\" option.",
            L"region property");
    }
    auto* cells = static_cast<ArrayOf*>(pixelList.readWriteDataPointer());
    RegionData data;
    data.rows = static_cast<indexType>(imageSize[0]);
    data.columns = static_cast<indexType>(imageSize[1]);
    for (indexType k = 0; k < pixelList.elementCount(); k++) {
        data.components.push_back(indicesFromArray(cells[k]));
    }
    return data;
}
//=============================================================================
static RegionData
regionDataFromLabels(const ArrayOf& labels)
{
    validateLabelMatrix(labels);
    std::vector<double> values = asDoubleVector(labels);
    std::map<double, std::vector<indexType>> byLabel;
    for (indexType k = 0; k < values.size(); k++) {
        if (values[k] > 0) {
            byLabel[values[k]].push_back(k);
        }
    }
    RegionData data;
    data.rows = labels.rows();
    data.columns = labels.columns();
    for (auto& item : byLabel) {
        data.components.push_back(std::move(item.second));
    }
    return data;
}
//=============================================================================
static RegionData
regionDataFromInput(const ArrayOf& input)
{
    if (input.isStruct() && input.isField("PixelIdxList")) {
        return regionDataFromCC(input);
    }
    if (input.isLogical()) {
        return regionDataFromCC(BWConnComp(input, 8));
    }
    return regionDataFromLabels(input);
}
//=============================================================================
static std::vector<std::string>
propertiesFromCell(const ArrayOf& cell)
{
    const auto* cells = static_cast<const ArrayOf*>(cell.dataPointer());
    std::vector<std::string> props;
    for (indexType k = 0; k < cell.elementCount(); k++) {
        props.push_back(canonicalProperty(cells[k]));
    }
    return props;
}
//=============================================================================
static std::vector<std::string>
parseProperties(const ArrayOfVector& argIn, size_t propStart, bool hasIntensity)
{
    if (propStart >= argIn.size()) {
        return { "Area", "Centroid", "BoundingBox" };
    }
    if (argIn.size() == propStart + 1 && argIn[propStart].isCell()) {
        return propertiesFromCell(argIn[propStart]);
    }
    if (argIn.size() == propStart + 1) {
        std::string selector = lowerAscii(textValue(argIn[propStart]));
        if (selector == "basic") {
            return { "Area", "Centroid", "BoundingBox" };
        }
        if (selector == "all") {
            std::vector<std::string> props;
            for (const auto& prop : SUPPORTED_PROPS) {
                if (hasIntensity || !needsIntensity(prop)) {
                    props.push_back(prop);
                }
            }
            return props;
        }
    }
    std::vector<std::string> props;
    for (size_t k = propStart; k < argIn.size(); k++) {
        props.push_back(canonicalProperty(argIn[k]));
    }
    return props;
}
//=============================================================================
static ArrayOf
doubleRow(const std::vector<double>& values)
{
    auto* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, values.size()));
    std::copy(values.begin(), values.end(), ptr);
    return ArrayOf(NLS_DOUBLE, Dimensions(1, values.size()), ptr);
}
//=============================================================================
static ArrayOf
doubleColumn(const std::vector<double>& values)
{
    auto* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, values.size()));
    std::copy(values.begin(), values.end(), ptr);
    return ArrayOf(NLS_DOUBLE, Dimensions(values.size(), 1), ptr);
}
//=============================================================================
static ArrayOf
doubleRow2(double first, double second)
{
    auto* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 2));
    ptr[0] = first;
    ptr[1] = second;
    return ArrayOf(NLS_DOUBLE, Dimensions(1, 2), ptr);
}
//=============================================================================
static ArrayOf
doubleRow4(double first, double second, double third, double fourth)
{
    auto* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 4));
    ptr[0] = first;
    ptr[1] = second;
    ptr[2] = third;
    ptr[3] = fourth;
    return ArrayOf(NLS_DOUBLE, Dimensions(1, 4), ptr);
}
//=============================================================================
static ArrayOf
pixelIdxList(const std::vector<indexType>& idx)
{
    auto* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, idx.size()));
    for (size_t k = 0; k < idx.size(); k++) {
        ptr[k] = static_cast<double>(idx[k] + 1);
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(idx.size(), 1), ptr);
}
//=============================================================================
static void
ensureBounds(const std::vector<indexType>& idx, const RegionData& data, ComponentCache& cache)
{
    if (cache.boundsReady) {
        return;
    }
    cache.minRow = data.rows;
    cache.minCol = data.columns;
    for (indexType value : idx) {
        indexType r = value % data.rows;
        indexType c = value / data.rows;
        cache.minRow = std::min(cache.minRow, r);
        cache.maxRow = std::max(cache.maxRow, r);
        cache.minCol = std::min(cache.minCol, c);
        cache.maxCol = std::max(cache.maxCol, c);
    }
    cache.height = cache.maxRow - cache.minRow + 1;
    cache.width = cache.maxCol - cache.minCol + 1;
    cache.boundsReady = true;
}
//=============================================================================
static void
ensureGeometry(const std::vector<indexType>& idx, const RegionData& data, ComponentCache& cache)
{
    if (cache.geometryReady) {
        return;
    }
    cache.rr.resize(idx.size());
    cache.cc.resize(idx.size());
    if (!cache.boundsReady) {
        cache.minRow = data.rows;
        cache.minCol = data.columns;
    }
    for (size_t k = 0; k < idx.size(); k++) {
        indexType r = idx[k] % data.rows;
        indexType c = idx[k] / data.rows;
        cache.rr[k] = r;
        cache.cc[k] = c;
        if (!cache.boundsReady) {
            cache.minRow = std::min(cache.minRow, r);
            cache.maxRow = std::max(cache.maxRow, r);
            cache.minCol = std::min(cache.minCol, c);
            cache.maxCol = std::max(cache.maxCol, c);
        }
        double x = static_cast<double>(c + 1);
        double y = static_cast<double>(r + 1);
        cache.sumX += x;
        cache.sumY += y;
        cache.sumXX += x * x;
        cache.sumYY += y * y;
        cache.sumXY += x * y;
    }
    if (!cache.boundsReady) {
        cache.height = cache.maxRow - cache.minRow + 1;
        cache.width = cache.maxCol - cache.minCol + 1;
        cache.boundsReady = true;
    }
    cache.geometryReady = true;
}
//=============================================================================
static ArrayOf
pixelList(const std::vector<indexType>& rr, const std::vector<indexType>& cc)
{
    auto* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, rr.size() * 2));
    for (size_t k = 0; k < rr.size(); k++) {
        ptr[k] = static_cast<double>(cc[k] + 1);
        ptr[k + rr.size()] = static_cast<double>(rr[k] + 1);
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(rr.size(), 2), ptr);
}
//=============================================================================
static ArrayOf
regionImageFromIndices(const std::vector<indexType>& idx, indexType rows, indexType minRow,
    indexType minCol, indexType height, indexType width)
{
    auto* ptr
        = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, height * width, {}, true));
    for (indexType value : idx) {
        indexType r = value % rows - minRow;
        indexType c = value / rows - minCol;
        ptr[r + c * height] = true;
    }
    return ArrayOf(NLS_LOGICAL, Dimensions(height, width), ptr);
}
//=============================================================================
static double
cross(const Point2D& origin, const Point2D& a, const Point2D& b)
{
    return (a.x - origin.x) * (b.y - origin.y) - (a.y - origin.y) * (b.x - origin.x);
}
//=============================================================================
static void
appendPixelCorners(std::vector<Point2D>& points, indexType row, indexType column)
{
    double left = static_cast<double>(column) + 0.5;
    double right = static_cast<double>(column) + 1.5;
    double top = static_cast<double>(row) + 0.5;
    double bottom = static_cast<double>(row) + 1.5;
    points.push_back({ left, top });
    points.push_back({ right, top });
    points.push_back({ right, bottom });
    points.push_back({ left, bottom });
}
//=============================================================================
static std::vector<Point2D>
convexHullFromPoints(std::vector<Point2D>& points)
{
    std::sort(points.begin(), points.end(),
        [](const Point2D& a, const Point2D& b) { return a.x == b.x ? a.y < b.y : a.x < b.x; });
    points.erase(std::unique(points.begin(), points.end(),
                     [](const Point2D& a, const Point2D& b) { return a.x == b.x && a.y == b.y; }),
        points.end());
    std::vector<Point2D> hull;
    for (const Point2D& p : points) {
        while (hull.size() >= 2 && cross(hull[hull.size() - 2], hull.back(), p) <= 0) {
            hull.pop_back();
        }
        hull.push_back(p);
    }
    size_t lowerSize = hull.size();
    for (auto it = points.rbegin() + 1; it != points.rend(); ++it) {
        while (hull.size() > lowerSize && cross(hull[hull.size() - 2], hull.back(), *it) <= 0) {
            hull.pop_back();
        }
        hull.push_back(*it);
    }
    if (!hull.empty()) {
        hull.pop_back();
    }
    return hull;
}
//=============================================================================
static bool
isComponentPixel(const std::vector<uint8_t>& mask, indexType height, indexType width,
    signedIndexType row, signedIndexType column)
{
    if (row < 0 || column < 0 || row >= static_cast<signedIndexType>(height)
        || column >= static_cast<signedIndexType>(width)) {
        return false;
    }
    return mask[static_cast<indexType>(row) + static_cast<indexType>(column) * height] != 0;
}
//=============================================================================
static std::vector<Point2D>
convexHullPoints(const ComponentCache& cache)
{
    std::vector<uint8_t> mask(cache.height * cache.width, 0);
    for (size_t k = 0; k < cache.rr.size(); k++) {
        indexType r = cache.rr[k] - cache.minRow;
        indexType c = cache.cc[k] - cache.minCol;
        mask[r + c * cache.height] = 1;
    }

    std::vector<Point2D> points;
    points.reserve(cache.rr.size() * 2);
    for (size_t k = 0; k < cache.rr.size(); k++) {
        indexType r = cache.rr[k] - cache.minRow;
        indexType c = cache.cc[k] - cache.minCol;
        signedIndexType sr = static_cast<signedIndexType>(r);
        signedIndexType sc = static_cast<signedIndexType>(c);
        bool boundary = !isComponentPixel(mask, cache.height, cache.width, sr - 1, sc)
            || !isComponentPixel(mask, cache.height, cache.width, sr + 1, sc)
            || !isComponentPixel(mask, cache.height, cache.width, sr, sc - 1)
            || !isComponentPixel(mask, cache.height, cache.width, sr, sc + 1);
        if (boundary) {
            appendPixelCorners(points, cache.rr[k], cache.cc[k]);
        }
    }
    return convexHullFromPoints(points);
}
//=============================================================================
static ArrayOf
convexHullArray(const std::vector<Point2D>& hull)
{
    auto* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, hull.size() * 2));
    for (size_t k = 0; k < hull.size(); k++) {
        ptr[k] = hull[k].x;
        ptr[k + hull.size()] = hull[k].y;
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(hull.size(), 2), ptr);
}
//=============================================================================
static void
ensureConvexHull(ComponentCache& cache)
{
    if (cache.convexHullReady) {
        return;
    }
    cache.convexHull = convexHullPoints(cache);
    cache.convexHullReady = true;
}
//=============================================================================
static void
ensureConvexMask(ComponentCache& cache)
{
    if (cache.convexMaskReady) {
        return;
    }
    ensureConvexHull(cache);
    cache.convexMask.assign(cache.height * cache.width, 0);
    const double tolerance = 1e-12;
    std::vector<double> intersections;
    intersections.reserve(cache.convexHull.size() * 2);
    for (indexType r = 0; r < cache.height; r++) {
        double y = static_cast<double>(cache.minRow + r) + 1;
        intersections.clear();
        for (size_t k = 0; k < cache.convexHull.size(); k++) {
            const Point2D& a = cache.convexHull[k];
            const Point2D& b = cache.convexHull[(k + 1) % cache.convexHull.size()];
            if (std::abs(a.y - b.y) <= tolerance) {
                if (std::abs(y - a.y) <= tolerance) {
                    intersections.push_back(a.x);
                    intersections.push_back(b.x);
                }
                continue;
            }
            double minY = std::min(a.y, b.y);
            double maxY = std::max(a.y, b.y);
            if (y < minY - tolerance || y > maxY + tolerance) {
                continue;
            }
            double t = (y - a.y) / (b.y - a.y);
            if (t >= -tolerance && t <= 1 + tolerance) {
                intersections.push_back(a.x + t * (b.x - a.x));
            }
        }
        if (intersections.empty()) {
            continue;
        }
        auto bounds = std::minmax_element(intersections.begin(), intersections.end());
        signedIndexType start = static_cast<signedIndexType>(
            std::ceil(*bounds.first - tolerance - static_cast<double>(cache.minCol) - 1));
        signedIndexType stop = static_cast<signedIndexType>(
            std::floor(*bounds.second + tolerance - static_cast<double>(cache.minCol) - 1));
        if (start < 0) {
            start = 0;
        }
        if (stop >= static_cast<signedIndexType>(cache.width)) {
            stop = static_cast<signedIndexType>(cache.width) - 1;
        }
        for (signedIndexType c = start; c <= stop; c++) {
            cache.convexMask[r + static_cast<indexType>(c) * cache.height] = 1;
        }
    }
    cache.convexMaskReady = true;
}
//=============================================================================
static ArrayOf
convexImage(const ComponentCache& cache)
{
    auto* ptr
        = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, cache.convexMask.size()));
    for (size_t k = 0; k < cache.convexMask.size(); k++) {
        ptr[k] = cache.convexMask[k] != 0;
    }
    return ArrayOf(NLS_LOGICAL, Dimensions(cache.height, cache.width), ptr);
}
//=============================================================================
static double
convexArea(const ComponentCache& cache)
{
    return static_cast<double>(std::count(cache.convexMask.begin(), cache.convexMask.end(), 1));
}
//=============================================================================
static ArrayOf
rangeCell(indexType firstRow, indexType lastRow, indexType firstCol, indexType lastCol)
{
    auto* elements = new_with_exception<ArrayOf>(2, false);
    indexType rowCount = lastRow - firstRow + 1;
    auto* rows = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, rowCount));
    for (indexType k = 0; k < rowCount; k++) {
        rows[k] = static_cast<double>(firstRow + k + 1);
    }
    elements[0] = ArrayOf(NLS_DOUBLE, Dimensions(1, rowCount), rows);
    indexType colCount = lastCol - firstCol + 1;
    auto* cols = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, colCount));
    for (indexType k = 0; k < colCount; k++) {
        cols[k] = static_cast<double>(firstCol + k + 1);
    }
    elements[1] = ArrayOf(NLS_DOUBLE, Dimensions(1, colCount), cols);
    return ArrayOf(NLS_CELL_ARRAY, Dimensions(1, 2), elements);
}
//=============================================================================
static double
perimeterCount(const std::vector<indexType>& rr, const std::vector<indexType>& cc, indexType minRow,
    indexType minCol, indexType height, indexType width)
{
    std::vector<uint8_t> mask(height * width, 0);
    for (size_t k = 0; k < rr.size(); k++) {
        mask[(rr[k] - minRow) + (cc[k] - minCol) * height] = 1;
    }
    double count = 0;
    for (size_t k = 0; k < rr.size(); k++) {
        indexType r = rr[k] - minRow;
        indexType c = cc[k] - minCol;
        bool boundary = false;
        for (int dy = -1; dy <= 1 && !boundary; dy++) {
            for (int dx = -1; dx <= 1; dx++) {
                signedIndexType y = static_cast<signedIndexType>(r) + dy;
                signedIndexType x = static_cast<signedIndexType>(c) + dx;
                if (y < 0 || x < 0 || y >= static_cast<signedIndexType>(height)
                    || x >= static_cast<signedIndexType>(width) || !mask[y + x * height]) {
                    boundary = true;
                    break;
                }
            }
        }
        count += boundary ? 1 : 0;
    }
    return count;
}
//=============================================================================
static EllipseProps
ellipseProps(const ComponentCache& cache)
{
    double invCount = 1.0 / cache.rr.size();
    double meanX = cache.sumX * invCount;
    double meanY = cache.sumY * invCount;
    double mu20 = cache.sumXX * invCount - meanX * meanX;
    double mu02 = cache.sumYY * invCount - meanY * meanY;
    double mu11 = cache.sumXY * invCount - meanX * meanY;
    double common = std::sqrt((mu20 - mu02) * (mu20 - mu02) + 4 * mu11 * mu11);
    double majorMoment = std::max(0.0, mu20 + mu02 + common);
    double minorMoment = std::max(0.0, mu20 + mu02 - common);
    EllipseProps out;
    out.orientation = 0.5 * std::atan2(2 * mu11, mu20 - mu02) * 180.0 / std::acos(-1);
    out.majorAxisLength = 2 * std::sqrt(2.0) * std::sqrt(majorMoment);
    out.minorAxisLength = 2 * std::sqrt(2.0) * std::sqrt(minorMoment);
    if (out.majorAxisLength > 0) {
        double ratio = out.minorAxisLength / out.majorAxisLength;
        out.eccentricity = std::sqrt(std::max(0.0, 1 - ratio * ratio));
    }
    return out;
}
//=============================================================================
static void
ensureEllipseProps(ComponentCache& cache)
{
    if (cache.ellipseReady) {
        return;
    }
    cache.ellipse = ellipseProps(cache);
    cache.ellipseReady = true;
}
//=============================================================================
static void
ensureIntensityStats(
    const std::vector<indexType>& idx, const IntensityData& intensity, ComponentCache& cache)
{
    if (cache.intensityStatsReady) {
        return;
    }
    IntensityStats stats;
    stats.minValue = intensity.at(idx[0]);
    stats.maxValue = stats.minValue;
    for (size_t k = 0; k < idx.size(); k++) {
        double value = intensity.at(idx[k]);
        stats.sum += value;
        stats.minValue = std::min(stats.minValue, value);
        stats.maxValue = std::max(stats.maxValue, value);
        stats.weightedX += (cache.cc[k] + 1) * value;
        stats.weightedY += (cache.rr[k] + 1) * value;
    }
    cache.intensityStats = stats;
    cache.intensityStatsReady = true;
}
//=============================================================================
static ArrayOf
pixelValues(const std::vector<indexType>& idx, const IntensityData& intensity)
{
    auto* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, idx.size()));
    for (size_t k = 0; k < idx.size(); k++) {
        ptr[k] = intensity.at(idx[k]);
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(idx.size(), 1), ptr);
}
//=============================================================================
static ArrayOf
fieldValue(const std::string& prop, const std::vector<indexType>& idx, const RegionData& data,
    ComponentCache& cache, const IntensityData& intensityData)
{
    if (prop == "Area") {
        return ArrayOf::doubleScalar(static_cast<double>(idx.size()));
    }
    if (prop == "PixelIdxList") {
        return pixelIdxList(idx);
    }
    if (prop == "PixelValues") {
        return pixelValues(idx, intensityData);
    }
    if (prop == "BoundingBox" || prop == "Image" || prop == "SubarrayIdx") {
        ensureBounds(idx, data, cache);
        if (prop == "BoundingBox") {
            return doubleRow4(cache.minCol + 0.5, cache.minRow + 0.5,
                static_cast<double>(cache.width), static_cast<double>(cache.height));
        }
        if (prop == "Image") {
            return regionImageFromIndices(
                idx, data.rows, cache.minRow, cache.minCol, cache.height, cache.width);
        }
        return rangeCell(cache.minRow, cache.maxRow, cache.minCol, cache.maxCol);
    }

    ensureGeometry(idx, data, cache);
    const std::vector<indexType>& rr = cache.rr;
    const std::vector<indexType>& cc = cache.cc;
    indexType minRow = cache.minRow;
    indexType minCol = cache.minCol;
    indexType height = cache.height;
    indexType width = cache.width;
    if (prop == "Centroid") {
        return doubleRow2(cache.sumX / idx.size(), cache.sumY / idx.size());
    }
    if (prop == "PixelList") {
        return pixelList(rr, cc);
    }
    if (prop == "Extent") {
        return ArrayOf::doubleScalar(static_cast<double>(idx.size()) / (height * width));
    }
    if (prop == "EquivDiameter") {
        return ArrayOf::doubleScalar(std::sqrt(4.0 * idx.size() / std::acos(-1)));
    }
    if (prop == "Perimeter") {
        return ArrayOf::doubleScalar(perimeterCount(rr, cc, minRow, minCol, height, width));
    }
    if (prop == "ConvexHull") {
        ensureConvexHull(cache);
        return convexHullArray(cache.convexHull);
    }
    if (prop == "ConvexImage") {
        ensureConvexMask(cache);
        return convexImage(cache);
    }
    if (prop == "ConvexArea" || prop == "Solidity") {
        ensureConvexMask(cache);
        double area = convexArea(cache);
        if (prop == "ConvexArea") {
            return ArrayOf::doubleScalar(area);
        }
        return ArrayOf::doubleScalar(static_cast<double>(idx.size()) / area);
    }
    if (prop == "Orientation" || prop == "MajorAxisLength" || prop == "MinorAxisLength"
        || prop == "Eccentricity") {
        ensureEllipseProps(cache);
        if (prop == "Orientation") {
            return ArrayOf::doubleScalar(cache.ellipse.orientation);
        }
        if (prop == "MajorAxisLength") {
            return ArrayOf::doubleScalar(cache.ellipse.majorAxisLength);
        }
        if (prop == "MinorAxisLength") {
            return ArrayOf::doubleScalar(cache.ellipse.minorAxisLength);
        }
        return ArrayOf::doubleScalar(cache.ellipse.eccentricity);
    }
    ensureIntensityStats(idx, intensityData, cache);
    const IntensityStats& stats = cache.intensityStats;
    if (prop == "MeanIntensity") {
        return ArrayOf::doubleScalar(stats.sum / idx.size());
    }
    if (prop == "MinIntensity") {
        return ArrayOf::doubleScalar(stats.minValue);
    }
    if (prop == "MaxIntensity") {
        return ArrayOf::doubleScalar(stats.maxValue);
    }
    if (stats.sum == 0) {
        double nan = std::numeric_limits<double>::quiet_NaN();
        return doubleRow({ nan, nan });
    }
    return doubleRow({ stats.weightedX / stats.sum, stats.weightedY / stats.sum });
}
//=============================================================================
static ArrayOf
emptyStats(const std::vector<std::string>& props)
{
    stringVector fields(props.begin(), props.end());
    Dimensions dims(0, 0);
    return ArrayOf::emptyStruct(fields, dims);
}
//=============================================================================
static ArrayOf
buildStats(const RegionData& data, const std::vector<std::string>& props, const ArrayOf& intensity)
{
    if (data.components.empty()) {
        return emptyStats(props);
    }
    stringVector fields(props.begin(), props.end());
    auto* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, data.components.size(), fields, false));
    ArrayOf result(
        NLS_STRUCT_ARRAY, Dimensions(1, data.components.size()), elements, false, fields);
    IntensityData intensityData = intensityDataFromArray(intensity);
    std::vector<ComponentCache> caches(data.components.size());
    indexType fieldCount = static_cast<indexType>(props.size());
    for (size_t k = 0; k < data.components.size(); k++) {
        indexType elementOffset = static_cast<indexType>(k) * fieldCount;
        for (indexType p = 0; p < fieldCount; p++) {
            elements[elementOffset + p]
                = fieldValue(props[p], data.components[k], data, caches[k], intensityData);
        }
    }
    return result;
}
//=============================================================================
ArrayOf
RegionProps(const ArrayOfVector& argIn)
{
    ArrayOf input = argIn[0];
    RegionData data = regionDataFromInput(input);
    ArrayOf intensity;
    size_t propStart = 1;
    if (argIn.size() >= 2 && argIn[1].isNumeric()
        && !(argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray())) {
        intensity = argIn[1];
        validate2DImage(intensity,
            formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:image_processing:imageDArray",
                                  L"%s must be a 2-D array."),
                L"Image"));
        if (intensity.rows() != data.rows || intensity.columns() != data.columns) {
            Error(L"Nelson:regionprops:intensitySizeMismatch", L"%s must have the same size as %s.",
                L"Intensity image", L"the region image");
        }
        propStart = 2;
    }
    std::vector<std::string> props = parseProperties(argIn, propStart, !intensity.isEmpty());
    for (const auto& prop : props) {
        if (needsIntensity(prop) && intensity.isEmpty()) {
            Error(L"Nelson:regionprops:intensityRequired", L"%s is required.", L"Intensity image");
        }
    }
    return buildStats(data, props, intensity);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
