//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cmath>
#include <limits>
#include <memory>
#include "GOPropertyValues.hpp"
#include "ParallelSort.hpp"
#include "RenderQt.hpp"
//=============================================================================
namespace {
//=============================================================================
constexpr double BSP_EPSILON = 1e-9;
constexpr double BSP_CLIP_EPSILON = 1e-8;
constexpr size_t BSP_ROOT_CANDIDATE_LIMIT = 96;
constexpr size_t BSP_DEPTH_SORT_LIMIT = 4000;
//=============================================================================
struct BspPlane
{
    point normal;
    double d = 0.;
    bool valid = false;
};
//=============================================================================
struct BspNode
{
    RenderQtBspPrimitive root;
    std::vector<RenderQtBspPrimitive> coplanar;
    std::unique_ptr<BspNode> front;
    std::unique_ptr<BspNode> back;
    BspPlane plane;
};
//=============================================================================
double
dotPoint(const point& a, const point& b)
{
    return a.x * b.x + a.y * b.y + a.z * b.z;
}
//=============================================================================
point
subtractPoint(const point& a, const point& b)
{
    return point(a.x - b.x, a.y - b.y, a.z - b.z);
}
//=============================================================================
point
crossPoint(const point& a, const point& b)
{
    return point(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x);
}
//=============================================================================
point
normalizePoint(point value)
{
    double norm = std::sqrt(dotPoint(value, value));
    if (!std::isfinite(norm) || norm <= BSP_EPSILON) {
        return point(0, 0, 0);
    }
    value.x /= norm;
    value.y /= norm;
    value.z /= norm;
    return value;
}
//=============================================================================
BspPlane
planeForPrimitive(const RenderQtBspPrimitive& primitive)
{
    BspPlane plane;
    if (primitive.type != RenderQtBspPrimitiveType::Polygon || primitive.vertices.size() < 3) {
        return plane;
    }
    const point& origin = primitive.vertices[0].eye;
    for (size_t i = 1; i + 1 < primitive.vertices.size(); ++i) {
        point normal = normalizePoint(crossPoint(subtractPoint(primitive.vertices[i].eye, origin),
            subtractPoint(primitive.vertices[i + 1].eye, origin)));
        if (dotPoint(normal, normal) > BSP_EPSILON) {
            plane.normal = normal;
            plane.d = -dotPoint(normal, origin);
            plane.valid = true;
            return plane;
        }
    }
    return plane;
}
//=============================================================================
double
planeDistance(const BspPlane& plane, const RenderQtBspVertex& vertex)
{
    return dotPoint(plane.normal, vertex.eye) + plane.d;
}
//=============================================================================
RenderQtBspVertex
interpolateVertex(const RenderQtBspVertex& first, const RenderQtBspVertex& second, double t)
{
    t = std::clamp(t, 0., 1.);
    RenderQtBspVertex result;
    result.eye = point(first.eye.x + (second.eye.x - first.eye.x) * t,
        first.eye.y + (second.eye.y - first.eye.y) * t,
        first.eye.z + (second.eye.z - first.eye.z) * t);
    result.clip = point(first.clip.x + (second.clip.x - first.clip.x) * t,
        first.clip.y + (second.clip.y - first.clip.y) * t,
        first.clip.z + (second.clip.z - first.clip.z) * t);
    result.pixel = QPointF(first.pixel.x() + (second.pixel.x() - first.pixel.x()) * t,
        first.pixel.y() + (second.pixel.y() - first.pixel.y()) * t);
    if (first.hasColor && second.hasColor) {
        result.color = RGBAColorData(first.color.r + (second.color.r - first.color.r) * t,
            first.color.g + (second.color.g - first.color.g) * t,
            first.color.b + (second.color.b - first.color.b) * t,
            first.color.a + (second.color.a - first.color.a) * t);
        result.hasColor = true;
    }
    return result;
}
//=============================================================================
RenderQtBspVertex
intersectEdgeWithPlane(
    const BspPlane& plane, const RenderQtBspVertex& first, const RenderQtBspVertex& second)
{
    double firstDistance = planeDistance(plane, first);
    double secondDistance = planeDistance(plane, second);
    double denominator = firstDistance - secondDistance;
    double t = std::abs(denominator) <= BSP_EPSILON ? 0. : firstDistance / denominator;
    return interpolateVertex(first, second, t);
}
//=============================================================================
double
clipCoordinate(const RenderQtBspVertex& vertex, int axis)
{
    if (axis == 0) {
        return vertex.clip.x;
    }
    if (axis == 1) {
        return vertex.clip.y;
    }
    return vertex.clip.z;
}
//=============================================================================
bool
isInsideClipPlane(const RenderQtBspVertex& vertex, int axis, double limit, bool upper)
{
    double coordinate = clipCoordinate(vertex, axis);
    if (upper) {
        return coordinate <= limit + BSP_CLIP_EPSILON;
    }
    return coordinate >= limit - BSP_CLIP_EPSILON;
}
//=============================================================================
RenderQtBspVertex
intersectClipPlane(
    const RenderQtBspVertex& first, const RenderQtBspVertex& second, int axis, double limit)
{
    double firstCoordinate = clipCoordinate(first, axis);
    double secondCoordinate = clipCoordinate(second, axis);
    double denominator = secondCoordinate - firstCoordinate;
    double t = std::abs(denominator) <= BSP_EPSILON ? 0. : (limit - firstCoordinate) / denominator;
    return interpolateVertex(first, second, t);
}
//=============================================================================
std::vector<RenderQtBspVertex>
clipPolygonByPlane(const std::vector<RenderQtBspVertex>& input, int axis, double limit, bool upper)
{
    std::vector<RenderQtBspVertex> output;
    if (input.empty()) {
        return output;
    }
    output.reserve(input.size() + 1);
    RenderQtBspVertex previous = input.back();
    bool previousInside = isInsideClipPlane(previous, axis, limit, upper);
    for (const auto& current : input) {
        bool currentInside = isInsideClipPlane(current, axis, limit, upper);
        if (currentInside != previousInside) {
            output.push_back(intersectClipPlane(previous, current, axis, limit));
        }
        if (currentInside) {
            output.push_back(current);
        }
        previous = current;
        previousInside = currentInside;
    }
    return output;
}
//=============================================================================
std::vector<RenderQtBspVertex>
clipPolygonToViewVolume(std::vector<RenderQtBspVertex> vertices)
{
    for (int axis = 0; axis < 3; ++axis) {
        vertices = clipPolygonByPlane(vertices, axis, -1., false);
        vertices = clipPolygonByPlane(vertices, axis, 1., true);
        if (vertices.size() < 3) {
            return {};
        }
    }
    return vertices;
}
//=============================================================================
bool
updateLineClipRange(double start, double delta, double lower, double upper, double& t0, double& t1)
{
    if (std::abs(delta) <= BSP_EPSILON) {
        return start >= lower - BSP_CLIP_EPSILON && start <= upper + BSP_CLIP_EPSILON;
    }
    double tLower = (lower - start) / delta;
    double tUpper = (upper - start) / delta;
    if (tLower > tUpper) {
        std::swap(tLower, tUpper);
    }
    t0 = std::max(t0, tLower);
    t1 = std::min(t1, tUpper);
    return t0 <= t1 + BSP_CLIP_EPSILON;
}
//=============================================================================
bool
clipLineToViewVolume(std::vector<RenderQtBspVertex>& vertices)
{
    if (vertices.size() != 2) {
        return false;
    }
    const RenderQtBspVertex& first = vertices[0];
    const RenderQtBspVertex& second = vertices[1];
    double t0 = 0.;
    double t1 = 1.;
    if (!updateLineClipRange(first.clip.x, second.clip.x - first.clip.x, -1., 1., t0, t1)
        || !updateLineClipRange(first.clip.y, second.clip.y - first.clip.y, -1., 1., t0, t1)
        || !updateLineClipRange(first.clip.z, second.clip.z - first.clip.z, -1., 1., t0, t1)) {
        return false;
    }
    if (t1 < 0. || t0 > 1.) {
        return false;
    }
    t0 = std::clamp(t0, 0., 1.);
    t1 = std::clamp(t1, 0., 1.);
    if (t0 > t1) {
        return false;
    }
    vertices = { interpolateVertex(first, second, t0), interpolateVertex(first, second, t1) };
    return true;
}
//=============================================================================
bool
isFiniteVertex(const RenderQtBspVertex& vertex)
{
    return std::isfinite(vertex.eye.x) && std::isfinite(vertex.eye.y) && std::isfinite(vertex.eye.z)
        && std::isfinite(vertex.clip.x) && std::isfinite(vertex.clip.y)
        && std::isfinite(vertex.clip.z) && std::isfinite(vertex.pixel.x())
        && std::isfinite(vertex.pixel.y());
}
//=============================================================================
double
meanEyeZ(const std::vector<RenderQtBspVertex>& vertices)
{
    if (vertices.empty()) {
        return 0.;
    }
    double z = 0.;
    for (const auto& vertex : vertices) {
        z += vertex.eye.z;
    }
    return z / static_cast<double>(vertices.size());
}
//=============================================================================
bool
bspPrimitiveDepthLess(const RenderQtBspPrimitive& a, const RenderQtBspPrimitive& b)
{
    if (a.meanEyeZ == b.meanEyeZ) {
        return a.order < b.order;
    }
    return a.meanEyeZ < b.meanEyeZ;
}
//=============================================================================
RenderQtBspPrimitive
withVertices(RenderQtBspPrimitive primitive, std::vector<RenderQtBspVertex> vertices)
{
    primitive.vertices = std::move(vertices);
    primitive.meanEyeZ = meanEyeZ(primitive.vertices);
    return primitive;
}
//=============================================================================
void
splitPolygonByPlane(const BspPlane& plane, const RenderQtBspPrimitive& primitive,
    RenderQtBspPrimitive& front, RenderQtBspPrimitive& back)
{
    std::vector<RenderQtBspVertex> frontVertices;
    std::vector<RenderQtBspVertex> backVertices;
    const auto& vertices = primitive.vertices;
    for (size_t i = 0; i < vertices.size(); ++i) {
        const auto& current = vertices[i];
        const auto& next = vertices[(i + 1) % vertices.size()];
        double currentDistance = planeDistance(plane, current);
        double nextDistance = planeDistance(plane, next);
        bool currentFront = currentDistance > BSP_EPSILON;
        bool currentBack = currentDistance < -BSP_EPSILON;
        bool nextFront = nextDistance > BSP_EPSILON;
        bool nextBack = nextDistance < -BSP_EPSILON;
        if (!currentBack) {
            frontVertices.push_back(current);
        }
        if (!currentFront) {
            backVertices.push_back(current);
        }
        if ((currentFront && nextBack) || (currentBack && nextFront)) {
            RenderQtBspVertex intersection = intersectEdgeWithPlane(plane, current, next);
            frontVertices.push_back(intersection);
            backVertices.push_back(intersection);
        }
    }
    front = withVertices(primitive, std::move(frontVertices));
    back = withVertices(primitive, std::move(backVertices));
}
//=============================================================================
void
splitLineByPlane(const BspPlane& plane, const RenderQtBspPrimitive& primitive,
    RenderQtBspPrimitive& front, RenderQtBspPrimitive& back)
{
    const auto& first = primitive.vertices[0];
    const auto& second = primitive.vertices[1];
    double firstDistance = planeDistance(plane, first);
    RenderQtBspVertex intersection = intersectEdgeWithPlane(plane, first, second);
    if (firstDistance > 0.) {
        front = withVertices(primitive, { first, intersection });
        back = withVertices(primitive, { intersection, second });
    } else {
        front = withVertices(primitive, { intersection, second });
        back = withVertices(primitive, { first, intersection });
    }
}
//=============================================================================
enum class PrimitiveSide
{
    Back,
    Coplanar,
    Front,
    Split
};
//=============================================================================
PrimitiveSide
classifyPrimitive(const BspPlane& plane, const RenderQtBspPrimitive& primitive)
{
    bool hasFront = false;
    bool hasBack = false;
    for (const auto& vertex : primitive.vertices) {
        double distance = planeDistance(plane, vertex);
        hasFront = hasFront || distance > BSP_EPSILON;
        hasBack = hasBack || distance < -BSP_EPSILON;
    }
    if (hasFront && hasBack) {
        return PrimitiveSide::Split;
    }
    if (hasFront) {
        return PrimitiveSide::Front;
    }
    if (hasBack) {
        return PrimitiveSide::Back;
    }
    return PrimitiveSide::Coplanar;
}
//=============================================================================
double
projectedArea(const RenderQtBspPrimitive& primitive)
{
    if (primitive.vertices.size() < 3) {
        return 0.;
    }
    double area = 0.;
    for (size_t i = 0; i < primitive.vertices.size(); ++i) {
        const QPointF& a = primitive.vertices[i].pixel;
        const QPointF& b = primitive.vertices[(i + 1) % primitive.vertices.size()].pixel;
        area += a.x() * b.y() - b.x() * a.y();
    }
    return std::abs(area) * 0.5;
}
//=============================================================================
size_t
selectRootPrimitive(const std::vector<RenderQtBspPrimitive>& primitives)
{
    size_t best = 0;
    double bestScore = std::numeric_limits<double>::infinity();
    double bestArea = -1.;
    size_t candidateCount = 0;
    for (size_t i = 0; i < primitives.size(); ++i) {
        BspPlane plane = planeForPrimitive(primitives[i]);
        if (!plane.valid) {
            continue;
        }
        if (candidateCount++ >= BSP_ROOT_CANDIDATE_LIMIT) {
            break;
        }
        int front = 0;
        int back = 0;
        int splits = 0;
        for (size_t j = 0; j < primitives.size(); ++j) {
            if (i == j) {
                continue;
            }
            PrimitiveSide side = classifyPrimitive(plane, primitives[j]);
            if (side == PrimitiveSide::Front) {
                ++front;
            } else if (side == PrimitiveSide::Back) {
                ++back;
            } else if (side == PrimitiveSide::Split) {
                ++splits;
            }
        }
        double area = projectedArea(primitives[i]);
        double score = splits * 8. + std::abs(front - back);
        if (score < bestScore || (score == bestScore && area > bestArea)) {
            bestScore = score;
            bestArea = area;
            best = i;
        }
    }
    return best;
}
//=============================================================================
std::unique_ptr<BspNode>
buildBspTree(std::vector<RenderQtBspPrimitive> primitives)
{
    if (primitives.empty()) {
        return nullptr;
    }
    size_t rootIndex = selectRootPrimitive(primitives);
    auto node = std::make_unique<BspNode>();
    node->root = primitives[rootIndex];
    node->plane = planeForPrimitive(node->root);
    primitives.erase(primitives.begin() + static_cast<std::ptrdiff_t>(rootIndex));
    if (!node->plane.valid) {
        Nelson::parallelSort(primitives, bspPrimitiveDepthLess);
        node->coplanar = std::move(primitives);
        return node;
    }

    std::vector<RenderQtBspPrimitive> front;
    std::vector<RenderQtBspPrimitive> back;
    for (const auto& primitive : primitives) {
        PrimitiveSide side = classifyPrimitive(node->plane, primitive);
        if (side == PrimitiveSide::Front) {
            front.push_back(primitive);
        } else if (side == PrimitiveSide::Back) {
            back.push_back(primitive);
        } else if (side == PrimitiveSide::Coplanar) {
            node->coplanar.push_back(primitive);
        } else if (primitive.type == RenderQtBspPrimitiveType::Polygon) {
            RenderQtBspPrimitive frontPart;
            RenderQtBspPrimitive backPart;
            splitPolygonByPlane(node->plane, primitive, frontPart, backPart);
            if (frontPart.vertices.size() >= 3) {
                front.push_back(frontPart);
            }
            if (backPart.vertices.size() >= 3) {
                back.push_back(backPart);
            }
        } else if (primitive.vertices.size() == 2) {
            RenderQtBspPrimitive frontPart;
            RenderQtBspPrimitive backPart;
            splitLineByPlane(node->plane, primitive, frontPart, backPart);
            front.push_back(frontPart);
            back.push_back(backPart);
        }
    }
    std::sort(node->coplanar.begin(), node->coplanar.end(),
        [](const auto& a, const auto& b) { return a.order < b.order; });
    node->front = buildBspTree(std::move(front));
    node->back = buildBspTree(std::move(back));
    return node;
}
//=============================================================================
template <typename DrawFunction>
void
drawCoplanar(const BspNode* node, DrawFunction draw)
{
    bool rootDrawn = false;
    for (const auto& primitive : node->coplanar) {
        if (!rootDrawn && node->root.order < primitive.order) {
            draw(node->root);
            rootDrawn = true;
        }
        draw(primitive);
    }
    if (!rootDrawn) {
        draw(node->root);
    }
}
//=============================================================================
template <typename DrawFunction>
void
traverseBsp(const BspNode* node, DrawFunction draw)
{
    if (!node) {
        return;
    }
    double cameraSide = node->plane.valid ? node->plane.d : -1.;
    if (cameraSide > BSP_EPSILON) {
        traverseBsp(node->back.get(), draw);
        drawCoplanar(node, draw);
        traverseBsp(node->front.get(), draw);
    } else {
        traverseBsp(node->front.get(), draw);
        drawCoplanar(node, draw);
        traverseBsp(node->back.get(), draw);
    }
}
//=============================================================================
} // namespace
//=============================================================================
RenderQtBspVertex
RenderQt::bspVertex(double x, double y, double z, bool& clipped)
{
    RenderQtBspVertex vertex;
    double xclip = 0.;
    double yclip = 0.;
    double zclip = 0.;
    toPixelsImpl(x, y, z, xclip, yclip, zclip, clipped);
    double a = viewp[0] + (1 + xclip) / 2.0 * viewp[2];
    double b = viewp[1] + (1 + yclip) / 2.0 * viewp[3];
    vertex.clip = point(xclip, yclip, zclip);
    vertex.pixel = QPointF(a, m_height - 1 - b);
    mapPoint(x, y, z, vertex.eye.x, vertex.eye.y, vertex.eye.z);
    return vertex;
}
//=============================================================================
void
RenderQt::appendBspPrimitive(RenderQtBspPrimitive primitive)
{
    primitive.order = bspPrimitiveOrder++;
    primitive.meanEyeZ = meanEyeZ(primitive.vertices);
    bspPrimitives.push_back(std::move(primitive));
}
//=============================================================================
void
RenderQt::appendBspPolygon(const std::vector<point>& vertices, const QColor& fillColor,
    const QColor& lineColor, double lineWidth, const std::wstring& lineStyle)
{
    if (!collectBspPrimitives || vertices.size() < 3) {
        return;
    }
    bool drawOutline
        = lineStyle != GO_PROPERTY_VALUE_NONE_STR && lineWidth > 0. && lineColor.alpha() != 0;
    RenderQtBspPrimitive primitive;
    primitive.type = RenderQtBspPrimitiveType::Polygon;
    primitive.fillColor = fillColor;
    primitive.lineColor = QColor(0, 0, 0, 0);
    primitive.lineWidth = 0.;
    primitive.lineStyle = GO_PROPERTY_VALUE_NONE_STR;
    primitive.closed = true;
    primitive.vertices.reserve(vertices.size());
    for (const point& vertex : vertices) {
        bool clipped = false;
        RenderQtBspVertex mapped = bspVertex(vertex.x, vertex.y, vertex.z, clipped);
        if (!isFiniteVertex(mapped)) {
            return;
        }
        primitive.vertices.push_back(mapped);
    }
    primitive.vertices = clipPolygonToViewVolume(std::move(primitive.vertices));
    if (primitive.vertices.size() < 3 || projectedArea(primitive) <= BSP_EPSILON) {
        return;
    }
    appendBspPrimitive(std::move(primitive));
    if (!drawOutline) {
        return;
    }
    for (size_t i = 0; i < vertices.size(); ++i) {
        appendBspLine(
            vertices[i], vertices[(i + 1) % vertices.size()], lineColor, lineWidth, lineStyle);
    }
}
//=============================================================================
void
RenderQt::appendBspInterpolatedPolygon(const std::vector<point>& vertices,
    const std::vector<RGBAColorData>& vertexColors, const QColor& lineColor, double lineWidth,
    const std::wstring& lineStyle)
{
    if (!collectBspPrimitives || vertices.size() < 3 || vertexColors.size() != vertices.size()) {
        return;
    }
    bool drawOutline
        = lineStyle != GO_PROPERTY_VALUE_NONE_STR && lineWidth > 0. && lineColor.alpha() != 0;
    RenderQtBspPrimitive primitive;
    primitive.type = RenderQtBspPrimitiveType::Polygon;
    primitive.fillColor = QColor(0, 0, 0, 0);
    primitive.lineColor = QColor(0, 0, 0, 0);
    primitive.lineWidth = 0.;
    primitive.lineStyle = GO_PROPERTY_VALUE_NONE_STR;
    primitive.closed = true;
    primitive.interpolateFill = true;
    primitive.vertices.reserve(vertices.size());
    for (size_t i = 0; i < vertices.size(); ++i) {
        bool clipped = false;
        RenderQtBspVertex mapped = bspVertex(vertices[i].x, vertices[i].y, vertices[i].z, clipped);
        if (!isFiniteVertex(mapped)) {
            return;
        }
        mapped.color = vertexColors[i];
        mapped.hasColor = true;
        primitive.vertices.push_back(mapped);
    }
    primitive.vertices = clipPolygonToViewVolume(std::move(primitive.vertices));
    if (primitive.vertices.size() < 3 || projectedArea(primitive) <= BSP_EPSILON) {
        return;
    }
    appendBspPrimitive(std::move(primitive));
    if (!drawOutline) {
        return;
    }
    for (size_t i = 0; i < vertices.size(); ++i) {
        appendBspLine(
            vertices[i], vertices[(i + 1) % vertices.size()], lineColor, lineWidth, lineStyle);
    }
}
//=============================================================================
void
RenderQt::appendBspLine(const point& first, const point& second, const QColor& color,
    double lineWidth, const std::wstring& lineStyle)
{
    if (!collectBspPrimitives || lineStyle == GO_PROPERTY_VALUE_NONE_STR || lineWidth <= 0.) {
        return;
    }
    bool firstClipped = false;
    bool secondClipped = false;
    RenderQtBspPrimitive primitive;
    primitive.type = RenderQtBspPrimitiveType::Polyline;
    primitive.lineColor = color;
    primitive.lineWidth = lineWidth;
    primitive.lineStyle = lineStyle;
    primitive.closed = false;
    primitive.vertices = { bspVertex(first.x, first.y, first.z, firstClipped),
        bspVertex(second.x, second.y, second.z, secondClipped) };
    if (!isFiniteVertex(primitive.vertices[0]) || !isFiniteVertex(primitive.vertices[1])
        || !clipLineToViewVolume(primitive.vertices)) {
        return;
    }
    appendBspPrimitive(std::move(primitive));
}
//=============================================================================
void
RenderQt::drawBspPrimitive(const RenderQtBspPrimitive& primitive)
{
    if (primitive.vertices.empty()) {
        return;
    }
    QPen pen(primitive.lineColor);
    setLineStyle(pen, primitive.lineStyle);
    // primitive.lineWidth is already a device pixel width: general lines arrive
    // through currentLineWidth (floored by lineWidth()) and surface edges are
    // converted with edgeStrokePixelWidth() before they are appended. Draw it as
    // given so the two sources are not scaled twice.
    pen.setWidthF(primitive.lineWidth);

    QPolygonF polygon;
    polygon.reserve(static_cast<int>(primitive.vertices.size()));
    for (const auto& vertex : primitive.vertices) {
        polygon.push_back(vertex.pixel);
    }
    if (primitive.type == RenderQtBspPrimitiveType::Polygon) {
        bool interpolatedFill = primitive.interpolateFill;
        for (const auto& vertex : primitive.vertices) {
            interpolatedFill = interpolatedFill && vertex.hasColor;
        }
        if (interpolatedFill) {
            pnt->setPen(Qt::NoPen);
            for (int k = 1; k < polygon.size() - 1; ++k) {
                drawInterpolatedTriangle(polygon[0], polygon[k], polygon[k + 1],
                    primitive.vertices[0].color, primitive.vertices[static_cast<size_t>(k)].color,
                    primitive.vertices[static_cast<size_t>(k + 1)].color);
            }
        } else {
            pnt->setBrush(QBrush(primitive.fillColor));
            if (primitive.lineStyle == GO_PROPERTY_VALUE_NONE_STR || primitive.lineWidth <= 0.
                || primitive.lineColor.alpha() == 0) {
                pnt->setPen(Qt::NoPen);
            } else {
                pnt->setPen(pen);
            }
            pnt->drawPolygon(polygon);
        }
        if (interpolatedFill && primitive.lineStyle != GO_PROPERTY_VALUE_NONE_STR
            && primitive.lineWidth > 0. && primitive.lineColor.alpha() != 0) {
            pnt->setBrush(Qt::NoBrush);
            pnt->setPen(pen);
            pnt->drawPolygon(polygon);
        }
    } else {
        pnt->setBrush(Qt::NoBrush);
        pnt->setPen(pen);
        QPainterPath path;
        path.moveTo(polygon[0]);
        for (int i = 1; i < polygon.size(); ++i) {
            path.lineTo(polygon[i]);
        }
        pnt->drawPath(path);
    }
}
//=============================================================================
void
RenderQt::flushBspPrimitives()
{
    if (bspPrimitives.empty()) {
        return;
    }
    if (bspPrimitives.size() > BSP_DEPTH_SORT_LIMIT) {
        Nelson::parallelSort(bspPrimitives, bspPrimitiveDepthLess);
        for (const auto& primitive : bspPrimitives) {
            drawBspPrimitive(primitive);
        }
        bspPrimitives.clear();
        return;
    }
    auto tree = buildBspTree(std::move(bspPrimitives));
    bspPrimitives.clear();
    traverseBsp(tree.get(), [this](const auto& primitive) { drawBspPrimitive(primitive); });
}
//=============================================================================
