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
#include <set>
#include <tuple>
#include <vector>
#include <QtGui/QImage>
#include "GOPropertyValues.hpp"
#include "RenderQt.hpp"
//=============================================================================
static int
clampColorComponent(double value)
{
    if (!std::isfinite(value)) {
        return 0;
    }
    value = std::max(0., std::min(1., value));
    return static_cast<int>(std::lround(value * 255.));
}
//=============================================================================
static QColor
toQColor(const RGBAColorData& color)
{
    return QColor(clampColorComponent(color.r), clampColorComponent(color.g),
        clampColorComponent(color.b), clampColorComponent(color.a));
}
//=============================================================================
using QtPatchPointKey = std::tuple<long long, long long, long long>;
using QtPatchEdgeKey = std::tuple<QtPatchPointKey, QtPatchPointKey>;
//=============================================================================
static long long
quantizeQtPatchCoordinate(double value)
{
    return static_cast<long long>(std::llround(value * 1e12));
}
//=============================================================================
static QtPatchPointKey
qtPatchPointKey(const point& value)
{
    return QtPatchPointKey(quantizeQtPatchCoordinate(value.x), quantizeQtPatchCoordinate(value.y),
        quantizeQtPatchCoordinate(value.z));
}
//=============================================================================
static bool
insertQtPatchEdge(std::set<QtPatchEdgeKey>& edges, const point& a, const point& b)
{
    QtPatchPointKey first = qtPatchPointKey(a);
    QtPatchPointKey second = qtPatchPointKey(b);
    if (second < first) {
        std::swap(first, second);
    }
    return edges.insert(QtPatchEdgeKey(first, second)).second;
}
//=============================================================================
void
RenderQt::setLightingScene(const RenderLightingScene& scene)
{
    lightingScene = scene;
}
//=============================================================================
void
RenderQt::setLightingMaterial(const RenderLightingMaterial& material)
{
    lightingMaterial = material;
}
//=============================================================================
static double
dot(const point& a, const point& b)
{
    return a.x * b.x + a.y * b.y + a.z * b.z;
}
//=============================================================================
static point
normalize(point value)
{
    double norm = std::sqrt(dot(value, value));
    if (!std::isfinite(norm) || norm <= 0) {
        return point(0, 0, 1);
    }
    value.x /= norm;
    value.y /= norm;
    value.z /= norm;
    return value;
}
//=============================================================================
static point
transformVector(const double* matrix, const point& value)
{
    return point(matrix[0] * value.x + matrix[4] * value.y + matrix[8] * value.z,
        matrix[1] * value.x + matrix[5] * value.y + matrix[9] * value.z,
        matrix[2] * value.x + matrix[6] * value.y + matrix[10] * value.z);
}
//=============================================================================
static point
transformDirection(const double* matrix, const point& value)
{
    point xAxis = normalize(point(matrix[0], matrix[1], matrix[2]));
    point yAxis = normalize(point(matrix[4], matrix[5], matrix[6]));
    point zAxis = normalize(point(matrix[8], matrix[9], matrix[10]));
    return point(xAxis.x * value.x + yAxis.x * value.y + zAxis.x * value.z,
        xAxis.y * value.x + yAxis.y * value.y + zAxis.y * value.z,
        xAxis.z * value.x + yAxis.z * value.y + zAxis.z * value.z);
}
//=============================================================================
static point
transformPoint(const double* matrix, const point& value)
{
    return point(matrix[0] * value.x + matrix[4] * value.y + matrix[8] * value.z + matrix[12],
        matrix[1] * value.x + matrix[5] * value.y + matrix[9] * value.z + matrix[13],
        matrix[2] * value.x + matrix[6] * value.y + matrix[10] * value.z + matrix[14]);
}
//=============================================================================
static point
transformNormal(const double* matrix, const point& value)
{
    double a00 = matrix[0];
    double a01 = matrix[4];
    double a02 = matrix[8];
    double a10 = matrix[1];
    double a11 = matrix[5];
    double a12 = matrix[9];
    double a20 = matrix[2];
    double a21 = matrix[6];
    double a22 = matrix[10];
    double c00 = a11 * a22 - a12 * a21;
    double c01 = a12 * a20 - a10 * a22;
    double c02 = a10 * a21 - a11 * a20;
    double c10 = a02 * a21 - a01 * a22;
    double c11 = a00 * a22 - a02 * a20;
    double c12 = a01 * a20 - a00 * a21;
    double c20 = a01 * a12 - a02 * a11;
    double c21 = a02 * a10 - a00 * a12;
    double c22 = a00 * a11 - a01 * a10;
    double determinant = a00 * c00 + a01 * c01 + a02 * c02;
    if (!std::isfinite(determinant) || std::abs(determinant) <= 1e-15) {
        return transformVector(matrix, value);
    }
    return point((c00 * value.x + c01 * value.y + c02 * value.z) / determinant,
        (c10 * value.x + c11 * value.y + c12 * value.z) / determinant,
        (c20 * value.x + c21 * value.y + c22 * value.z) / determinant);
}
//=============================================================================
RGBAColorData
RenderQt::applyLighting(
    const RGBAColorData& color, const point& normal, const point& vertex, bool enabled) const
{
    if (!enabled || !lightingScene.enabled || lightingScene.lights.empty()) {
        return color;
    }
    point vertexEye = transformPoint(model, vertex);
    point n = normalize(transformNormal(model, normal));
    point viewDirection = normalize(point(-vertexEye.x, -vertexEye.y, -vertexEye.z));
    // Two-sided lighting: faces whose normal points away from the viewer are lit
    // from their reverse side instead of collapsing to the ambient term.
    if (dot(n, viewDirection) < 0.) {
        n = point(-n.x, -n.y, -n.z);
    }
    double r = lightingMaterial.ambientStrength * lightingScene.ambientLightColor[0];
    double g = lightingMaterial.ambientStrength * lightingScene.ambientLightColor[1];
    double b = lightingMaterial.ambientStrength * lightingScene.ambientLightColor[2];
    double specularR = 0.;
    double specularG = 0.;
    double specularB = 0.;
    double shininess = std::max(0., lightingMaterial.specularExponent);
    for (const RenderLight& light : lightingScene.lights) {
        point lightPosition(light.position[0], light.position[1], light.position[2]);
        point direction;
        if (light.isLocal) {
            point lightEye = transformPoint(model, lightPosition);
            direction = point(
                lightEye.x - vertexEye.x, lightEye.y - vertexEye.y, lightEye.z - vertexEye.z);
        } else {
            direction = transformDirection(model, lightPosition);
        }
        direction = normalize(direction);
        double diffuse = std::max(0., dot(n, direction));
        r += lightingMaterial.diffuseStrength * diffuse * light.color[0];
        g += lightingMaterial.diffuseStrength * diffuse * light.color[1];
        b += lightingMaterial.diffuseStrength * diffuse * light.color[2];
        if (diffuse > 0. && lightingMaterial.specularStrength > 0.) {
            point reflected = normalize(point(2. * diffuse * n.x - direction.x,
                2. * diffuse * n.y - direction.y, 2. * diffuse * n.z - direction.z));
            double specular = std::pow(std::max(0., dot(reflected, viewDirection)), shininess)
                * lightingMaterial.specularStrength * lightingMaterial.specularColorReflectance;
            specularR += specular * light.color[0];
            specularG += specular * light.color[1];
            specularB += specular * light.color[2];
        }
    }
    return RGBAColorData(std::min(1., color.r * r + specularR),
        std::min(1., color.g * g + specularG), std::min(1., color.b * b + specularB), color.a);
}
//=============================================================================
static bool
isInsideTriangle(double w0, double w1, double w2)
{
    const double tolerance = -1e-8;
    return w0 >= tolerance && w1 >= tolerance && w2 >= tolerance;
}
//=============================================================================
void
RenderQt::drawInterpolatedTriangle(const QPointF& p0, const QPointF& p1, const QPointF& p2,
    const RGBAColorData& c0, const RGBAColorData& c1, const RGBAColorData& c2)
{
    const double denominator
        = (p1.y() - p2.y()) * (p0.x() - p2.x()) + (p2.x() - p1.x()) * (p0.y() - p2.y());
    if (std::abs(denominator) < 1e-12) {
        return;
    }

    int minX = static_cast<int>(std::floor(std::min({ p0.x(), p1.x(), p2.x() })));
    int maxX = static_cast<int>(std::ceil(std::max({ p0.x(), p1.x(), p2.x() })));
    int minY = static_cast<int>(std::floor(std::min({ p0.y(), p1.y(), p2.y() })));
    int maxY = static_cast<int>(std::ceil(std::max({ p0.y(), p1.y(), p2.y() })));
    minX = std::max(minX, 0);
    minY = std::max(minY, 0);
    maxX = std::min(maxX, static_cast<int>(std::ceil(m_width)));
    maxY = std::min(maxY, static_cast<int>(std::ceil(m_height)));

    const int imageWidth = maxX - minX + 1;
    const int imageHeight = maxY - minY + 1;
    if (imageWidth <= 0 || imageHeight <= 0) {
        return;
    }

    QImage image(imageWidth, imageHeight, QImage::Format_ARGB32);
    image.fill(Qt::transparent);
    for (int y = minY; y <= maxY; ++y) {
        QRgb* line = reinterpret_cast<QRgb*>(image.scanLine(y - minY));
        for (int x = minX; x <= maxX; ++x) {
            const double px = static_cast<double>(x) + 0.5;
            const double py = static_cast<double>(y) + 0.5;
            const double w0
                = ((p1.y() - p2.y()) * (px - p2.x()) + (p2.x() - p1.x()) * (py - p2.y()))
                / denominator;
            const double w1
                = ((p2.y() - p0.y()) * (px - p2.x()) + (p0.x() - p2.x()) * (py - p2.y()))
                / denominator;
            const double w2 = 1. - w0 - w1;
            if (!isInsideTriangle(w0, w1, w2)) {
                continue;
            }
            const double r = w0 * c0.r + w1 * c1.r + w2 * c2.r;
            const double g = w0 * c0.g + w1 * c1.g + w2 * c2.g;
            const double b = w0 * c0.b + w1 * c1.b + w2 * c2.b;
            const double a = w0 * c0.a + w1 * c1.a + w2 * c2.a;
            line[x - minX] = qRgba(clampColorComponent(r), clampColorComponent(g),
                clampColorComponent(b), clampColorComponent(a));
        }
    }
    pnt->drawImage(minX, minY, image);
}
//=============================================================================
void
RenderQt::drawTexturedTriangle(const QImage& tex, const QPointF& p0, const QPointF& p1,
    const QPointF& p2, const double uv0[2], const double uv1[2], const double uv2[2],
    const RGBAColorData& c0, const RGBAColorData& c1, const RGBAColorData& c2)
{
    const int texW = tex.width();
    const int texH = tex.height();
    if (texW <= 0 || texH <= 0) {
        return;
    }
    const double denominator
        = (p1.y() - p2.y()) * (p0.x() - p2.x()) + (p2.x() - p1.x()) * (p0.y() - p2.y());
    if (std::abs(denominator) < 1e-12) {
        return;
    }
    int minX = static_cast<int>(std::floor(std::min({ p0.x(), p1.x(), p2.x() })));
    int maxX = static_cast<int>(std::ceil(std::max({ p0.x(), p1.x(), p2.x() })));
    int minY = static_cast<int>(std::floor(std::min({ p0.y(), p1.y(), p2.y() })));
    int maxY = static_cast<int>(std::ceil(std::max({ p0.y(), p1.y(), p2.y() })));
    minX = std::max(minX, 0);
    minY = std::max(minY, 0);
    maxX = std::min(maxX, static_cast<int>(std::ceil(m_width)));
    maxY = std::min(maxY, static_cast<int>(std::ceil(m_height)));
    const int imageWidth = maxX - minX + 1;
    const int imageHeight = maxY - minY + 1;
    if (imageWidth <= 0 || imageHeight <= 0) {
        return;
    }
    // Bilinear texel fetch at normalized (u, v) in [0, 1].
    auto sampleTexel = [&tex, texW, texH](double u, double v, double& tr, double& tg, double& tb) {
        const double fx = std::clamp(u, 0.0, 1.0) * static_cast<double>(texW - 1);
        const double fy = std::clamp(v, 0.0, 1.0) * static_cast<double>(texH - 1);
        const int x0 = static_cast<int>(std::floor(fx));
        const int y0 = static_cast<int>(std::floor(fy));
        const int x1 = std::min(x0 + 1, texW - 1);
        const int y1 = std::min(y0 + 1, texH - 1);
        const double wx = fx - x0;
        const double wy = fy - y0;
        const QRgb p00 = tex.pixel(x0, y0);
        const QRgb p10 = tex.pixel(x1, y0);
        const QRgb p01 = tex.pixel(x0, y1);
        const QRgb p11 = tex.pixel(x1, y1);
        auto mix = [&](int c00, int c10, int c01, int c11) {
            const double top = c00 * (1.0 - wx) + c10 * wx;
            const double bottom = c01 * (1.0 - wx) + c11 * wx;
            return (top * (1.0 - wy) + bottom * wy) / 255.0;
        };
        tr = mix(qRed(p00), qRed(p10), qRed(p01), qRed(p11));
        tg = mix(qGreen(p00), qGreen(p10), qGreen(p01), qGreen(p11));
        tb = mix(qBlue(p00), qBlue(p10), qBlue(p01), qBlue(p11));
    };
    QImage image(imageWidth, imageHeight, QImage::Format_ARGB32);
    image.fill(Qt::transparent);
    for (int y = minY; y <= maxY; ++y) {
        QRgb* line = reinterpret_cast<QRgb*>(image.scanLine(y - minY));
        for (int x = minX; x <= maxX; ++x) {
            const double px = static_cast<double>(x) + 0.5;
            const double py = static_cast<double>(y) + 0.5;
            const double w0
                = ((p1.y() - p2.y()) * (px - p2.x()) + (p2.x() - p1.x()) * (py - p2.y()))
                / denominator;
            const double w1
                = ((p2.y() - p0.y()) * (px - p2.x()) + (p0.x() - p2.x()) * (py - p2.y()))
                / denominator;
            const double w2 = 1. - w0 - w1;
            if (!isInsideTriangle(w0, w1, w2)) {
                continue;
            }
            const double u = w0 * uv0[0] + w1 * uv1[0] + w2 * uv2[0];
            const double v = w0 * uv0[1] + w1 * uv1[1] + w2 * uv2[1];
            double tr, tg, tb;
            sampleTexel(u, v, tr, tg, tb);
            // The vertex colour carries baked lighting (white without lights), so it
            // modulates the texel; alpha comes from the vertices (FaceAlpha), never
            // the texel, whose alpha channel is meaningless for an indexed image.
            const double mr = w0 * c0.r + w1 * c1.r + w2 * c2.r;
            const double mg = w0 * c0.g + w1 * c1.g + w2 * c2.g;
            const double mb = w0 * c0.b + w1 * c1.b + w2 * c2.b;
            const double a = w0 * c0.a + w1 * c1.a + w2 * c2.a;
            line[x - minX] = qRgba(clampColorComponent(tr * mr), clampColorComponent(tg * mg),
                clampColorComponent(tb * mb), clampColorComponent(a));
        }
    }
    pnt->drawImage(minX, minY, image);
}
//=============================================================================
void
RenderQt::drawPatch(const FaceList& faces, const PatchRenderOptions& options)
{
    if (faces.size() == 0) {
        return;
    }
    bool useFaceGouraud = options.faceLighting == LightingMode::Gouraud;
    if (collectBspPrimitives) {
        std::set<QtPatchEdgeKey> bspEdges;
        for (const auto& face : faces) {
            if (face.vertices.size() < 3) {
                continue;
            }
            QColor fillColor(0, 0, 0, 0);
            if (face.FaceColorMode == ColorMode::ColorSpec) {
                point normal = face.normals.empty() ? point(0, 0, 1) : face.normals[0];
                point vertex = face.vertices.empty() ? point(0, 0, 0) : face.vertices[0];
                fillColor = toQColor(applyLighting(
                    face.FaceColor, normal, vertex, options.faceLighting != LightingMode::None));
            } else if ((face.FaceColorMode == ColorMode::Flat
                           || face.FaceColorMode == ColorMode::Interp)
                && !face.vertexcolors.empty()) {
                point normal = face.normals.empty() ? point(0, 0, 1) : face.normals[0];
                point vertex = face.vertices.empty() ? point(0, 0, 0) : face.vertices[0];
                fillColor = toQColor(applyLighting(face.vertexcolors[0], normal, vertex,
                    options.faceLighting != LightingMode::None));
            }
            if (face.FaceColorMode != ColorMode::None) {
                // FaceColor 'interp' interpolates the per-vertex colours across the
                // face like Gouraud lighting does, even when no light is present.
                // Otherwise the face collapses to vertexcolors[0].
                bool useInterpFill = useFaceGouraud
                    || (face.FaceColorMode == ColorMode::Interp
                        && face.vertexcolors.size() == face.vertices.size());
                std::vector<point> subPath;
                std::vector<RGBAColorData> subColors;
                auto appendFace = [&]() {
                    if (subPath.size() < 3) {
                        return;
                    }
                    if (useInterpFill && subColors.size() == subPath.size()) {
                        appendBspInterpolatedPolygon(
                            subPath, subColors, QColor(0, 0, 0, 0), 0., GO_PROPERTY_VALUE_NONE_STR);
                    } else {
                        appendBspPolygon(
                            subPath, fillColor, QColor(0, 0, 0, 0), 0., GO_PROPERTY_VALUE_NONE_STR);
                    }
                };
                for (size_t i = 0; i < face.vertices.size(); ++i) {
                    const auto& vertex = face.vertices[i];
                    if (std::isnan(vertex.x) || std::isnan(vertex.y) || std::isnan(vertex.z)) {
                        appendFace();
                        subPath.clear();
                        subColors.clear();
                        continue;
                    }
                    subPath.push_back(vertex);
                    if (useFaceGouraud) {
                        RGBAColorData baseColor = face.FaceColor;
                        if ((face.FaceColorMode == ColorMode::Flat
                                || face.FaceColorMode == ColorMode::Interp)
                            && !face.vertexcolors.empty()) {
                            size_t colorIndex = face.FaceColorMode == ColorMode::Interp
                                    && face.vertexcolors.size() == face.vertices.size()
                                ? i
                                : 0;
                            baseColor = face.vertexcolors[colorIndex];
                        }
                        point normal = i < face.normals.size()
                            ? face.normals[i]
                            : (face.normals.empty() ? point(0, 0, 1) : face.normals[0]);
                        subColors.push_back(applyLighting(baseColor, normal, vertex, true));
                    } else if (face.FaceColorMode == ColorMode::Interp
                        && face.vertexcolors.size() == face.vertices.size()) {
                        subColors.push_back(face.vertexcolors[i]);
                    }
                }
                appendFace();
            }
            if (options.lineStyle == GO_PROPERTY_VALUE_NONE_STR || options.lineWidth <= 0.
                || face.EdgeColorMode == ColorMode::None) {
                continue;
            }
            QColor edgeColor(0, 0, 0, 0);
            if (face.EdgeColorMode == ColorMode::ColorSpec) {
                point normal = face.normals.empty() ? point(0, 0, 1) : face.normals[0];
                point vertex = face.vertices.empty() ? point(0, 0, 0) : face.vertices[0];
                edgeColor = toQColor(applyLighting(
                    face.EdgeColor, normal, vertex, options.edgeLighting != LightingMode::None));
            } else if (!face.edgecolors.empty()) {
                point normal = face.normals.empty() ? point(0, 0, 1) : face.normals[0];
                point vertex = face.vertices.empty() ? point(0, 0, 0) : face.vertices[0];
                edgeColor = toQColor(applyLighting(face.edgecolors[0], normal, vertex,
                    options.edgeLighting != LightingMode::None));
            }
            std::vector<point> subPath;
            auto appendEdges = [&]() {
                if (subPath.size() < 2) {
                    return;
                }
                for (size_t i = 1; i < subPath.size(); ++i) {
                    if (options.uniqueEdges
                        && !insertQtPatchEdge(bspEdges, subPath[i - 1], subPath[i])) {
                        continue;
                    }
                    appendBspLine(subPath[i - 1], subPath[i], edgeColor, options.lineWidth,
                        options.lineStyle);
                }
                if (subPath.size() >= 3) {
                    if (options.uniqueEdges
                        && !insertQtPatchEdge(bspEdges, subPath.back(), subPath.front())) {
                        return;
                    }
                    appendBspLine(subPath.back(), subPath.front(), edgeColor, options.lineWidth,
                        options.lineStyle);
                }
            };
            for (const auto& vertex : face.vertices) {
                if (std::isnan(vertex.x) || std::isnan(vertex.y) || std::isnan(vertex.z)) {
                    appendEdges();
                    subPath.clear();
                    continue;
                }
                subPath.push_back(vertex);
            }
            appendEdges();
        }
        return;
    }
    bool noEdges = (options.lineStyle == GO_PROPERTY_VALUE_NONE_STR) || (options.lineWidth <= 0.);
    bool antialiasing = pnt->testRenderHint(QPainter::Antialiasing);
    if (noEdges && antialiasing) {
        pnt->setRenderHint(QPainter::Antialiasing, false);
    }

    const size_t kMaxVertices = 1000000;
    if (noEdges && !useFaceGouraud) {
        struct ColorPath
        {
            QColor color;
            QPainterPath path;
        };
        std::vector<ColorPath> paths;
        bool canUsePathFill = true;
        auto appendPath = [this](QPainterPath& path, const Face& face) {
            bool haveSubPath = false;
            bool haveDrawableSubPath = false;
            int pointCount = 0;
            auto closeSubPath = [&]() {
                if (haveSubPath && pointCount >= 3) {
                    path.closeSubpath();
                    haveDrawableSubPath = true;
                }
                haveSubPath = false;
                pointCount = 0;
            };
            for (const auto& v : face.vertices) {
                if (std::isnan(v.x) || std::isnan(v.y) || std::isnan(v.z)) {
                    closeSubPath();
                    continue;
                }
                QPointF pt = map(v.x, v.y, v.z);
                if (!std::isfinite(pt.x()) || !std::isfinite(pt.y())) {
                    closeSubPath();
                    continue;
                }
                if (!haveSubPath) {
                    path.moveTo(pt);
                    haveSubPath = true;
                    pointCount = 1;
                } else {
                    path.lineTo(pt);
                    ++pointCount;
                }
            }
            closeSubPath();
            return haveDrawableSubPath;
        };

        for (const auto& face : faces) {
            const size_t vcount = face.vertices.size();
            if (vcount < 3 || vcount > kMaxVertices || face.FaceColorMode == ColorMode::None) {
                continue;
            }
            QColor color;
            if (face.FaceColorMode == ColorMode::ColorSpec) {
                point normal = face.normals.empty() ? point(0, 0, 1) : face.normals[0];
                point vertex = face.vertices.empty() ? point(0, 0, 0) : face.vertices[0];
                color = toQColor(applyLighting(
                    face.FaceColor, normal, vertex, options.faceLighting != LightingMode::None));
            } else if (face.FaceColorMode == ColorMode::Flat && !face.vertexcolors.empty()) {
                point normal = face.normals.empty() ? point(0, 0, 1) : face.normals[0];
                point vertex = face.vertices.empty() ? point(0, 0, 0) : face.vertices[0];
                color = toQColor(applyLighting(face.vertexcolors[0], normal, vertex,
                    options.faceLighting != LightingMode::None));
            } else {
                canUsePathFill = false;
                break;
            }

            auto it = std::find_if(paths.begin(), paths.end(),
                [&](const ColorPath& entry) { return entry.color.rgba() == color.rgba(); });
            if (it == paths.end()) {
                ColorPath entry;
                entry.color = color;
                entry.path.setFillRule(Qt::OddEvenFill);
                paths.push_back(entry);
                it = paths.end() - 1;
            }
            appendPath(it->path, face);
        }
        if (canUsePathFill) {
            pnt->setPen(Qt::NoPen);
            for (const auto& entry : paths) {
                if (!entry.path.isEmpty()) {
                    pnt->fillPath(entry.path, entry.color);
                }
            }
            if (antialiasing) {
                pnt->setRenderHint(QPainter::Antialiasing, true);
            }
            return;
        }
    }

    for (const auto& face : faces) {
        const size_t vcount = face.vertices.size();
        if (vcount < 3 || vcount > kMaxVertices) {
            continue;
        }

        pnt->setBrush(Qt::NoBrush);
        QPen pen(pnt->pen());
        pen.setStyle(Qt::NoPen);
        pnt->setPen(pen);

        if (face.FaceColorMode == ColorMode::ColorSpec) {
            point normal = face.normals.empty() ? point(0, 0, 1) : face.normals[0];
            point vertex = face.vertices.empty() ? point(0, 0, 0) : face.vertices[0];
            pnt->setBrush(toQColor(applyLighting(
                face.FaceColor, normal, vertex, options.faceLighting != LightingMode::None)));
        }
        if ((face.FaceColorMode == ColorMode::Flat || face.FaceColorMode == ColorMode::Interp)
            && !face.vertexcolors.empty()) {
            point normal = face.normals.empty() ? point(0, 0, 1) : face.normals[0];
            point vertex = face.vertices.empty() ? point(0, 0, 0) : face.vertices[0];
            pnt->setBrush(toQColor(applyLighting(
                face.vertexcolors[0], normal, vertex, options.faceLighting != LightingMode::None)));
        }

        QPolygonF sub;
        sub.reserve(static_cast<int>(std::min<size_t>(vcount, 1024)));
        std::vector<RGBAColorData> subColors;
        subColors.reserve(static_cast<int>(std::min<size_t>(vcount, 1024)));
        bool useInterpolatedColors = useFaceGouraud
            || (face.FaceColorMode == ColorMode::Interp && face.vertexcolors.size() == vcount);
        auto drawFilledSubPolygon = [&]() {
            if (sub.size() < 3 || face.FaceColorMode == ColorMode::None) {
                return;
            }
            if (useInterpolatedColors && subColors.size() == static_cast<size_t>(sub.size())) {
                for (int k = 1; k < sub.size() - 1; ++k) {
                    drawInterpolatedTriangle(sub[0], sub[k], sub[k + 1], subColors[0],
                        subColors[static_cast<size_t>(k)], subColors[static_cast<size_t>(k + 1)]);
                }
            } else {
                pnt->drawPolygon(sub);
            }
        };

        for (size_t i = 0; i < vcount; ++i) {
            const auto& v = face.vertices[i];
            if (std::isnan(v.x) || std::isnan(v.y) || std::isnan(v.z)) {
                drawFilledSubPolygon();
                sub.clear();
                subColors.clear();
                continue;
            }

            QPointF pt = map(v.x, v.y, v.z);
            if (!std::isfinite(pt.x()) || !std::isfinite(pt.y())) {
                sub.clear();
                subColors.clear();
                continue;
            }
            sub.push_back(pt);
            if (useFaceGouraud) {
                RGBAColorData baseColor = face.FaceColor;
                if ((face.FaceColorMode == ColorMode::Flat
                        || face.FaceColorMode == ColorMode::Interp)
                    && !face.vertexcolors.empty()) {
                    size_t colorIndex = face.FaceColorMode == ColorMode::Interp
                            && face.vertexcolors.size() == vcount
                        ? i
                        : 0;
                    baseColor = face.vertexcolors[colorIndex];
                }
                point normal = i < face.normals.size()
                    ? face.normals[i]
                    : (face.normals.empty() ? point(0, 0, 1) : face.normals[0]);
                subColors.push_back(applyLighting(baseColor, normal, v, true));
            } else if (face.FaceColorMode == ColorMode::Interp
                && face.vertexcolors.size() == vcount) {
                subColors.push_back(face.vertexcolors[i]);
            }
        }
        drawFilledSubPolygon();
    }

    if (!noEdges) {
        std::set<QtPatchEdgeKey> edges;
        for (const auto& face : faces) {
            const size_t vcount = face.vertices.size();
            if (vcount < 3 || vcount > kMaxVertices || face.EdgeColorMode == ColorMode::None) {
                continue;
            }

            QPen pen;
            if (face.EdgeColorMode == ColorMode::ColorSpec) {
                point normal = face.normals.empty() ? point(0, 0, 1) : face.normals[0];
                point vertex = face.vertices.empty() ? point(0, 0, 0) : face.vertices[0];
                pen = QPen(toQColor(applyLighting(
                    face.EdgeColor, normal, vertex, options.edgeLighting != LightingMode::None)));
            } else if ((face.EdgeColorMode == ColorMode::Flat
                           || face.EdgeColorMode == ColorMode::Interp)
                && !face.edgecolors.empty()) {
                point normal = face.normals.empty() ? point(0, 0, 1) : face.normals[0];
                point vertex = face.vertices.empty() ? point(0, 0, 0) : face.vertices[0];
                pen = QPen(toQColor(applyLighting(face.edgecolors[0], normal, vertex,
                    options.edgeLighting != LightingMode::None)));
            } else {
                continue;
            }
            pen.setWidthF(options.lineWidth);
            setLineStyle(pen, options.lineStyle);
            if (options.lineJoin == GO_PROPERTY_VALUE_ROUND_STR) {
                pen.setJoinStyle(Qt::RoundJoin);
            } else if (options.lineJoin == L"miter") {
                pen.setJoinStyle(Qt::MiterJoin);
            } else if (options.lineJoin == L"chamfer") {
                pen.setJoinStyle(Qt::BevelJoin);
            }
            pnt->setPen(pen);
            pnt->setBrush(Qt::NoBrush);

            QPolygonF sub;
            std::vector<point> subPoints;
            sub.reserve(static_cast<int>(std::min<size_t>(vcount, 1024)));
            subPoints.reserve(static_cast<int>(std::min<size_t>(vcount, 1024)));
            auto drawEdgeSubPolygon = [&]() {
                if (sub.size() < 2) {
                    return;
                }
                if (!options.uniqueEdges && sub.size() >= 3) {
                    pnt->drawPolygon(sub);
                    return;
                }
                const int edgeCount = sub.size() == 2 ? 1 : sub.size();
                for (int i = 0; i < edgeCount; ++i) {
                    int j = (i + 1) % sub.size();
                    if (options.uniqueEdges
                        && !insertQtPatchEdge(edges, subPoints[i], subPoints[j])) {
                        continue;
                    }
                    pnt->drawLine(sub[i], sub[j]);
                }
            };
            for (const auto& v : face.vertices) {
                if (std::isnan(v.x) || std::isnan(v.y) || std::isnan(v.z)) {
                    drawEdgeSubPolygon();
                    sub.clear();
                    subPoints.clear();
                    continue;
                }
                QPointF pt = map(v.x, v.y, v.z);
                if (!std::isfinite(pt.x()) || !std::isfinite(pt.y())) {
                    sub.clear();
                    subPoints.clear();
                    continue;
                }
                sub.push_back(pt);
                subPoints.push_back(v);
            }
            drawEdgeSubPolygon();
        }
    }
    if (noEdges && antialiasing) {
        pnt->setRenderHint(QPainter::Antialiasing, true);
    }
}
//=============================================================================
