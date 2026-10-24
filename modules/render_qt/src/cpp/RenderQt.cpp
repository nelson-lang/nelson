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
#include <cmath>
#include <QtGui/QGuiApplication>
#include <QtGui/QScreen>
#include <algorithm>
#include <QtGui/QPainterPath>
#include <QtGui/QTransform>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Types.hpp"
#include "GOPropertyValues.hpp"
#include "RenderQt.hpp"
#include "RenderQtHelpers.hpp"
#include "RenderQtPolyline.hpp"
#include "characters_encoding.hpp"
#include "QStringConverter.hpp"
#include "ParallelSort.hpp"
//=============================================================================
// The OMP_PARALLEL_FOR_LOOP macro (omp_for_loop.hpp) references
// NelsonConfiguration unqualified; make the Nelson namespace visible here.
using namespace Nelson;
//=============================================================================
// Screen-dpi points->pixels used by the Qt backends (kept local so the render
// module does not import the graphics DLL: the link goes the other way).
static double
qtScreenPointsToPixels(double points)
{
    QScreen* screen = QGuiApplication::primaryScreen();
    double dpi = screen ? screen->logicalDotsPerInch() : 96.0;
    return points * (dpi / 72.0);
}
//=============================================================================
RenderQt::~RenderQt() = default;
//=============================================================================
RenderQt::RenderQt(QPainter* painter, double x1, double y1, double width, double height,
    const std::wstring& _renderName)
{
    pnt = painter;
    pnt->setRenderHint(QPainter::TextAntialiasing);
    m_x1 = x1;
    m_y1 = y1;
    m_width = width;
    m_height = height;
    inDirect = false;
    pnt->setPen(QColor(0, 0, 0));
    pnt->setBrush(QColor(0, 0, 0));
    debugFlag = false;
    deferSurfaceQuads = false;
    surfaceGroupDepth = 0;
    collectBspPrimitives = false;
    savedCollectBspPrimitives = false;
    bspPrimitiveOrder = 0;
    currentLineWidth = 1.;
    currentLineStyle = L"-";
    renderName = _renderName;
}
//=============================================================================
static void
sortQuadsByProjectedDepth(std::vector<quad3D>& quads)
{
    Nelson::parallelSort(quads);
}
//=============================================================================
std::wstring
RenderQt::getRenderName()
{
    return renderName;
}
//=============================================================================
bool
RenderQt::getGraphicsSmoothing()
{
    return pnt->testRenderHint(QPainter::Antialiasing);
}
//=============================================================================
void
RenderQt::setGraphicsSmoothing(bool on)
{
    pnt->setRenderHint(QPainter::Antialiasing, on);
}
//=============================================================================
void
RenderQt::tri(double x1, double y1, double z1, double x2, double y2, double z2, double x3,
    double y3, double z3)
{
    if (collectBspPrimitives) {
        appendBspPolygon({ point(x1, y1, z1), point(x2, y2, z2), point(x3, y3, z3) },
            pnt->brush().color(), pnt->pen().color(), currentLineWidth, currentLineStyle);
        return;
    }
    QPolygonF poly;
    poly.reserve(3);
    poly.push_back(map(x1, y1, z1));
    poly.push_back(map(x2, y2, z2));
    poly.push_back(map(x3, y3, z3));
    pnt->drawPolygon(poly);
}
//=============================================================================
bool
RenderQt::polygonFill(
    const std::vector<std::vector<double>>& points, const std::vector<double>& faceColor)
{
    if (points.size() < 3 || faceColor.size() < 3) {
        return false;
    }
    color(faceColor);
    QPolygonF poly;
    poly.reserve(static_cast<int>(points.size()));
    for (const auto& point : points) {
        poly.push_back(map(point[0], point[1], 0));
    }
    pnt->drawPolygon(poly);
    return true;
}
//=============================================================================
void
RenderQt::triLine(double x1, double y1, double z1, double x2, double y2, double z2, double x3,
    double y3, double z3)
{
    if (collectBspPrimitives) {
        QColor color = pnt->pen().color();
        appendBspLine(
            point(x1, y1, z1), point(x2, y2, z2), color, currentLineWidth, currentLineStyle);
        appendBspLine(
            point(x2, y2, z2), point(x3, y3, z3), color, currentLineWidth, currentLineStyle);
        appendBspLine(
            point(x3, y3, z3), point(x1, y1, z1), color, currentLineWidth, currentLineStyle);
        return;
    }
    QPolygonF poly;
    poly.reserve(3);
    poly.push_back(map(x1, y1, z1));
    poly.push_back(map(x2, y2, z2));
    poly.push_back(map(x3, y3, z3));
    pnt->drawPolyline(poly);
}
//=============================================================================
void
RenderQt::color(const std::vector<double> col)
{
    QPen pen(pnt->pen());
    QBrush brush(pnt->brush());

    int R = (int)(col[0] * 255);
    int G = (int)(col[1] * 255);
    int B = (int)(col[2] * 255);

    QColor color = QColor::fromRgb(R, G, B);
    if (col.size() == 4) {
        int A = (int)(col[3] * 255);
        color.setAlpha(A);
    }

    pen.setColor(color);
    brush.setColor(color);
    pnt->setPen(pen);
    QColor C = pen.color();
    int r, g, b;
    C.getRgb(&r, &g, &b);
    pnt->setBrush(brush);
}
//=============================================================================
void
RenderQt::setLineStyle(QPen& _pen, const std::wstring& style)
{
    // Dash patterns are in units of the pen width (Qt multiplies them by the width,
    // like RenderWeb::dashArray). Dashed and dash-dot both have a period of about 8:
    // dashed is 4 + 4, dash-dot is dash, gap, dot, gap (4 + 1 + 2 + 1). The dotted
    // style keeps Qt's short dot.
    if (style == L"-") {
        _pen.setStyle(Qt::SolidLine);
    } else if (style == L"--") {
        _pen.setDashPattern(QVector<qreal> { 4.0, 4.0 });
    } else if (style == L":") {
        _pen.setStyle(Qt::DotLine);
    } else if (style == L"-.") {
        _pen.setDashPattern(QVector<qreal> { 4.0, 1.0, 2.0, 1.0 });
    } else if (style == GO_PROPERTY_VALUE_NONE_STR) {
        _pen.setStyle(Qt::NoPen);
    }
}
//=============================================================================
void
RenderQt::setLineStyle(const std::wstring& style)
{
    currentLineStyle = style;
    QPen pen(pnt->pen());
    setLineStyle(pen, style);
    pnt->setPen(pen);
}
//=============================================================================
void
RenderQt::lineWidth(double n)
{
    QPen pen(pnt->pen());
    pen.setWidthF(edgeStrokePixelWidth(n));
    currentLineWidth = pen.widthF();
    pnt->setPen(pen);
}
//=============================================================================
double
RenderQt::edgeStrokePixelWidth(double points) const
{
    return std::max(1.0, qtScreenPointsToPixels(points));
}
//=============================================================================
double
RenderQt::pointsToPixels(double points)
{
    if (renderName == L"Image" || renderName == L"SVG") {
        return points * (150.0 / 72.0);
    }
    return qtScreenPointsToPixels(points);
}
//=============================================================================
void
RenderQt::line(double x1, double y1, double z1, double x2, double y2, double z2)
{
    // Direct draw uses pixel coordinates while the clip cuboid is in axes data
    // coordinates, so the cuboid test is skipped (RenderOpenGL pauses its clip planes
    // for the same reason). Marker centers outside the box are dropped by the
    // toPixels() clipped flag, and the painter clip rect trims the overhang.
    if (!inDirect && m_dataClip.enabled
        && !renderClipSegment(m_dataClip, x1, y1, z1, x2, y2, z2, x1, y1, z1, x2, y2, z2)) {
        return;
    }
    if (collectBspPrimitives) {
        appendBspLine(point(x1, y1, z1), point(x2, y2, z2), pnt->pen().color(), currentLineWidth,
            currentLineStyle);
        return;
    }
    pnt->drawLine(map(x1, y1, z1), map(x2, y2, z2));
}
//=============================================================================
void
RenderQt::line(double x1, double y1, double x2, double y2)
{
    double z1 = 0.0, z2 = 0.0;
    if (!inDirect && m_dataClip.enabled
        && !renderClipSegment(m_dataClip, x1, y1, z1, x2, y2, z2, x1, y1, z1, x2, y2, z2)) {
        return;
    }
    if (collectBspPrimitives) {
        appendBspLine(point(x1, y1, 0), point(x2, y2, 0), pnt->pen().color(), currentLineWidth,
            currentLineStyle);
        return;
    }
    pnt->drawLine(map(x1, y1, 0), map(x2, y2, 0));
}
//=============================================================================
void
RenderQt::lineSeries(std::vector<double> xs, std::vector<double> ys, std::vector<double> zs)
{
    const size_t pointCount = std::min(xs.size(), ys.size());
    if (pointCount < 2) {
        return;
    }
    size_t index = 0;
    while (index < pointCount) {
        while (index < pointCount) {
            double z = index < zs.size() ? zs[index] : 0;
            if (std::isfinite(xs[index]) && std::isfinite(ys[index]) && std::isfinite(z)) {
                break;
            }
            index++;
        }
        std::vector<double> localXs;
        std::vector<double> localYs;
        std::vector<double> localZs;
        localXs.reserve(pointCount - index);
        localYs.reserve(pointCount - index);
        localZs.reserve(pointCount - index);
        while (index < pointCount) {
            double z = index < zs.size() ? zs[index] : 0;
            if (!(std::isfinite(xs[index]) && std::isfinite(ys[index]) && std::isfinite(z))) {
                break;
            }
            localXs.push_back(xs[index]);
            localYs.push_back(ys[index]);
            localZs.push_back(z);
            index++;
        }
        if (localXs.size() >= 2) {
            // Per-segment clipping only when the polyline leaves the axes box.
            const bool clipThis = renderClipNeeded(m_dataClip, localXs, localYs, localZs);
            if (collectBspPrimitives) {
                for (size_t k = 1; k < localXs.size(); ++k) {
                    double ax0 = localXs[k - 1], ay0 = localYs[k - 1], az0 = localZs[k - 1];
                    double ax1 = localXs[k], ay1 = localYs[k], az1 = localZs[k];
                    if (clipThis
                        && !renderClipSegment(m_dataClip, ax0, ay0, az0, ax1, ay1, az1, ax0, ay0,
                            az0, ax1, ay1, az1)) {
                        continue;
                    }
                    appendBspLine(point(ax0, ay0, az0), point(ax1, ay1, az1), pnt->pen().color(),
                        currentLineWidth, currentLineStyle);
                }
                continue;
            }
            if (clipThis) {
                for (size_t k = 1; k < localXs.size(); ++k) {
                    double ax0 = localXs[k - 1], ay0 = localYs[k - 1], az0 = localZs[k - 1];
                    double ax1 = localXs[k], ay1 = localYs[k], az1 = localZs[k];
                    if (!renderClipSegment(m_dataClip, ax0, ay0, az0, ax1, ay1, az1, ax0, ay0, az0,
                            ax1, ay1, az1)) {
                        continue;
                    }
                    pnt->drawLine(map(ax0, ay0, az0), map(ax1, ay1, az1));
                }
                continue;
            }
            strokePolyline(pnt, map(localXs, localYs, localZs));
        }
    }
}
//=============================================================================
void
RenderQt::debug()
{
    debugFlag = !debugFlag;
    return;
}
//=============================================================================
void
RenderQt::setupDirectDraw()
{
    if (collectBspPrimitives) {
        flushBspPrimitives();
    }
    savedCollectBspPrimitives = collectBspPrimitives;
    collectBspPrimitives = false;
    pnt->save();
    for (int i = 0; i < 16; i++) {
        save_model[i] = model[i];
        model[i] = 0;
        save_proj[i] = proj[i];
    }
    for (int i = 0; i < 4; i++) {
        save_viewp[i] = viewp[i];
    }
    model[0] = 1;
    model[5] = 1;
    model[10] = 1;
    model[15] = 1;
    viewport(m_x1, m_y1, m_width, m_height);
    project(m_x1, m_x1 + m_width, m_y1, m_y1 + m_height, -1, 1);
    inDirect = true;
}
//=============================================================================
void
RenderQt::releaseDirectDraw()
{
    pnt->restore();
    for (int i = 0; i < 16; i++) {
        model[i] = save_model[i];
        proj[i] = save_proj[i];
    }
    for (int i = 0; i < 4; i++) {
        viewp[i] = save_viewp[i];
    }
    inDirect = false;
    collectBspPrimitives = savedCollectBspPrimitives;
}
//=============================================================================
void
RenderQt::setClipBox(double x0, double y0, double width, double height)
{
    pnt->setClipRect(static_cast<int>(std::floor(x0)),
        static_cast<int>(std::floor(m_height - (y0 + height))), static_cast<int>(std::ceil(width)),
        static_cast<int>(std::ceil(height)));
    clipToRegion();
}
//=============================================================================
void
RenderQt::clearClipBox()
{
    if (!regions.empty()) {
        pnt->setClipRect(QRectF(0, 0, m_width, m_height), Qt::ReplaceClip);
        return;
    }
    pnt->setClipping(false);
}
//=============================================================================
void
RenderQt::setDataClipBox(const RenderClipBox& box)
{
    m_dataClip = box;
}
//=============================================================================
void
RenderQt::clearDataClipBox()
{
    m_dataClip.enabled = false;
}
//=============================================================================
void
RenderQt::getModelviewMatrix(std::vector<double>& amodel)
{
    amodel.clear();
    amodel.reserve(16);
    amodel.insert(amodel.begin(), std::begin(model), std::end(model));
}
//=============================================================================
QPointF
RenderQt::map(double x, double y, double z)
{
    double a, b;
    toPixels(x, y, z, a, b);
    b = m_height - 1 - b;
    return { rint(a), rint(b) };
}
//=============================================================================
QVector<QPointF>
RenderQt::map(
    const std::vector<double>& xs, const std::vector<double>& ys, const std::vector<double>& zs)
{
    QVector<QPointF> retval;
    retval.resize(xs.size());
    OMP_PARALLEL_FOR_LOOP(xs.size())
    for (Nelson::ompIndexType i = 0; i < (Nelson::ompIndexType)xs.size(); i++) {
        retval[i] = map(xs[i], ys[i], zs[i]);
    }
    return retval;
}
//=============================================================================
void
RenderQt::depth(bool)
{
}
//=============================================================================
void
RenderQt::rect(double x1, double y1, double x2, double y2)
{
    quadline(x1, y1, 0, x2, y1, 0, x2, y2, 0, x1, y2, 0);
}
//=============================================================================
void
RenderQt::rectFill(double x1, double y1, double x2, double y2)
{
    quad(x1, y1, 0, x2, y1, 0, x2, y2, 0, x1, y2, 0);
}
//=============================================================================
void
RenderQt::circle(double x1, double y1, double radius)
{
    QPointF uleft(map(x1 - radius, y1 + radius, 0));
    QPointF lright(map(x1 + radius, y1 - radius, 0));
    QRectF rect;
    rect.setBottomRight(lright);
    rect.setTopLeft(uleft);
    QBrush brsh(pnt->brush());
    pnt->setBrush(Qt::NoBrush);
    pnt->drawEllipse(rect);
    pnt->setBrush(brsh);
}
//=============================================================================
void
RenderQt::circleFill(double x1, double y1, double radius)
{
    QPointF uleft(map(x1 - radius, y1 + radius, 0));
    QPointF lright(map(x1 + radius, y1 - radius, 0));
    QRectF rect;
    rect.setBottomRight(lright);
    rect.setTopLeft(uleft);
    QPen pen(pnt->pen());
    pnt->setPen(Qt::NoPen);
    pnt->drawEllipse(rect);
    pnt->setPen(pen);
}
//=============================================================================
static QColor
chartColor(const std::vector<double>& color)
{
    if (color.size() < 3) {
        return QColor(0, 0, 0, 0);
    }
    int alpha = color.size() >= 4 ? static_cast<int>(std::clamp(color[3], 0.0, 1.0) * 255) : 255;
    return QColor(static_cast<int>(std::clamp(color[0], 0.0, 1.0) * 255),
        static_cast<int>(std::clamp(color[1], 0.0, 1.0) * 255),
        static_cast<int>(std::clamp(color[2], 0.0, 1.0) * 255), alpha);
}
//=============================================================================
static bool
isNoColor(const std::vector<double>& color)
{
    return color.empty() || color[0] < 0;
}
//=============================================================================
static QPointF
pointOnCircle(const QPointF& center, double radius, double angleDegrees)
{
    double radians = -angleDegrees * M_PI / 180.0;
    return QPointF(
        center.x() + radius * std::cos(radians), center.y() + radius * std::sin(radians));
}
//=============================================================================
void
RenderQt::drawPieSlice(double centerX, double centerY, double outerRadius, double innerRadius,
    double startAngle, double spanAngle, const std::vector<double>& faceColor,
    const std::vector<double>& edgeColor, double lineWidth)
{
    if (outerRadius <= 0 || std::fabs(spanAngle) <= 1e-12) {
        return;
    }
    innerRadius = std::max(0.0, std::min(innerRadius, outerRadius));

    QPointF center(centerX, centerY);
    QRectF outer(centerX - outerRadius, centerY - outerRadius, 2 * outerRadius, 2 * outerRadius);
    QPainterPath path;
    path.arcMoveTo(outer, startAngle);
    path.arcTo(outer, startAngle, spanAngle);
    if (innerRadius > 0) {
        double endAngle = startAngle + spanAngle;
        QRectF inner(
            centerX - innerRadius, centerY - innerRadius, 2 * innerRadius, 2 * innerRadius);
        path.lineTo(pointOnCircle(center, innerRadius, endAngle));
        path.arcTo(inner, endAngle, -spanAngle);
    } else {
        path.lineTo(center);
    }
    path.closeSubpath();

    QPen oldPen = pnt->pen();
    QBrush oldBrush = pnt->brush();
    if (isNoColor(faceColor)) {
        pnt->setBrush(Qt::NoBrush);
    } else {
        pnt->setBrush(QBrush(chartColor(faceColor)));
    }
    if (isNoColor(edgeColor) || lineWidth <= 0) {
        pnt->setPen(Qt::NoPen);
    } else {
        QPen pen(chartColor(edgeColor));
        pen.setWidthF(edgeStrokePixelWidth(lineWidth));
        pnt->setPen(pen);
    }
    pnt->drawPath(path);
    pnt->setPen(oldPen);
    pnt->setBrush(oldBrush);
}
//=============================================================================
void
RenderQt::drawImage(int x1, int y1, const RGBAImageView& picView)
{
    QImage pic = Nelson::qImageFromView(picView);
    pnt->drawImage(x1, y1, pic);
}
//=============================================================================
void
RenderQt::drawImage(double x1, double y1, double x2, double y2, const RGBAImageView& picView)
{
    QImage pic = Nelson::qImageFromView(picView);
    QPointF pt(map(x1, y1, 0));
    pt.setY(pt.y() - pic.height());
    pnt->drawImage(pt, pic);
}
//=============================================================================
void
RenderQt::drawImage(const std::vector<double>& xp, const std::vector<double>& yp,
    const std::vector<double>& xLim, const std::vector<double>& yLim, bool xFlip, bool yFlip,
    const RGBAImageView& picView)
{
    QImage pic = Nelson::qImageFromView(picView);
    float dx = (pic.width() > 1) ? (float)((xp[1] - xp[0]) / (double)(pic.width() - 1)) : 1.0f;
    float dy = (pic.height() > 1) ? (float)((yp[1] - yp[0]) / (double)(pic.height() - 1)) : 1.0f;
    float data_x1 = (float)xp[0] - dx / 2.0f;
    float data_y1 = (float)yp[0] - dy / 2.0f;
    float data_x2 = (float)xp[1] + dx / 2.0f;
    float data_y2 = (float)yp[1] + dy / 2.0f;

    float lim_x1 = (float)xLim[0];
    float lim_y1 = (float)yLim[0];
    float lim_x2 = (float)xLim[1];
    float lim_y2 = (float)yLim[1];

    // A descending XData or YData span mirrors the image along that axis. Clip
    // against the ordered interval: the raw one collapses to a point and drops it.
    bool xDescending = (xp[1] < xp[0]);
    bool yDescending = (yp[1] < yp[0]);
    float data_xlo = std::min(data_x1, data_x2);
    float data_xhi = std::max(data_x1, data_x2);
    float data_ylo = std::min(data_y1, data_y2);
    float data_yhi = std::max(data_y1, data_y2);

    float vis_x1 = std::clamp(lim_x1, data_xlo, data_xhi);
    float vis_x2 = std::clamp(lim_x2, data_xlo, data_xhi);
    float vis_y1 = std::clamp(lim_y1, data_ylo, data_yhi);
    float vis_y2 = std::clamp(lim_y2, data_ylo, data_yhi);

    float img_x1 = (vis_x1 - data_x1) * pic.width() / (data_x2 - data_x1);
    float img_x2 = (vis_x2 - data_x1) * pic.width() / (data_x2 - data_x1);
    float img_y1 = (vis_y1 - data_y1) * pic.height() / (data_y2 - data_y1);
    float img_y2 = (vis_y2 - data_y1) * pic.height() / (data_y2 - data_y1);

    if (yFlip) {
        vis_y1 = lim_y1 + lim_y2 - vis_y1;
        vis_y2 = lim_y1 + lim_y2 - vis_y2;
    }

    if (xFlip) {
        vis_x1 = lim_x1 + lim_x2 - vis_x1;
        vis_x2 = lim_x1 + lim_x2 - vis_x2;
    }

    QPointF topLeft(map(std::min(vis_x1, vis_x2), std::max(vis_y1, vis_y2), 0));
    QPointF botRight(map(std::max(vis_x1, vis_x2), std::min(vis_y2, vis_y1), 0));
    QRectF target(topLeft, botRight);

    // Draw the visible sub-rectangle of the (non-owning) source directly, with any
    // mirroring applied through the painter transform, to avoid per-frame pixel copies.
    double src_x1 = std::clamp((double)std::min(img_x1, img_x2), 0.0, (double)pic.width());
    double src_y1 = std::clamp((double)std::min(img_y1, img_y2), 0.0, (double)pic.height());
    double src_x2 = std::clamp((double)std::max(img_x1, img_x2), 0.0, (double)pic.width());
    double src_y2 = std::clamp((double)std::max(img_y1, img_y2), 0.0, (double)pic.height());
    QRectF source(src_x1, src_y1, src_x2 - src_x1, src_y2 - src_y1);
    if (source.width() <= 0.0 || source.height() <= 0.0 || target.width() == 0.0
        || target.height() == 0.0) {
        return;
    }
    bool mirrorX = (xFlip != xDescending);
    bool mirrorY = ((!yFlip) != yDescending);
    if (!mirrorX && !mirrorY) {
        pnt->drawImage(target, pic, source);
        return;
    }
    pnt->save();
    QPointF center = target.center();
    QTransform mirror;
    mirror.translate(center.x(), center.y());
    mirror.scale(mirrorX ? -1.0 : 1.0, mirrorY ? -1.0 : 1.0);
    mirror.translate(-center.x(), -center.y());
    pnt->setWorldTransform(mirror, true);
    pnt->drawImage(target, pic, source);
    pnt->restore();
}
//=============================================================================
void
RenderQt::drawMappedQuad(const quad3D& mapqd)
{
    QPolygonF poly;
    poly.reserve(4);
    poly.push_back(QPointF(mapqd.pts[0].x, mapqd.pts[0].y));
    poly.push_back(QPointF(mapqd.pts[1].x, mapqd.pts[1].y));
    poly.push_back(QPointF(mapqd.pts[3].x, mapqd.pts[3].y));
    poly.push_back(QPointF(mapqd.pts[2].x, mapqd.pts[2].y));

    QColor faceColor(
        (int)(mapqd.r * 255), (int)(mapqd.g * 255), (int)(mapqd.b * 255), (int)(mapqd.a * 255));

    QColor edgeColor(QColor((int)(mapqd.er * 255), (int)(mapqd.eg * 255), (int)(mapqd.eb * 255),
        (int)(mapqd.ea * 255)));

    const bool texturedFill = mapqd.textureIndex >= 0
        && mapqd.textureIndex < static_cast<int>(m_textureStore.size()) && mapqd.a > 0;
    if (texturedFill) {
        const QImage& tex = m_textureStore[mapqd.textureIndex];
        pnt->setPen(Qt::NoPen);
        const double uv0[2] = { mapqd.u[0], mapqd.v[0] };
        const double uv1[2] = { mapqd.u[1], mapqd.v[1] };
        const double uv2[2] = { mapqd.u[2], mapqd.v[2] };
        const double uv3[2] = { mapqd.u[3], mapqd.v[3] };
        // Triangles follow the ring order of poly (0, 1, 3, 2), so their texture
        // coordinates and modulating colours use the matching quad vertices.
        drawTexturedTriangle(tex, poly[0], poly[1], poly[2], uv0, uv1, uv3,
            RGBAColorData(mapqd.vr[0], mapqd.vg[0], mapqd.vb[0], mapqd.va[0]),
            RGBAColorData(mapqd.vr[1], mapqd.vg[1], mapqd.vb[1], mapqd.va[1]),
            RGBAColorData(mapqd.vr[3], mapqd.vg[3], mapqd.vb[3], mapqd.va[3]));
        drawTexturedTriangle(tex, poly[0], poly[2], poly[3], uv0, uv3, uv2,
            RGBAColorData(mapqd.vr[0], mapqd.vg[0], mapqd.vb[0], mapqd.va[0]),
            RGBAColorData(mapqd.vr[3], mapqd.vg[3], mapqd.vb[3], mapqd.va[3]),
            RGBAColorData(mapqd.vr[2], mapqd.vg[2], mapqd.vb[2], mapqd.va[2]));
        if (mapqd.lineStyle == GO_PROPERTY_VALUE_NONE_STR) {
            return;
        }
    }

    bool filledWithInterpolation = !texturedFill && mapqd.interpolateFill && mapqd.a > 0;
    if (filledWithInterpolation) {
        pnt->setPen(Qt::NoPen);
        drawInterpolatedTriangle(poly[0], poly[1], poly[2],
            RGBAColorData(mapqd.vr[0], mapqd.vg[0], mapqd.vb[0], mapqd.va[0]),
            RGBAColorData(mapqd.vr[1], mapqd.vg[1], mapqd.vb[1], mapqd.va[1]),
            RGBAColorData(mapqd.vr[3], mapqd.vg[3], mapqd.vb[3], mapqd.va[3]));
        drawInterpolatedTriangle(poly[0], poly[2], poly[3],
            RGBAColorData(mapqd.vr[0], mapqd.vg[0], mapqd.vb[0], mapqd.va[0]),
            RGBAColorData(mapqd.vr[3], mapqd.vg[3], mapqd.vb[3], mapqd.va[3]),
            RGBAColorData(mapqd.vr[2], mapqd.vg[2], mapqd.vb[2], mapqd.va[2]));
        if (mapqd.lineStyle == GO_PROPERTY_VALUE_NONE_STR) {
            return;
        }
    }

    pnt->setBrush((filledWithInterpolation || texturedFill) ? Qt::NoBrush : QBrush(faceColor));
    if (mapqd.lineStyle == GO_PROPERTY_VALUE_NONE_STR) {
        pnt->setPen(QPen(faceColor));
        pnt->drawPolygon(poly);
        return;
    } else {
        QPen pen = pnt->pen();
        setLineStyle(pen, mapqd.lineStyle);
        QColor penColor = Qt::NoPen;
        if (mapqd.meshstyle == meshStyle::Both) {
            penColor = edgeColor;
        }
        pen.setColor(penColor);
        pen.setWidthF(mapqd.lineWidth);
        pnt->setPen(pen);
    }
    pnt->drawPolygon(poly);

    if (mapqd.meshstyle != meshStyle::Both) {
        size_t idx1, idx2;
        if (mapqd.meshstyle == meshStyle::Row) {
            idx1 = 1;
            idx2 = 3;
        } else {
            idx1 = 0;
            idx2 = 2;
        }
        for (int i = 0; i < poly.size(); i++) {
            if (i == idx1 || i == idx2) {
                pnt->setPen(edgeColor);
                pnt->drawLine(poly[i], poly[(i + 1) % poly.size()]);
            }
        }
    }
}
//=============================================================================
void
RenderQt::beginSurfaceGroup()
{
    if (surfaceGroupDepth > 0) {
        surfaceGroupDepth++;
        return;
    }
    surfaceGroupDepth = 1;
    deferredSurfaceQuads.clear();
    deferSurfaceQuads = true;
    bspPrimitives.clear();
    bspPrimitiveOrder = 0;
    collectBspPrimitives = true;
}
//=============================================================================
void
RenderQt::endSurfaceGroup()
{
    if (!deferSurfaceQuads) {
        return;
    }
    if (surfaceGroupDepth > 1) {
        surfaceGroupDepth--;
        return;
    }
    surfaceGroupDepth = 0;
    deferSurfaceQuads = false;
    collectBspPrimitives = false;
    flushBspPrimitives();
    if (deferredSurfaceQuads.empty()) {
        return;
    }
    sortQuadsByProjectedDepth(deferredSurfaceQuads);
    for (const auto& mapqd : deferredSurfaceQuads) {
        drawMappedQuad(mapqd);
    }
    deferredSurfaceQuads.clear();
}
//=============================================================================
void
RenderQt::quadStrips(const std::vector<std::vector<coloredPoint>>& faces, bool flatfaces,
    const std::vector<std::vector<coloredPoint>>& edges, bool flatedges, meshStyle meshstyle,
    double lineWidth, const std::wstring& lineStyle, enum LightingMode::LightingMode faceLighting,
    enum LightingMode::LightingMode edgeLighting, uint64_t)
{
    const bool hasLights = lightingScene.enabled && !lightingScene.lights.empty();
    bool lightFaces = hasLights && faceLighting != LightingMode::None;
    bool lightEdges = hasLights && edgeLighting != LightingMode::None;
    const std::vector<std::vector<coloredPoint>>* faceStrips = &faces;
    const std::vector<std::vector<coloredPoint>>* edgeStrips = &edges;
    std::vector<std::vector<coloredPoint>> litFaces;
    std::vector<std::vector<coloredPoint>> litEdges;
    if (lightFaces || lightEdges) {
        litFaces = faces;
        litEdges = edges;
        faceStrips = &litFaces;
        edgeStrips = &litEdges;
        for (auto& qlist : litFaces) {
            for (auto& pt : qlist) {
                RGBAColorData lit = applyLighting(RGBAColorData(pt.r, pt.g, pt.b, pt.a),
                    point(pt.nx, pt.ny, pt.nz), point(pt.x, pt.y, pt.z), lightFaces);
                pt.r = lit.r;
                pt.g = lit.g;
                pt.b = lit.b;
                pt.a = lit.a;
            }
        }
        for (auto& qlist : litEdges) {
            for (auto& pt : qlist) {
                RGBAColorData lit = applyLighting(RGBAColorData(pt.r, pt.g, pt.b, pt.a),
                    point(pt.nx, pt.ny, pt.nz), point(pt.x, pt.y, pt.z), lightEdges);
                pt.r = lit.r;
                pt.g = lit.g;
                pt.b = lit.b;
                pt.a = lit.a;
            }
        }
    }
    bool useInterpolatedFaces = faceLighting == LightingMode::Gouraud || !flatfaces;
    // Surface edges are stroked at a device pixel width at least: a sub-pixel pen
    // antialiases to nothing and the mesh of a dense pcolor/flat surf vanishes.
    const double edgeWidthPx = edgeStrokePixelWidth(lineWidth);
    if (collectBspPrimitives && !useInterpolatedFaces) {
        auto colorFromPoint = [](const coloredPoint& pt) {
            return QColor(static_cast<int>(std::clamp(pt.r, 0., 1.) * 255),
                static_cast<int>(std::clamp(pt.g, 0., 1.) * 255),
                static_cast<int>(std::clamp(pt.b, 0., 1.) * 255),
                static_cast<int>(std::clamp(pt.a, 0., 1.) * 255));
        };
        auto appendClippedEdge = [this, edgeWidthPx, &lineStyle](
                                     const point& p0, const point& p1, const QColor& color) {
            double ax0 = p0.x, ay0 = p0.y, az0 = p0.z;
            double ax1 = p1.x, ay1 = p1.y, az1 = p1.z;
            if (m_dataClip.enabled
                && !renderClipSegment(
                    m_dataClip, ax0, ay0, az0, ax1, ay1, az1, ax0, ay0, az0, ax1, ay1, az1)) {
                return;
            }
            appendBspLine(
                point(ax0, ay0, az0), point(ax1, ay1, az1), color, edgeWidthPx, lineStyle);
        };
        auto appendQuadEdges = [this, meshstyle, lineStyle, &appendClippedEdge](
                                   const std::vector<point>& pts, const QColor& color) {
            if (lineStyle == GO_PROPERTY_VALUE_NONE_STR || color.alpha() == 0) {
                return;
            }
            if (meshstyle == meshStyle::Both || meshstyle == meshStyle::Column) {
                appendClippedEdge(pts[0], pts[1], color);
                appendClippedEdge(pts[2], pts[3], color);
            }
            if (meshstyle == meshStyle::Both || meshstyle == meshStyle::Row) {
                appendClippedEdge(pts[1], pts[2], color);
                appendClippedEdge(pts[3], pts[0], color);
            }
        };
        size_t stripCount = std::max(faceStrips->size(), edgeStrips->size());
        size_t primitiveCount = 0;
        for (size_t i = 0; i < stripCount; ++i) {
            const std::vector<coloredPoint>* faceStrip
                = i < faceStrips->size() ? &(*faceStrips)[i] : nullptr;
            const std::vector<coloredPoint>* edgeStrip
                = i < edgeStrips->size() ? &(*edgeStrips)[i] : nullptr;
            const std::vector<coloredPoint>& source = faceStrip ? *faceStrip : *edgeStrip;
            size_t quadCount = source.size() >= 4 ? (source.size() - 2) / 2 : 0;
            if (faceStrip) {
                primitiveCount += quadCount;
            }
            if (edgeStrip && !(faceStrip && meshstyle == meshStyle::Both)) {
                size_t edgesPerQuad = meshstyle == meshStyle::Both ? 4 : 2;
                primitiveCount += quadCount * edgesPerQuad;
            }
        }
        bspPrimitives.reserve(bspPrimitives.size() + primitiveCount);
        for (size_t i = 0; i < stripCount; ++i) {
            const std::vector<coloredPoint>* faceStrip
                = i < faceStrips->size() ? &(*faceStrips)[i] : nullptr;
            const std::vector<coloredPoint>* edgeStrip
                = i < edgeStrips->size() ? &(*edgeStrips)[i] : nullptr;
            const std::vector<coloredPoint>& source = faceStrip ? *faceStrip : *edgeStrip;
            for (size_t j = 2; j + 1 < source.size(); j += 2) {
                std::vector<point> pts = { point(source[j - 2].x, source[j - 2].y, source[j - 2].z),
                    point(source[j - 1].x, source[j - 1].y, source[j - 1].z),
                    point(source[j + 1].x, source[j + 1].y, source[j + 1].z),
                    point(source[j].x, source[j].y, source[j].z) };
                if (faceStrip) {
                    // Clip the face polygon to the axes limits. Its outline is not
                    // stroked (edges are drawn separately as clipped segments), so no
                    // spurious grid line appears along the box faces.
                    std::vector<coloredPoint> quad = { (*faceStrip)[j - 2], (*faceStrip)[j - 1],
                        (*faceStrip)[j + 1], (*faceStrip)[j] };
                    std::vector<coloredPoint> clipped
                        = m_dataClip.enabled ? renderClipPolygon(m_dataClip, quad) : quad;
                    if (clipped.size() >= 3) {
                        std::vector<point> fpts;
                        fpts.reserve(clipped.size());
                        for (const auto& cp : clipped) {
                            fpts.push_back(point(cp.x, cp.y, cp.z));
                        }
                        appendBspPolygon(fpts, colorFromPoint((*faceStrip)[j - 2]),
                            QColor(0, 0, 0, 0), 0., GO_PROPERTY_VALUE_NONE_STR);
                    }
                }
                if (edgeStrip) {
                    appendQuadEdges(pts, colorFromPoint((*edgeStrip)[j - 2]));
                }
            }
        }
        return;
    }
    std::vector<quad3D> mapqds(mapQuads(*faceStrips, *edgeStrips, useInterpolatedFaces));
    for (auto& mapqd : mapqds) {
        mapqd.meshstyle = meshstyle;
        mapqd.lineWidth = edgeWidthPx;
        mapqd.lineStyle = lineStyle;
    }
    if (deferSurfaceQuads) {
        deferredSurfaceQuads.insert(deferredSurfaceQuads.end(), mapqds.begin(), mapqds.end());
        return;
    }
    sortQuadsByProjectedDepth(mapqds);
    for (const auto& mapqd : mapqds) {
        drawMappedQuad(mapqd);
    }
}
//=============================================================================
void
RenderQt::clear(std::vector<double> col)
{
    pnt->save();
    if (col.size() == 3) {
        pnt->setPen(QColor((int)(col[0] * 255), (int)(col[1] * 255), (int)(col[2] * 255)));
        pnt->setBrush(QColor((int)(col[0] * 255), (int)(col[1] * 255), (int)(col[2] * 255)));
        pnt->drawRect((int)m_x1, (int)m_y1, (int)m_width, (int)m_height);
    }
    pnt->restore();
}
//=============================================================================
std::vector<quad3D>
RenderQt::mapQuadsFacesOnly(
    const std::vector<std::vector<coloredPoint>>& faces, bool interpolateFaces)
{
    std::vector<quad3D> retval;
    bool packedQuads = !interpolateFaces && faces.size() == 1 && faces[0].size() > 4
        && (faces[0].size() % 4) == 0;
    if (packedQuads) {
        retval.reserve(faces[0].size() / 4);
        for (size_t base = 0; base + 3 < faces[0].size(); base += 4) {
            quad3D qx;
            bool anyclipped = false;
            double zmean = 0;
            for (int k = 0; k < 4; k++) {
                coloredPoint cpt(faces[0][base + k]);
                double a, b, c;
                bool clipped;
                toPixels(cpt.x, cpt.y, cpt.z, a, b, c, clipped);
                anyclipped = anyclipped | clipped;
                if (m_dataClip.enabled
                    && !renderClipPointInside(
                        m_dataClip, cpt.x, cpt.y, cpt.z, renderClipEpsilon(m_dataClip))) {
                    anyclipped = true;
                }
                qx.pts[k].x = a;
                qx.pts[k].y = m_height - 1 - b;
                qx.pts[k].z = c;
                zmean += c;
                qx.r = cpt.r;
                qx.g = cpt.g;
                qx.b = cpt.b;
                qx.a = cpt.a;
                qx.er = cpt.r;
                qx.eg = cpt.g;
                qx.eb = cpt.b;
                qx.ea = 0;
                qx.vr[k] = cpt.r;
                qx.vg[k] = cpt.g;
                qx.vb[k] = cpt.b;
                qx.va[k] = cpt.a;
                qx.u[k] = cpt.u;
                qx.v[k] = cpt.v;
            }
            qx.interpolateFill = false;
            qx.textureIndex = m_surfaceTextureActive ? m_currentTextureIndex : -1;
            qx.meanz = -zmean / 4.0;
            if (!anyclipped) {
                retval.push_back(qx);
            }
        }
        return retval;
    }
    retval.reserve(faces.size());
    for (const auto& qlist : faces) {
        for (int j = 2; j < qlist.size(); j += 2) {
            quad3D qx;
            bool anyclipped = false;
            double zmean = 0;
            for (int k = 0; k < 4; k++) {
                coloredPoint cpt(qlist[j - 2 + k]);
                double a, b, c;
                bool clipped;
                toPixels(cpt.x, cpt.y, cpt.z, a, b, c, clipped);
                anyclipped = anyclipped | clipped;
                if (m_dataClip.enabled
                    && !renderClipPointInside(
                        m_dataClip, cpt.x, cpt.y, cpt.z, renderClipEpsilon(m_dataClip))) {
                    anyclipped = true;
                }
                qx.pts[k].x = a;
                qx.pts[k].y = m_height - 1 - b;
                qx.pts[k].z = c;
                zmean += c;
                qx.r = cpt.r;
                qx.g = cpt.g;
                qx.b = cpt.b;
                qx.a = cpt.a;
                qx.er = cpt.r;
                qx.eg = cpt.g;
                qx.eb = cpt.b;
                qx.ea = 0;
                qx.vr[k] = cpt.r;
                qx.vg[k] = cpt.g;
                qx.vb[k] = cpt.b;
                qx.va[k] = cpt.a;
                qx.u[k] = cpt.u;
                qx.v[k] = cpt.v;
            }
            qx.interpolateFill = interpolateFaces;
            qx.textureIndex = m_surfaceTextureActive ? m_currentTextureIndex : -1;
            qx.meanz = -zmean / 4.0;
            if (!anyclipped) {
                retval.push_back(qx);
            }
        }
    }
    return retval;
}
//=============================================================================
std::vector<quad3D>
RenderQt::mapQuads(const std::vector<std::vector<coloredPoint>>& faces,
    const std::vector<std::vector<coloredPoint>>& edges, bool interpolateFaces)
{
    std::vector<quad3D> retval;
    if (edges.size() == 0) {
        return mapQuadsFacesOnly(faces, interpolateFaces);
    }
    if (faces.size() == 0) {
        return mapQuadsEdgesOnly(edges);
    }
    // The face and edge strips come from two independent tessellation passes
    // (GOSurface::buildQuadsNoTexMap) and are index-aligned only if both chose the
    // same tessellation. surf does (one strip per grid cell), mesh does not: its
    // white faces coalesce into variable-length strips, so strip count and length
    // differ. Pairing vertices by index then reads past the shorter edge strip, an
    // intermittent access violation. When the structures diverge, draw faces and
    // edges as independent quads, as RenderOpenGL and RenderWeb do.
    bool aligned = faces.size() == edges.size();
    for (size_t i = 0; aligned && i < faces.size(); i++) {
        if (faces[i].size() != edges[i].size()) {
            aligned = false;
        }
    }
    if (!aligned) {
        std::vector<quad3D> faceQuads = mapQuadsFacesOnly(faces, interpolateFaces);
        std::vector<quad3D> edgeQuads = mapQuadsEdgesOnly(edges);
        faceQuads.insert(faceQuads.end(), edgeQuads.begin(), edgeQuads.end());
        return faceQuads;
    }
    for (int i = 0; i < faces.size(); i++) {
        const std::vector<coloredPoint>& qlist = faces[i];
        const std::vector<coloredPoint>& elist = edges[i];
        for (int j = 2; j < qlist.size(); j += 2) {
            quad3D qx;
            bool anyclipped = false;
            double zmean = 0;
            for (int k = 0; k < 4; k++) {
                coloredPoint cpt(qlist[j - 2 + k]);
                coloredPoint ept(elist[j - 2 + k]);
                double a, b, c;
                bool clipped;
                toPixels(cpt.x, cpt.y, cpt.z, a, b, c, clipped);
                anyclipped = anyclipped | clipped;
                if (m_dataClip.enabled
                    && !renderClipPointInside(
                        m_dataClip, cpt.x, cpt.y, cpt.z, renderClipEpsilon(m_dataClip))) {
                    anyclipped = true;
                }
                qx.pts[k].x = a;
                qx.pts[k].y = m_height - 1 - b;
                qx.pts[k].z = c;
                zmean += c;
                qx.r = cpt.r;
                qx.g = cpt.g;
                qx.b = cpt.b;
                qx.a = cpt.a;
                qx.er = ept.r;
                qx.eg = ept.g;
                qx.eb = ept.b;
                qx.ea = ept.a;
                qx.vr[k] = cpt.r;
                qx.vg[k] = cpt.g;
                qx.vb[k] = cpt.b;
                qx.va[k] = cpt.a;
                qx.u[k] = cpt.u;
                qx.v[k] = cpt.v;
            }
            qx.interpolateFill = interpolateFaces;
            qx.textureIndex = m_surfaceTextureActive ? m_currentTextureIndex : -1;
            qx.meanz = -zmean / 4.0;
            if (!anyclipped) {
                retval.push_back(qx);
            }
        }
    }
    return retval;
}
//=============================================================================
std::vector<quad3D>
RenderQt::mapQuadsEdgesOnly(const std::vector<std::vector<coloredPoint>>& edges)
{
    std::vector<quad3D> retval;
    for (const auto& elist : edges) {
        for (int j = 2; j < elist.size(); j += 2) {
            quad3D qx;
            bool anyclipped = false;
            double zmean = 0;
            for (int k = 0; k < 4; k++) {
                coloredPoint cpt(elist[j - 2 + k]);
                double a, b, c;
                bool clipped;
                toPixels(cpt.x, cpt.y, cpt.z, a, b, c, clipped);
                anyclipped = anyclipped | clipped;
                if (m_dataClip.enabled
                    && !renderClipPointInside(
                        m_dataClip, cpt.x, cpt.y, cpt.z, renderClipEpsilon(m_dataClip))) {
                    anyclipped = true;
                }
                qx.pts[k].x = a;
                qx.pts[k].y = m_height - 1 - b;
                qx.pts[k].z = c;
                zmean += c;
                qx.r = cpt.r;
                qx.g = cpt.g;
                qx.b = cpt.b;
                qx.a = 0;
                qx.er = cpt.r;
                qx.eg = cpt.g;
                qx.eb = cpt.b;
                qx.ea = cpt.a;
                qx.vr[k] = cpt.r;
                qx.vg[k] = cpt.g;
                qx.vb[k] = cpt.b;
                qx.va[k] = cpt.a;
            }
            qx.interpolateFill = false;
            qx.meanz = -zmean / 4.0;
            if (!anyclipped) {
                retval.push_back(qx);
            }
        }
    }
    return retval;
}
//=============================================================================
void
RenderQt::quad(double x1, double y1, double z1, double x2, double y2, double z2, double x3,
    double y3, double z3, double x4, double y4, double z4)
{
    if (collectBspPrimitives) {
        appendBspPolygon(
            { point(x1, y1, z1), point(x2, y2, z2), point(x3, y3, z3), point(x4, y4, z4) },
            pnt->brush().color(), pnt->pen().color(), currentLineWidth, currentLineStyle);
        return;
    }
    QPolygonF poly;
    poly.reserve(4);
    poly.push_back(map(x1, y1, z1));
    poly.push_back(map(x2, y2, z2));
    poly.push_back(map(x3, y3, z3));
    poly.push_back(map(x4, y4, z4));
    pnt->drawPolygon(poly);
}
//=============================================================================
void
RenderQt::quadline(double x1, double y1, double z1, double x2, double y2, double z2, double x3,
    double y3, double z3, double x4, double y4, double z4)
{
    if (collectBspPrimitives) {
        QColor color = pnt->pen().color();
        appendBspLine(
            point(x1, y1, z1), point(x2, y2, z2), color, currentLineWidth, currentLineStyle);
        appendBspLine(
            point(x2, y2, z2), point(x3, y3, z3), color, currentLineWidth, currentLineStyle);
        appendBspLine(
            point(x3, y3, z3), point(x4, y4, z4), color, currentLineWidth, currentLineStyle);
        appendBspLine(
            point(x4, y4, z4), point(x1, y1, z1), color, currentLineWidth, currentLineStyle);
        return;
    }
    QPolygonF poly;
    poly.reserve(5);
    poly.push_back(map(x1, y1, z1));
    poly.push_back(map(x2, y2, z2));
    poly.push_back(map(x3, y3, z3));
    poly.push_back(map(x4, y4, z4));
    poly.push_back(map(x1, y1, z1));
    pnt->drawPolyline(poly);
}
//=============================================================================
void
RenderQt::toPixelsImpl(
    double x, double y, double z, double& xclip, double& yclip, double& zclip, bool& clipped)
{
    double xprime = 0;
    double yprime = 0;
    double zprime = 0;
    mapPoint(x, y, z, xprime, yprime, zprime);

    double wclip = proj[3] * xprime + proj[7] * yprime + proj[11] * zprime + proj[15];
    if (wclip <= 0.0) {
        // At or behind the perspective near plane: the divide would flip signs,
        // so flag the point clipped instead (orthographic keeps wclip == 1).
        xclip = yclip = zclip = 2.0;
        clipped = true;
        return;
    }
    xclip = (proj[0] * xprime + proj[4] * yprime + proj[8] * zprime + proj[12]) / wclip;
    yclip = (proj[1] * xprime + proj[5] * yprime + proj[9] * zprime + proj[13]) / wclip;
    zclip = (proj[2] * xprime + proj[6] * yprime + proj[10] * zprime + proj[14]) / wclip;

    constexpr double clipEpsilon = 1e-8;
    clipped = (xclip < -1. - clipEpsilon) || (xclip > 1. + clipEpsilon)
        || (yclip < -1. - clipEpsilon) || (yclip > 1. + clipEpsilon) || (zclip < -1. - clipEpsilon)
        || (zclip > 1. + clipEpsilon);
}
//=============================================================================
void
RenderQt::toPixels(double x, double y, double z, double& a, double& b, double& c, bool& clipped)
{
    double xclip, yclip, zclip;
    toPixelsImpl(x, y, z, xclip, yclip, zclip, clipped);
    a = viewp[0] + (1 + xclip) / 2.0 * viewp[2];
    b = viewp[1] + (1 + yclip) / 2.0 * viewp[3];
    c = zclip;
}
//=============================================================================
void
RenderQt::toPixels(double x, double y, double z, double& a, double& b, bool& clipped)
{
    double xclip, yclip, zclip;
    toPixelsImpl(x, y, z, xclip, yclip, zclip, clipped);
    a = viewp[0] + (1 + xclip) / 2.0 * viewp[2];
    b = viewp[1] + (1 + yclip) / 2.0 * viewp[3];
}
//=============================================================================
void
RenderQt::toPixels(double x, double y, double z, double& a, double& b)
{
    bool clipped;
    toPixels(x, y, z, a, b, clipped);
}
//=============================================================================
void
RenderQt::toPixels(double x, double y, double z, int& a, int& b)
{
    double aval, bval;
    bool clipped;
    toPixels(x, y, z, aval, bval, clipped);
    a = static_cast<int>(aval);
    b = static_cast<int>(bval);
}
//=============================================================================
static void
cross(double ux, double uy, double uz, double vx, double vy, double vz, double& sx, double& sy,
    double& sz)
{
    sx = uy * vz - uz * vy;
    sy = uz * vx - ux * vz;
    sz = ux * vy - uy * vx;
}
//=============================================================================
static bool
operator<(const quad3D& a, const quad3D& b)
{
    return (a.meanz < b.meanz);
}
//=============================================================================
void
RenderQt::scale(double sx, double sy, double sz)
{
    model[0] *= sx;
    model[4] *= sy;
    model[8] *= sz;
    model[1] *= sx;
    model[5] *= sy;
    model[9] *= sz;
    model[2] *= sx;
    model[6] *= sy;
    model[10] *= sz;
    model[3] *= sx;
    model[7] *= sy;
    model[11] *= sz;
}
//=============================================================================
void
RenderQt::pushModelMatrix(const double* matrix16ColumnMajor)
{
    std::array<double, 16> saved;
    for (int i = 0; i < 16; ++i) {
        saved[i] = model[i];
    }
    m_modelStack.push_back(saved);
    m_dataClipEnabledStack.push_back(m_dataClip.enabled ? 1 : 0);
    m_dataClip.enabled = false;
    if (!matrix16ColumnMajor) {
        return;
    }
    // result = model * M, both column-major (index = col * 4 + row).
    double result[16];
    for (int col = 0; col < 4; ++col) {
        for (int row = 0; row < 4; ++row) {
            double sum = 0.0;
            for (int k = 0; k < 4; ++k) {
                sum += saved[k * 4 + row] * matrix16ColumnMajor[col * 4 + k];
            }
            result[col * 4 + row] = sum;
        }
    }
    for (int i = 0; i < 16; ++i) {
        model[i] = result[i];
    }
}
//=============================================================================
void
RenderQt::popModelMatrix()
{
    if (m_modelStack.empty()) {
        return;
    }
    const std::array<double, 16>& saved = m_modelStack.back();
    for (int i = 0; i < 16; ++i) {
        model[i] = saved[i];
    }
    m_modelStack.pop_back();
    if (!m_dataClipEnabledStack.empty()) {
        m_dataClip.enabled = m_dataClipEnabledStack.back() != 0;
        m_dataClipEnabledStack.pop_back();
    }
}
//=============================================================================
void
RenderQt::setSurfaceTexture(const RGBAImageView& texture)
{
    if (texture.pixels == nullptr || texture.width <= 0 || texture.height <= 0) {
        clearSurfaceTexture();
        return;
    }
    // Copy into an owned QImage kept in the per-paint store, so a deferred 3-D
    // surface group can still sample it after clearSurfaceTexture unbinds it.
    QImage image = Nelson::qImageFromView(texture).convertToFormat(QImage::Format_ARGB32);
    if (image.isNull()) {
        clearSurfaceTexture();
        return;
    }
    m_textureStore.push_back(std::move(image));
    m_currentTextureIndex = static_cast<int>(m_textureStore.size()) - 1;
    m_surfaceTextureActive = true;
}
//=============================================================================
void
RenderQt::clearSurfaceTexture()
{
    // Unbind for subsequent objects, but keep the stored textures: deferred quads
    // reference them by index and are drawn later, in endSurfaceGroup.
    m_surfaceTextureActive = false;
    m_currentTextureIndex = -1;
}
//=============================================================================
void
RenderQt::lookAt(double eyex, double eyey, double eyez, double centerx, double centery,
    double centerz, double upx, double upy, double upz)
{
    double fx = centerx - eyex;
    double fy = centery - eyey;
    double fz = centerz - eyez;
    double fnorm = sqrt(fx * fx + fy * fy + fz * fz);
    fx /= fnorm;
    fy /= fnorm;
    fz /= fnorm;
    double upnorm = sqrt(upx * upx + upy * upy + upz * upz);
    upx /= upnorm;
    upy /= upnorm;
    upz /= upnorm;
    double sx, sy, sz;
    cross(fx, fy, fz, upx, upy, upz, sx, sy, sz);
    if (std::sqrt(sx * sx + sy * sy + sz * sz) < 1e-9) {
        // Up vector (nearly) parallel to the viewing direction, e.g. a camera looking
        // straight down an axis. Use the world axis least aligned with the forward
        // direction, otherwise the basis collapses to a null matrix.
        double auxx = 0.0, auxy = 0.0, auxz = 0.0;
        if (std::fabs(fx) <= std::fabs(fy) && std::fabs(fx) <= std::fabs(fz)) {
            auxx = 1.0;
        } else if (std::fabs(fy) <= std::fabs(fz)) {
            auxy = 1.0;
        } else {
            auxz = 1.0;
        }
        cross(fx, fy, fz, auxx, auxy, auxz, sx, sy, sz);
    }
    double ux, uy, uz;
    cross(sx, sy, sz, fx, fy, fz, ux, uy, uz);
    model[0] = sx;
    model[4] = sy;
    model[8] = sz;
    model[12] = -(sx * eyex + sy * eyey + sz * eyez);
    model[1] = ux;
    model[5] = uy;
    model[9] = uz;
    model[13] = -(ux * eyex + uy * eyey + uz * eyez);
    model[2] = -fx;
    model[6] = -fy;
    model[10] = -fz;
    model[14] = fx * eyex + fy * eyey + fz * eyez;
    model[3] = 0;
    model[7] = 0;
    model[11] = 0;
    model[15] = 1;
}
//=============================================================================
void
RenderQt::mapPoint(double x, double y, double z, double& a, double& b, double& c)
{
    a = model[0] * x + model[4] * y + model[8] * z + model[12];
    b = model[1] * x + model[5] * y + model[9] * z + model[13];
    c = model[2] * x + model[6] * y + model[10] * z + model[14];
}
//=============================================================================
void
RenderQt::project(double left, double right, double bottom, double top, double near, double far)
{
    double tx = -(right + left) / (right - left);
    double ty = -(top + bottom) / (top - bottom);
    double tz = -(far + near) / (far - near);
    for (double& i : proj) {
        i = 0;
    }
    proj[0] = 2 / (right - left);
    proj[5] = 2 / (top - bottom);
    proj[10] = -2 / (far - near);
    proj[12] = tx;
    proj[13] = ty;
    proj[14] = tz;
    proj[15] = 1;
}
//=============================================================================
void
RenderQt::projectPerspective(
    double left, double right, double bottom, double top, double near, double far)
{
    buildFrustumProjectionMatrix(proj, left, right, bottom, top, near, far);
}
//=============================================================================
void
RenderQt::viewport(double x0, double y0, double width, double height)
{
    viewp[0] = (int)x0;
    viewp[1] = (int)y0;
    viewp[2] = (int)width;
    viewp[3] = (int)height;
    pnt->setClipRect((int)x0, (int)(m_height - (y0 + height)), (int)width, (int)height);
    clipToRegion();
}
//=============================================================================
void
RenderQt::clipToEntireFrame()
{
    // Axes Clipping == 'off': widen the painter clip from the axes rect to the whole
    // frame so overflowing geometry spills into the figure. The viewport mapping is
    // untouched, so content still lands in the axes sub-rect. Inside a pushed region
    // the frame is the region: clip to it instead of disabling clipping.
    if (!regions.empty()) {
        pnt->setClipRect(QRectF(0, 0, m_width, m_height), Qt::ReplaceClip);
        return;
    }
    pnt->setClipping(false);
}
//=============================================================================
bool
RenderQt::pushRegion(double x, double y, double width, double height)
{
    if (width <= 0 || height <= 0) {
        return false;
    }
    regions.push_back({ m_x1, m_y1, m_width, m_height });
    // Translating the painter onto the sub-rectangle keeps every coordinate derived
    // from width()/height() valid without touching the projection math. The incoming
    // origin is bottom-left, the painter origin is top-left.
    pnt->save();
    pnt->translate(x, m_height - (y + height));
    m_x1 = 0;
    m_y1 = 0;
    m_width = width;
    m_height = height;
    clipToRegion();
    return true;
}
//=============================================================================
void
RenderQt::popRegion()
{
    if (regions.empty()) {
        return;
    }
    pnt->restore();
    const SavedRegion& saved = regions.back();
    m_x1 = saved.x1;
    m_y1 = saved.y1;
    m_width = saved.width;
    m_height = saved.height;
    regions.pop_back();
}
//=============================================================================
void
RenderQt::clipToRegion()
{
    if (regions.empty()) {
        return;
    }
    pnt->setClipRect(QRectF(0, 0, m_width, m_height), Qt::IntersectClip);
}
//=============================================================================
std::vector<std::wstring>
RenderQt::splitEoLText(const std::wstring& text)
{
    wchar_t delim = L'\n';
    std::wstring line;
    std::vector<std::wstring> vec;
    std::wstringstream ss(text);
    while (std::getline(ss, line, delim)) {
        vec.push_back(line);
    }
    return vec;
}
//=============================================================================
QRect
RenderQt::boundingRectMultiLine(const std::wstring& text, QFont fnt, size_t& nbLines)
{
    QFontMetrics fm(fnt);
    std::vector<std::wstring> lines = splitEoLText(text);
    nbLines = lines.size();
    if (nbLines < 2) {
        return fm.boundingRect(Nelson::wstringToQString(text));
    }
    // QFontMetrics::boundingRect(QString) measures a single line: it counts a
    // newline as one more (unprintable) glyph instead of starting a new line.
    // The block is as wide as its widest line and grows by one line advance
    // per extra line, the last line keeping its own ink box.
    QRect sze = fm.boundingRect(Nelson::wstringToQString(lines.back()));
    int widest = 0;
    for (const std::wstring& line : lines) {
        widest = std::max(widest, fm.boundingRect(Nelson::wstringToQString(line)).width());
    }
    sze.setWidth(widest);
    sze.setHeight(sze.height() + static_cast<int>(nbLines - 1) * fm.lineSpacing());
    sze.moveTop(sze.top() - static_cast<int>(nbLines - 1) * fm.lineSpacing());
    return sze;
}
//=============================================================================
void
RenderQt::measureText(std::wstring txt, const FontSpec& fntSpec, AlignmentFlag xflag,
    AlignmentFlag yflag, int& width, int& height, int& xoffset, int& yoffset)
{
    QFont fnt = Nelson::qFontFromSpec(fntSpec);
    size_t nbLines;
    QRect sze = boundingRectMultiLine(txt, fnt, nbLines);
    width = sze.width();
    height = sze.height();
    yoffset = -height;
    xoffset = 0;
    if (xflag == Mean) {
        xoffset -= width / 2;
    } else if (xflag == Max) {
        xoffset -= width;
    }
    if (yflag == Mean) {
        yoffset += height / 2;
    } else if (yflag == Min) {
        yoffset += height;
    }
}
//=============================================================================
void
RenderQt::putText(double x, double y, std::wstring txt, std::vector<double> color,
    AlignmentFlag xflag, AlignmentFlag yflag, const FontSpec& fntSpec, double rotation)
{
    QFont fnt = Nelson::qFontFromSpec(fntSpec);
    size_t nbLines;
    QRect sze = boundingRectMultiLine(txt, fnt, nbLines);
    QFontMetrics fm(fnt);
    int width = sze.width();
    int height = sze.height();
    double xdelta = 0.;
    double ydelta = 0.;
    if (xflag == Mean) {
        xdelta = -width / 2.0;
    }
    if (xflag == Max) {
        xdelta = -width;
    }
    if (yflag == Mean) {
        ydelta = -height / 2.0;
    }
    if (yflag == Max) {
        ydelta = -height;
    }
    ydelta += fm.descent();
    double costhet = cos(rotation * M_PI / 180.0);
    double sinthet = sin(rotation * M_PI / 180.0);
    double xpos = x + xdelta * costhet - ydelta * sinthet;
    double ypos = y + xdelta * sinthet + ydelta * costhet;
    QPointF pos(map(xpos, ypos, 0));
    QPen pen(pnt->pen());
    QBrush brush(pnt->brush());
    QColor textColor((int)(color[0] * 255), (int)(color[1] * 255), (int)(color[2] * 255));
    pnt->setPen(textColor);
    pnt->setFont(fnt);
    pnt->setBackground(QBrush(Qt::red));
    pnt->save();
    pnt->translate(pos);
    pnt->rotate(-rotation);
    auto drawLine = [&](const std::wstring& line, int baseline) {
        QString qText = Nelson::wstringToQString(line);
        if (renderName == L"SVG") {
            QPainterPath path;
            path.addText(QPointF(0, baseline), fnt, qText);
            pnt->setPen(Qt::NoPen);
            pnt->setBrush(QBrush(textColor));
            pnt->drawPath(path);
        } else {
            pnt->drawText(0, baseline, qText);
        }
    };
    if (nbLines > 1) {
        // One draw per line: the last line keeps the single-line baseline and earlier
        // lines stack above it by one line advance each, which is where the alignment
        // offsets computed from the block height expect them.
        const std::vector<std::wstring> lines = splitEoLText(txt);
        const int lineSpacing = fm.lineSpacing();
        for (size_t k = 0; k < lines.size(); ++k) {
            drawLine(lines[k], -static_cast<int>(lines.size() - 1 - k) * lineSpacing);
        }
    } else {
        drawLine(txt, 0);
    }
    pnt->restore();
    pnt->setPen(pen);
    pnt->setBrush(brush);
}
//=============================================================================
