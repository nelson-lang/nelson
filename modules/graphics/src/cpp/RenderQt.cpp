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
#include <algorithm>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Types.hpp"
#include "GOPropertyValues.hpp"
#include "RenderQt.hpp"
#include "characters_encoding.hpp"
#include "QStringConverter.hpp"
#include "ParallelSort.hpp"
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
    renderName = _renderName;
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
    QPolygonF poly;
    poly.reserve(3);
    poly.push_back(map(x1, y1, z1));
    poly.push_back(map(x2, y2, z2));
    poly.push_back(map(x3, y3, z3));
    pnt->drawPolygon(poly);
}
//=============================================================================
void
RenderQt::triLine(double x1, double y1, double z1, double x2, double y2, double z2, double x3,
    double y3, double z3)
{
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
    if (style == L"-") {
        _pen.setStyle(Qt::SolidLine);
    } else if (style == L"--") {
        _pen.setStyle(Qt::DashLine);
    } else if (style == L":") {
        _pen.setStyle(Qt::DotLine);
    } else if (style == L"-.") {
        _pen.setStyle(Qt::DashDotLine);
    } else if (style == GO_PROPERTY_VALUE_NONE_STR) {
        _pen.setStyle(Qt::NoPen);
    }
}
//=============================================================================
void
RenderQt::setLineStyle(const std::wstring& style)
{
    QPen pen(pnt->pen());
    setLineStyle(pen, style);
    pnt->setPen(pen);
}
//=============================================================================
void
RenderQt::lineWidth(double n)
{
    QPen pen(pnt->pen());
    pen.setWidthF(n + 0.2);
    pnt->setPen(pen);
}
//=============================================================================
void
RenderQt::line(double x1, double y1, double z1, double x2, double y2, double z2)
{
    pnt->drawLine(map(x1, y1, z1), map(x2, y2, z2));
}
//=============================================================================
void
RenderQt::line(double x1, double y1, double x2, double y2)
{
    pnt->drawLine(map(x1, y1, 0), map(x2, y2, 0));
}
//=============================================================================
void
RenderQt::lineSeries(std::vector<double> xs, std::vector<double> ys, std::vector<double> zs)
{
    if (xs.size() < 2) {
        return;
    }
    pnt->drawPolyline(map(xs, ys, zs));
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
void
RenderQt::drawImage(int x1, int y1, QImage pic)
{
    pnt->drawImage(x1, y1, pic);
}
//=============================================================================
void
RenderQt::drawImage(double x1, double y1, double x2, double y2, QImage pic)
{
    QPointF pt(map(x1, y1, 0));
    pt.setY(pt.y() - pic.height());
    pnt->drawImage(pt, pic);
}
//=============================================================================
void
RenderQt::drawImage(const std::vector<double>& xp, const std::vector<double>& yp,
    const std::vector<double>& xLim, const std::vector<double>& yLim, bool xFlip, bool yFlip,
    QImage pic)
{
    float data_x1 = (float)xp[0];
    float data_y1 = (float)yp[0];
    float data_x2 = (float)xp[1];
    float data_y2 = (float)yp[1];

    float lim_x1 = (float)xLim[0];
    float lim_y1 = (float)yLim[0];
    float lim_x2 = (float)xLim[1];
    float lim_y2 = (float)yLim[1];

    float vis_x1 = qMax(qMin(lim_x1, data_x2), data_x1);
    float vis_x2 = qMax(qMin(lim_x2, data_x2), data_x1);
    float vis_y1 = qMax(qMin(lim_y1, data_y2), data_y1);
    float vis_y2 = qMax(qMin(lim_y2, data_y2), data_y1);

    float img_x1 = (vis_x1 - data_x1) * (pic.width() - 1) / (data_x2 - data_x1);
    float img_x2 = (vis_x2 - data_x1) * (pic.width() - 1) / (data_x2 - data_x1);
    float img_y1 = (vis_y1 - data_y1) * (pic.height() - 1) / (data_y2 - data_y1);
    float img_y2 = (vis_y2 - data_y1) * (pic.height() - 1) / (data_y2 - data_y1);

    if (yFlip) {
        vis_y1 = lim_y1 + lim_y2 - vis_y1;
        vis_y2 = lim_y1 + lim_y2 - vis_y2;
    }

    if (!yFlip) {
        float img_y1_t = pic.height() - img_y2;
        img_y2 = pic.height() - img_y1;
        img_y1 = img_y1_t;
    }

    if (xFlip) {
        vis_x1 = lim_x1 + lim_x2 - vis_x1;
        vis_x2 = lim_x1 + lim_x2 - vis_x2;
    }

    if (xFlip) {
        float img_x1_t = pic.width() - img_x2;
        img_x2 = pic.width() - img_x1;
        img_x1 = img_x1_t;
    }

    img_x2++;
    img_y2++;

    QPointF topLeft(map(std::min(vis_x1, vis_x2), std::max(vis_y1, vis_y2), 0));
    QPointF botRight(map(std::max(vis_x1, vis_x2), std::min(vis_y2, vis_y1), 0));
    QRectF target(topLeft, botRight);
    QRectF source(QPointF(img_x1, img_y1), QPointF(img_x2, img_y2));

    float xscale = xFlip ? -1 : 1;
    float yscale = (!yFlip) ? -1 : 1;

    pnt->drawImage(target, pic.transformed(QTransform().scale(xscale, yscale)), source);
}
//=============================================================================
void
RenderQt::quadStrips(std::vector<std::vector<coloredPoint>> faces, bool flatfaces,
    std::vector<std::vector<coloredPoint>> edges, bool flatedges, meshStyle meshstyle,
    double lineWidth, const std::wstring& lineStyle)
{
    std::vector<quad3D> mapqds(mapQuads(faces, edges));
    Nelson::parallelSort(mapqds);
    bool isLineStyleNone = (lineStyle == GO_PROPERTY_VALUE_NONE_STR);

    for (size_t k = 0; k < mapqds.size(); k++) {
        quad3D mapqd = mapqds[k];
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

        pnt->setBrush(faceColor);
        if (isLineStyleNone) {
            pnt->setPen(QPen(faceColor));
        } else {
            QPen pen = pnt->pen();
            setLineStyle(pen, lineStyle);
            QColor penColor = Qt::NoPen;
            if (meshstyle == meshStyle::Both) {
                penColor = edgeColor;
            }
            pen.setColor(penColor);
            pen.setWidth(lineWidth);
            pnt->setPen(pen);
        }
        pnt->drawPolygon(poly);

        if (meshstyle != meshStyle::Both) {
            size_t idx1, idx2;
            if (meshstyle == meshStyle::Row) {
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
RenderQt::mapQuadsFacesOnly(std::vector<std::vector<coloredPoint>>& faces)
{
    std::vector<quad3D> retval;
    retval.reserve(faces.size());
    for (auto qlist : faces) {
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
            }
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
RenderQt::mapQuads(
    std::vector<std::vector<coloredPoint>>& faces, std::vector<std::vector<coloredPoint>>& edges)
{
    std::vector<quad3D> retval;
    if (edges.size() == 0) {
        return mapQuadsFacesOnly(faces);
    }
    if (faces.size() == 0) {
        return mapQuadsEdgesOnly(edges);
    }
    for (int i = 0; i < faces.size(); i++) {
        std::vector<coloredPoint> qlist(faces[i]);
        std::vector<coloredPoint> elist(edges[i]);
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
            }
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
RenderQt::mapQuadsEdgesOnly(std::vector<std::vector<coloredPoint>>& edges)
{
    std::vector<quad3D> retval;
    for (auto elist : edges) {
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
            }
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
    xclip = (proj[0] * xprime + proj[4] * yprime + proj[8] * zprime + proj[12]) / wclip;
    yclip = (proj[1] * xprime + proj[5] * yprime + proj[9] * zprime + proj[13]) / wclip;
    zclip = (proj[2] * xprime + proj[6] * yprime + proj[10] * zprime + proj[14]) / wclip;

    clipped
        = (xclip < -1) || (xclip > 1) || (yclip < -1) || (yclip > 1) || (zclip < -1) || (zclip > 1);
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
    double ux, uy, uz;
    cross(sx, sy, sz, fx, fy, fz, ux, uy, uz);
    model[0] = sx;
    model[4] = sy;
    model[8] = sz;
    model[12] = -eyex;
    model[1] = ux;
    model[5] = uy;
    model[9] = uz;
    model[13] = -eyey;
    model[2] = -fx;
    model[6] = -fy;
    model[10] = -fz;
    model[14] = -eyez;
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
RenderQt::viewport(double x0, double y0, double width, double height)
{
    viewp[0] = (int)x0;
    viewp[1] = (int)y0;
    viewp[2] = (int)width;
    viewp[3] = (int)height;
    pnt->setClipRect((int)x0, (int)(m_height - (y0 + height)), (int)width, (int)height);
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
    QRect sze;
    nbLines = lines.size();
    // if (nbLines > 1) {
    //     size_t idxMax = 0;
    //     size_t lenMax = 0;
    //     for (size_t k = 0; k < nbLines; k++) {
    //         size_t len = std::max(lenMax, lines[k].length());
    //         if (len > lenMax) {
    //             idxMax = k;
    //         }
    //     }
    //     sze = fm.boundingRect(Nelson::wstringToQString(lines[idxMax]));
    //     sze.setHeight(sze.height() * nbLines);
    // } else {
    //     sze = fm.boundingRect(Nelson::wstringToQString(text));
    // }
    sze = fm.boundingRect(Nelson::wstringToQString(text));

    return sze;
}
//=============================================================================
void
RenderQt::measureText(std::wstring txt, QFont fnt, AlignmentFlag xflag, AlignmentFlag yflag,
    int& width, int& height, int& xoffset, int& yoffset)
{
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
    AlignmentFlag xflag, AlignmentFlag yflag, QFont fnt, double rotation)
{
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
    pnt->setPen(QColor((int)(color[0] * 255), (int)(color[1] * 255), (int)(color[2] * 255)));
    pnt->setFont(fnt);
    pnt->setBackground(QBrush(Qt::red));
    pnt->save();
    pnt->translate(pos);
    pnt->rotate(-rotation);
    if (nbLines > 1) {
        QRect rect(0, 0, width, height);
        pnt->drawText(rect, Qt::TextWordWrap, Nelson::wstringToQString(txt));
    } else {
        pnt->drawText(0, 0, Nelson::wstringToQString(txt));
    }
    pnt->restore();
    pnt->setPen(pen);
}
//=============================================================================
void
RenderQt::drawPatch(const FaceList& faces, double lineWidth, const std::wstring& lineStyle)
{
    QPolygonF poly;
    if (faces.size() == 0) {
        return;
    }
    poly.reserve(std::max(faces[0].vertices.size(), faces[faces.size() - 1].vertices.size()));
    for (const auto& face : faces) {
        poly.resize(0);
        if (face.FaceColorMode == ColorMode::ColorSpec) {
            pnt->setBrush(QColor((int)(face.FaceColor.r * 255), (int)(face.FaceColor.g * 255),
                (int)(face.FaceColor.b * 255), (int)(face.FaceColor.a * 255)));
        }
        if (face.FaceColorMode == ColorMode::Flat || face.FaceColorMode == ColorMode::Interp) {
            pnt->setBrush(
                QColor((int)(face.vertexcolors[0].r * 255), (int)(face.vertexcolors[0].g * 255),
                    (int)(face.vertexcolors[0].b * 255), (int)(face.vertexcolors[0].a * 255)));
        }
        if (face.EdgeColorMode == ColorMode::ColorSpec) {
            QPen pen((QColor((int)(face.EdgeColor.r * 255), (int)(face.EdgeColor.g * 255),
                (int)(face.EdgeColor.b * 255), (int)(face.EdgeColor.a * 255))));
            pen.setWidthF(lineWidth);
            setLineStyle(pen, lineStyle);
            pnt->setPen(pen);
        }
        if (face.EdgeColorMode == ColorMode::Flat || face.EdgeColorMode == ColorMode::Interp) {
            QPen pen(QColor((int)(face.edgecolors[0].r * 255), (int)(face.edgecolors[0].g * 255),
                (int)(face.edgecolors[0].b * 255), (int)(face.edgecolors[0].a * 255)));
            pen.setWidthF(lineWidth);
            setLineStyle(pen, lineStyle);
            pnt->setPen(pen);
        }
        // Map vertices
        for (const auto& v : face.vertices) {
            poly.push_back(map(v.x, v.y, v.z));
        }
        // Draw the polygon
        pnt->drawPolygon(poly);
    }
}
//=============================================================================
