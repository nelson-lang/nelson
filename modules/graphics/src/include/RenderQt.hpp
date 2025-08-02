//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <QtGui/QPainter>
#include <QtGui/QPainterPath>
#include <QtCore/QPointF>
#include "RenderInterface.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
using point3D = struct
{
    double x;
    double y;
    double z;
};
//=============================================================================
using quad3D = struct
{
    point3D pts[4];
    double meanz;
    double r;
    double g;
    double b;
    double a;
    double er;
    double eg;
    double eb;
    double ea;
};
//=============================================================================
class NLSGRAPHICS_IMPEXP RenderQt : public RenderInterface
{
private:
    //=============================================================================
    std::wstring renderName;

    double model[16];
    double proj[16];
    int viewp[4];
    double save_model[16];
    double save_proj[16];
    int save_viewp[4];
    double m_x1, m_y1, m_width, m_height;
    QPainter* pnt;
    bool inDirect;
    bool debugFlag;
    //=============================================================================
    QPointF
    map(double x, double y, double z);
    //=============================================================================
    QVector<QPointF>
    map(const std::vector<double>& xs, const std::vector<double>& ys,
        const std::vector<double>& zs);
    //=============================================================================
    std::vector<quad3D>
    mapQuads(std::vector<std::vector<coloredPoint>>& faces,
        std::vector<std::vector<coloredPoint>>& edges);
    //=============================================================================
    std::vector<quad3D>
    mapQuadsFacesOnly(std::vector<std::vector<coloredPoint>>& faces);
    //=============================================================================
    std::vector<quad3D>
    mapQuadsEdgesOnly(std::vector<std::vector<coloredPoint>>& edges);
    //=============================================================================
    std::vector<std::wstring>
    splitEoLText(const std::wstring& text);
    //=============================================================================
    QRect
    boundingRectMultiLine(const std::wstring& text, QFont fnt, size_t& nbLines);
    //=============================================================================
    void
    setLineStyle(QPen& _pen, const std::wstring& style);
    //=============================================================================
    void
    toPixelsImpl(
        double x, double y, double z, double& xclip, double& yclip, double& zclip, bool& clipped);
    //=============================================================================
public:
    //=============================================================================
    virtual bool
    getGraphicsSmoothing() override;
    //=============================================================================
    virtual void
    setGraphicsSmoothing(bool on) override;
    //=============================================================================
    virtual double
    width() override
    {
        return m_width;
    }
    //=============================================================================
    virtual double
    height() override
    {
        return m_height;
    }
    //=============================================================================
    void
    debug() override;
    //=============================================================================
    RenderQt(QPainter* painter, double x1, double y1, double width, double height,
        const std::wstring& renderName);
    //=============================================================================
    ~RenderQt() override;
    //=============================================================================
    std::wstring
    getRenderName() override;
    //=============================================================================
    void clear(std::vector<double>) override;
    //=============================================================================
    void
    toPixels(double x, double y, double z, int& a, int& b) override;
    //=============================================================================
    void
    toPixels(double x, double y, double z, double& a, double& b) override;
    //=============================================================================
    void
    toPixels(double x, double y, double z, double& a, double& b, bool& clipped) override;
    //=============================================================================
    virtual void
    toPixels(double x, double y, double z, double& a, double& b, double& c, bool& clipped);
    //=============================================================================
    void
    lookAt(double px, double py, double pz, double tx, double ty, double tz, double ux, double uy,
        double uz) override;
    //=============================================================================
    void
    scale(double sx, double sy, double sz) override;
    //=============================================================================
    void
    mapPoint(double x, double y, double z, double& a, double& b, double& c) override;
    //=============================================================================
    void
    project(double xmin, double xmax, double ymin, double ymax, double zmin, double zmax) override;
    //=============================================================================
    void
    viewport(double x0, double y0, double width, double height) override;
    //=============================================================================
    void
    quad(double x1, double y1, double z1, double x2, double y2, double z2, double x3, double y3,
        double z3, double x4, double y4, double z4) override;
    //=============================================================================
    void
    quadline(double x1, double y1, double z1, double x2, double y2, double z2, double x3, double y3,
        double z3, double x4, double y4, double z4) override;
    //=============================================================================
    void
    tri(double x1, double y1, double z1, double x2, double y2, double z2, double x3, double y3,
        double z3) override;
    //=============================================================================
    void
    triLine(double x1, double y1, double z1, double x2, double y2, double z2, double x3, double y3,
        double z3) override;
    //=============================================================================
    void
    color(const std::vector<double>) override;
    //=============================================================================
    void
    setLineStyle(const std::wstring& style) override;
    //=============================================================================
    void
    lineWidth(double n) override;
    //=============================================================================
    void
    line(double x1, double y1, double z1, double x2, double y2, double z2) override;
    //=============================================================================
    void
    line(double x1, double y1, double x2, double y2) override;
    //=============================================================================
    void
    lineSeries(std::vector<double> xs, std::vector<double> ys, std::vector<double> zs) override;
    //=============================================================================
    void
    setupDirectDraw() override;
    //=============================================================================
    void
    releaseDirectDraw() override;
    //=============================================================================
    void
    getModelviewMatrix(std::vector<double>& model) override;
    //=============================================================================
    void
    putText(double x, double y, std::wstring txt, std::vector<double> color, AlignmentFlag xflag,
        AlignmentFlag yflag, QFont fnt, double rotation) override;
    //=============================================================================
    void
    measureText(std::wstring txt, QFont fnt, AlignmentFlag xflag, AlignmentFlag yflag, int& width,
        int& height, int& xoffset, int& yoffset) override;
    //=============================================================================
    void
    depth(bool) override;
    //=============================================================================
    void
    rect(double x1, double y1, double x2, double y2) override;
    //=============================================================================
    void
    rectFill(double x1, double y1, double x2, double y2) override;
    //=============================================================================
    void
    circle(double x1, double y1, double radius) override;
    //=============================================================================
    void
    circleFill(double x1, double y1, double radius) override;
    //=============================================================================
    void
    drawImage(int x1, int y1, QImage pic) override;
    //=============================================================================
    void
    drawImage(double x1, double y1, double x2, double y2, QImage pic) override;
    //=============================================================================
    void
    drawImage(const std::vector<double>& xp, const std::vector<double>& yp,
        const std::vector<double>& xLim, const std::vector<double>& yLim, bool xFlip, bool yFlip,
        QImage pic);
    //=============================================================================
    void
    quadStrips(std::vector<std::vector<coloredPoint>> faces, bool flatfaces,
        std::vector<std::vector<coloredPoint>> edges, bool flatedges, meshStyle meshstyle,
        double lineWidth, const std::wstring& lineStyle) override;
    //=============================================================================
    void
    drawPatch(const FaceList& faces, double lineWidth, const std::wstring& lineStyle);
    //=============================================================================
};
//=============================================================================
