//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <vector>
#include <string>
#include <QtGui/QFont>
#include "ColoredPoint.hpp"
#include "RGBAColorData.hpp"
//=============================================================================
enum meshStyle
{
    Row,
    Column,
    Both
};
//=============================================================================
namespace ColorMode {
enum ColorMode
{
    ColorSpec,
    None,
    Flat,
    Interp
};
}
//=============================================================================
namespace AlphaMode {
enum AlphaMode
{
    Scalar,
    Flat,
    Interp
};
}
namespace EraseMode {
enum EraseMode
{
    Normal,
    None,
    Xor,
    Background
};
}
namespace LightingMode {
enum LightingMode
{
    None,
    Flat,
    Gouraud,
    Phong
};
}

class point
{
public:
    double x;
    double y;
    double z;
    point()
    {
        x = 0;
        y = 0;
        z = 0;
    };
    point(double X, double Y, double Z) : x(X), y(Y), z(Z) {};
};
//=============================================================================
class Face
{
public:
    std::vector<point> vertices;
    std::vector<RGBAColorData> edgecolors;
    std::vector<RGBAColorData> vertexcolors;
    RGBAColorData FaceColor;
    RGBAColorData EdgeColor;
    enum ColorMode::ColorMode FaceColorMode;
    enum ColorMode::ColorMode EdgeColorMode;
};
//=============================================================================
using FaceList = std::vector<Face>;
//=============================================================================

class RenderInterface
{
public:
    //=============================================================================
    enum AlignmentFlag
    {
        Min,
        Mean,
        Max
    };
    enum SymbolType
    {
        Plus,
        Circle,
        Star,
        Dot,
        Times,
        Square,
        Diamond,
        Up,
        Down,
        Right,
        Left,
        Pentagram,
        Hexagram,
        None
    };
    //=============================================================================
    virtual std::wstring
    getRenderName()
        = 0;
    //=============================================================================
    virtual bool
    getGraphicsSmoothing()
        = 0;
    //=============================================================================
    virtual void
    setGraphicsSmoothing(bool on)
        = 0;
    //=============================================================================
    virtual void
    debug()
        = 0;
    //=============================================================================
    RenderInterface() = default;
    //=============================================================================
    virtual ~RenderInterface() = default;
    //=============================================================================
    virtual double
    width()
        = 0;
    //=============================================================================
    virtual double
    height()
        = 0;
    //=============================================================================
    virtual void clear(std::vector<double>) = 0;
    //=============================================================================
    virtual void
    toPixels(double x, double y, double z, int& a, int& b)
        = 0;
    //=============================================================================
    virtual void
    toPixels(double x, double y, double z, double& a, double& b)
        = 0;
    //=============================================================================
    virtual void
    toPixels(double x, double y, double z, double& a, double& b, bool& clipped)
        = 0;
    //=============================================================================
    virtual void
    lookAt(double px, double py, double pz, double tx, double ty, double tz, double ux, double uy,
        double uz)
        = 0;
    //=============================================================================
    virtual void
    scale(double sx, double sy, double sz)
        = 0;
    //=============================================================================
    virtual void
    mapPoint(double x, double y, double z, double& a, double& b, double& c)
        = 0;
    //=============================================================================
    virtual void
    project(double xmin, double xmax, double ymin, double ymax, double zmin, double zmax)
        = 0;
    //=============================================================================
    virtual void
    viewport(double x0, double y0, double width, double height)
        = 0;
    //=============================================================================
    virtual void
    quad(double x1, double y1, double z1, double x2, double y2, double z2, double x3, double y3,
        double z3, double x4, double y4, double z4)
        = 0;
    //=============================================================================
    virtual void
    quadline(double x1, double y1, double z1, double x2, double y2, double z2, double x3, double y3,
        double z3, double x4, double y4, double z4)
        = 0;
    //=============================================================================
    virtual void
    tri(double x1, double y1, double z1, double x2, double y2, double z2, double x3, double y3,
        double z3)
        = 0;
    //=============================================================================
    virtual void
    triLine(double x1, double y1, double z1, double x2, double y2, double z2, double x3, double y3,
        double z3)
        = 0;
    //=============================================================================
    virtual void color(std::vector<double>) = 0;
    //=============================================================================
    virtual void
    setLineStyle(const std::wstring& style)
        = 0;
    //=============================================================================
    void
    color(double r, double g, double b)
    {
        std::vector<double> t;
        t.push_back(r);
        t.push_back(g);
        t.push_back(b);
        color(t);
    }
    //=============================================================================
    virtual void
    lineWidth(double n)
        = 0;
    //=============================================================================
    virtual void
    line(double x1, double y1, double z1, double x2, double y2, double z2)
        = 0;
    virtual void
    line(double x1, double y1, double x2, double y2)
        = 0;
    //=============================================================================
    virtual void
    lineSeries(std::vector<double> xs, std::vector<double> ys, std::vector<double> zs)
        = 0;
    virtual void
    setupDirectDraw()
        = 0;
    //=============================================================================
    virtual void
    releaseDirectDraw()
        = 0;
    //=============================================================================
    virtual void
    getModelviewMatrix(std::vector<double>& model)
        = 0;
    virtual void
    putText(double x, double y, std::wstring txt, std::vector<double> color, AlignmentFlag xflag,
        AlignmentFlag yflag, QFont fnt, double rotation)
        = 0;
    //=============================================================================
    virtual void
    measureText(std::wstring txt, QFont fnt, AlignmentFlag xflag, AlignmentFlag yflag, int& width,
        int& height, int& xoffset, int& yoffset)
        = 0;
    //=============================================================================
    virtual void
    depth(bool)
        = 0;
    //=============================================================================
    virtual void
    rect(double x1, double y1, double x2, double y2)
        = 0;
    //=============================================================================
    virtual void
    rectFill(double x1, double y1, double x2, double y2)
        = 0;
    //=============================================================================
    virtual void
    circle(double x1, double y1, double radius)
        = 0;
    //=============================================================================
    virtual void
    circleFill(double x1, double y1, double radius)
        = 0;
    //=============================================================================
    virtual void
    drawImage(int x1, int y1, QImage pic)
        = 0;
    //=============================================================================
    virtual void
    drawImage(double x1, double y1, double x2, double y2, QImage pic)
        = 0;
    //=============================================================================
    virtual void
    drawImage(const std::vector<double>& xp, const std::vector<double>& yp,
        const std::vector<double>& xLim, const std::vector<double>& yLim, bool xFlip, bool yFlip,
        QImage pic)
        = 0;
    //=============================================================================
    virtual void
    quadStrips(std::vector<std::vector<coloredPoint>> faces, bool flatfaces,
        std::vector<std::vector<coloredPoint>> edges, bool flatedges, meshStyle meshstyle,
        double lineWidth, const std::wstring& lineStyle)
        = 0;
    //=============================================================================
    virtual void
    drawPatch(const FaceList& faces, double lineWidth, const std::wstring& lineStyle)
        = 0;
    //=============================================================================
};
//=============================================================================
