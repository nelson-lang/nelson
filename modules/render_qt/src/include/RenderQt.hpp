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
#include <array>
#include <QtGui/QPainter>
#include <QtGui/QFont>
#include <QtGui/QPainterPath>
#include <QtGui/QColor>
#include <QtCore/QPointF>
#include "RenderInterface.hpp"
#include "nlsRenderQt_exports.h"
//=============================================================================
struct point3D
{
    double x;
    double y;
    double z;
};
//=============================================================================
struct quad3D
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
    double vr[4];
    double vg[4];
    double vb[4];
    double va[4];
    // Per-vertex texture coordinates, used only when a surface texture is bound
    // (FaceColor 'texturemap'); the vertex colours then carry baked lighting.
    double u[4] { 0, 0, 0, 0 };
    double v[4] { 0, 0, 0, 0 };
    // Index into RenderQt's per-paint texture store, or -1 when not textured. A
    // stored index (not the live texture) survives the deferred draw of a 3-D
    // surface group, which happens after clearSurfaceTexture.
    int textureIndex { -1 };
    bool interpolateFill;
    meshStyle meshstyle;
    double lineWidth;
    std::wstring lineStyle;
};
//=============================================================================
enum class RenderQtBspPrimitiveType
{
    Polygon,
    Polyline
};
//=============================================================================
struct RenderQtBspVertex
{
    point eye;
    point clip;
    QPointF pixel;
    RGBAColorData color;
    bool hasColor = false;
};
//=============================================================================
struct RenderQtBspPrimitive
{
    RenderQtBspPrimitiveType type;
    std::vector<RenderQtBspVertex> vertices;
    QColor fillColor;
    QColor lineColor;
    double lineWidth;
    std::wstring lineStyle;
    size_t order;
    double meanEyeZ;
    bool closed;
    bool interpolateFill = false;
};
//=============================================================================
class NLSRENDERQT_IMPEXP RenderQt : public RenderInterface
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
    // frames saved by pushRegion, innermost last
    struct SavedRegion
    {
        double x1, y1, width, height;
    };
    std::vector<SavedRegion> regions;
    // Saved modelview matrices for the transform-group (hgtransform) stack.
    std::vector<std::array<double, 16>> m_modelStack;
    // Data clipping is suspended while a transform is active (see RenderWeb).
    std::vector<char> m_dataClipEnabledStack;
    // keep the painter clip inside the innermost region, whatever the
    // drawing code asks for
    void
    clipToRegion();
    QPainter* pnt;
    bool inDirect;
    bool debugFlag;
    bool deferSurfaceQuads;
    int surfaceGroupDepth;
    std::vector<quad3D> deferredSurfaceQuads;
    bool collectBspPrimitives;
    bool savedCollectBspPrimitives;
    size_t bspPrimitiveOrder;
    std::vector<RenderQtBspPrimitive> bspPrimitives;
    double currentLineWidth;
    std::wstring currentLineStyle;
    RenderLightingScene lightingScene;
    RenderLightingMaterial lightingMaterial;
    RenderClipBox m_dataClip;
    // Surface textures bound by setSurfaceTexture during this paint, kept so a
    // deferred 3-D surface group can still sample them after clearSurfaceTexture.
    // RenderQt is constructed per paint, so the store is naturally bounded.
    std::vector<QImage> m_textureStore;
    int m_currentTextureIndex { -1 };
    bool m_surfaceTextureActive { false };
    //=============================================================================
    QPointF
    map(double x, double y, double z);
    //=============================================================================
    QVector<QPointF>
    map(const std::vector<double>& xs, const std::vector<double>& ys,
        const std::vector<double>& zs);
    //=============================================================================
    std::vector<quad3D>
    mapQuads(const std::vector<std::vector<coloredPoint>>& faces,
        const std::vector<std::vector<coloredPoint>>& edges, bool interpolateFaces);
    //=============================================================================
    std::vector<quad3D>
    mapQuadsFacesOnly(const std::vector<std::vector<coloredPoint>>& faces, bool interpolateFaces);
    //=============================================================================
    std::vector<quad3D>
    mapQuadsEdgesOnly(const std::vector<std::vector<coloredPoint>>& edges);
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
    // Convert a LineWidth (in points) to a device pixel stroke width, clamped so a
    // thin line never renders below one pixel. Mirrors lineWidth() so surface/quad
    // edges, drawn through their own pens, match the weight of every other line.
    double
    edgeStrokePixelWidth(double points) const;
    //=============================================================================
    void
    drawMappedQuad(const quad3D& mapqd);
    //=============================================================================
    RenderQtBspVertex
    bspVertex(double x, double y, double z, bool& clipped);
    //=============================================================================
    void
    appendBspPrimitive(RenderQtBspPrimitive primitive);
    //=============================================================================
    void
    appendBspPolygon(const std::vector<point>& vertices, const QColor& fillColor,
        const QColor& lineColor, double lineWidth, const std::wstring& lineStyle);
    //=============================================================================
    void
    appendBspInterpolatedPolygon(const std::vector<point>& vertices,
        const std::vector<RGBAColorData>& vertexColors, const QColor& lineColor, double lineWidth,
        const std::wstring& lineStyle);
    //=============================================================================
    void
    appendBspLine(const point& first, const point& second, const QColor& color, double lineWidth,
        const std::wstring& lineStyle);
    //=============================================================================
    void
    flushBspPrimitives();
    //=============================================================================
    void
    drawBspPrimitive(const RenderQtBspPrimitive& primitive);
    //=============================================================================
    void
    drawInterpolatedTriangle(const QPointF& p0, const QPointF& p1, const QPointF& p2,
        const RGBAColorData& c0, const RGBAColorData& c1, const RGBAColorData& c2);
    //=============================================================================
    // Same rasterization as drawInterpolatedTriangle, but each fragment samples the
    // bound surface texture at its interpolated (u, v); the interpolated vertex
    // colour (baked lighting, white otherwise) modulates the texel. Keeps the
    // texture crisp independently of the mesh resolution.
    void
    drawTexturedTriangle(const QImage& tex, const QPointF& p0, const QPointF& p1, const QPointF& p2,
        const double uv0[2], const double uv1[2], const double uv2[2], const RGBAColorData& c0,
        const RGBAColorData& c1, const RGBAColorData& c2);
    //=============================================================================
    RGBAColorData
    applyLighting(
        const RGBAColorData& color, const point& normal, const point& vertex, bool enabled) const;
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
    bool
    pushRegion(double x, double y, double width, double height) override;
    //=============================================================================
    void
    popRegion() override;
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
    projectPerspective(
        double left, double right, double bottom, double top, double znear, double zfar) override;
    //=============================================================================
    void
    viewport(double x0, double y0, double width, double height) override;
    //=============================================================================
    void
    clipToEntireFrame() override;
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
    bool
    polygonFill(const std::vector<std::vector<double>>& points,
        const std::vector<double>& faceColor) override;
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
    double
    pointsToPixels(double points) override;
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
    setClipBox(double x0, double y0, double width, double height) override;
    //=============================================================================
    void
    clearClipBox() override;
    //=============================================================================
    void
    setDataClipBox(const RenderClipBox& box) override;
    //=============================================================================
    void
    clearDataClipBox() override;
    //=============================================================================
    void
    pushModelMatrix(const double* matrix16ColumnMajor) override;
    //=============================================================================
    void
    popModelMatrix() override;
    //=============================================================================
    void
    setSurfaceTexture(const RGBAImageView& texture) override;
    //=============================================================================
    void
    clearSurfaceTexture() override;
    //=============================================================================
    bool
    supportsSurfaceTexture() override
    {
        return true;
    }
    //=============================================================================
    void
    getModelviewMatrix(std::vector<double>& model) override;
    //=============================================================================
    void
    putText(double x, double y, std::wstring txt, std::vector<double> color, AlignmentFlag xflag,
        AlignmentFlag yflag, const FontSpec& fnt, double rotation) override;
    //=============================================================================
    void
    measureText(std::wstring txt, const FontSpec& fnt, AlignmentFlag xflag, AlignmentFlag yflag,
        int& width, int& height, int& xoffset, int& yoffset) override;
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
    drawPieSlice(double centerX, double centerY, double outerRadius, double innerRadius,
        double startAngle, double spanAngle, const std::vector<double>& faceColor,
        const std::vector<double>& edgeColor, double lineWidth) override;
    //=============================================================================
    void
    drawImage(int x1, int y1, const RGBAImageView& pic) override;
    //=============================================================================
    void
    drawImage(double x1, double y1, double x2, double y2, const RGBAImageView& pic) override;
    //=============================================================================
    void
    drawImage(const std::vector<double>& xp, const std::vector<double>& yp,
        const std::vector<double>& xLim, const std::vector<double>& yLim, bool xFlip, bool yFlip,
        const RGBAImageView& pic) override;
    //=============================================================================
    void
    setLightingScene(const RenderLightingScene& scene) override;
    //=============================================================================
    void
    setLightingMaterial(const RenderLightingMaterial& material) override;
    //=============================================================================
    void
    beginSurfaceGroup() override;
    //=============================================================================
    void
    endSurfaceGroup() override;
    //=============================================================================
    void
    quadStrips(const std::vector<std::vector<coloredPoint>>& faces, bool flatfaces,
        const std::vector<std::vector<coloredPoint>>& edges, bool flatedges, meshStyle meshstyle,
        double lineWidth, const std::wstring& lineStyle,
        enum LightingMode::LightingMode faceLighting = LightingMode::None,
        enum LightingMode::LightingMode edgeLighting = LightingMode::None,
        uint64_t geometryToken = 0) override;
    //=============================================================================
    void
    drawPatch(const FaceList& faces, const PatchRenderOptions& options) override;
    //=============================================================================
};
//=============================================================================
