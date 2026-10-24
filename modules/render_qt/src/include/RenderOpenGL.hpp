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
#include <unordered_map>
#include <unordered_set>
#include <QtCore/QPointer>
#include <QtGui/QFont>
#include <QtGui/QImage>
#include <QtCore/QtGlobal>
#include <QtOpenGLWidgets/QOpenGLWidget>
#include "RenderInterface.hpp"
#include "nlsRenderQt_exports.h"
//=============================================================================
class QOpenGLContext;
//=============================================================================
// Flattened client-side vertex arrays for one patch's cacheable draw: triangle
// soup for the faces and a de-duplicated line list for the edges, each as
// tightly packed position (3 floats) and RGBA colour (4 floats) streams drawn
// with glDrawArrays.
struct PatchVertexArrays
{
    std::vector<float> facePos;
    std::vector<float> faceColor;
    std::vector<float> edgePos;
    std::vector<float> edgeColor;
};
//=============================================================================
// Same idea for an opaque, unlit surface: the face quads and the mesh lines as
// tightly packed float position (3) + RGBA (4) streams, built once per
// geometryToken. The GL_DOUBLE arrays interleaved over the 96-byte coloredPoint
// would re-send tens of megabytes per frame.
struct SurfaceVertexArrays
{
    std::vector<float> facePos;
    std::vector<float> faceColor;
    std::vector<float> edgePos;
    std::vector<float> edgeColor;
};
//=============================================================================
class NLSRENDERQT_IMPEXP RenderOpenGL : public RenderInterface
{
private:
    std::wstring renderName;
    double model[16];
    double proj[16];
    int viewp[4];
    double save_model[16];
    double save_proj[16];
    int save_viewp[4];
    // Saved modelview matrices for the transform-group (hgtransform) stack.
    std::vector<std::array<double, 16>> m_modelStack;
    // Data clipping is suspended while a transform is active (see RenderWeb).
    std::vector<char> m_dataClipEnabledStack;
    double m_x1 = 0;
    double m_y1 = 0;
    double m_width = 0;
    double m_height = 0;
    QOpenGLWidget* widget = nullptr;
    bool graphicsSmoothing = true;
    bool inDirect = false;
    bool debugFlag = false;
    double currentColor[4] = { 1, 1, 1, 1 };
    struct TextureCacheEntry
    {
        unsigned int texture = 0;
        int width = 0;
        int height = 0;
    };
    struct ImageTextureSlot
    {
        unsigned int texture = 0;
        int width = 0;
        int height = 0;
    };
    struct ImageTextureCacheEntry
    {
        std::array<ImageTextureSlot, 3> textures;
        size_t activeSlot = 0;
        uint64_t revision = 0;
        uint64_t lastUse = 0;
    };
    std::unordered_map<std::wstring, TextureCacheEntry> textTextureCache;
    std::unordered_map<uint64_t, ImageTextureCacheEntry> imageTextureCache;
    QPointer<QOpenGLContext> imageTextureContext;
    uint64_t imageTextureUse = 0;
    std::unordered_map<uint64_t, unsigned int> surfaceDisplayListCache;
    std::unordered_map<uint64_t, unsigned int> patchDisplayListCache;
    std::unordered_set<uint64_t> surfaceDisplayListCandidates;
    std::unordered_set<uint64_t> patchDisplayListCandidates;
    // Client-side vertex arrays (position + RGBA) for the cacheable patch case,
    // keyed by geometryToken. Legacy display lists are not GPU-resident on common
    // drivers and replay the per-vertex command stream each frame.
    std::unordered_map<uint64_t, PatchVertexArrays> patchVertexArrayCache;
    std::unordered_map<uint64_t, SurfaceVertexArrays> surfaceVertexArrayCache;
    RenderLightingScene lightingScene;
    RenderLightingMaterial lightingMaterial;
    RenderClipBox m_dataClip;
    // Surface texture bound by setSurfaceTexture, sampled by GL while FaceColor is
    // 'texturemap'. RGBA8 row-major; uploaded to a transient GL texture per draw.
    std::vector<unsigned char> m_surfaceTextureRgba;
    int m_surfaceTextureW = 0;
    int m_surfaceTextureH = 0;
    bool m_surfaceTextureActive = false;

    // Enable/disable the six OpenGL clip planes that bound the axes data limits
    // (the mapped clip cuboid). Planes are specified with the axes modelview
    // loaded so hardware clipping keeps only geometry inside the axes box.
    void
    applyDataClipPlanes();
    void
    disableDataClipPlanes();

    void
    resetState(double x1, double y1, double width, double height, const std::wstring& name);
    // Device pixel ratio of the backing window. The GL frame buffer is allocated at
    // device resolution, so point-based sizes (markers, line widths) are multiplied
    // by this ratio. Returns 1.0 when no widget is attached.
    double
    deviceScale() const;
    void
    clearTextTextureCache();
    void
    clearImageTextureCache();
    unsigned int
    imageTexture(const RGBAImageView& image);
    void
    clearDisplayListCaches();
    bool
    shouldCompileDisplayList(std::unordered_set<uint64_t>& candidates, uint64_t cacheKey);
    void
    restoreCurrentColor();
    void
    loadModelMatrix();
    void
    loadProjectionMatrix();
    void
    toPixelsImpl(
        double x, double y, double z, double& xclip, double& yclip, double& zclip, bool& clipped);
    QPointF
    map(double x, double y, double z);
    void
    setLineStyleGL(const std::wstring& style);
    void
    configureLighting(bool enabled);
    void
    disableLighting();
    std::vector<std::wstring>
    splitEoLText(const std::wstring& text);
    QRect
    boundingRectMultiLine(const std::wstring& text, QFont fnt, size_t& nbLines);

public:
    RenderOpenGL(QOpenGLWidget* glWidget, double x1, double y1, double width, double height,
        const std::wstring& renderName);
    ~RenderOpenGL() override;
    void
    resetSurface(double x1, double y1, double width, double height, const std::wstring& name);

    std::wstring
    getRenderName() override;
    bool
    getGraphicsSmoothing() override;
    void
    setGraphicsSmoothing(bool on) override;
    void
    debug() override;
    double
    width() override;
    double
    height() override;
    void clear(std::vector<double>) override;
    void
    clearDepthBuffer() override;
    void
    toPixels(double x, double y, double z, int& a, int& b) override;
    void
    toPixels(double x, double y, double z, double& a, double& b) override;
    void
    toPixels(double x, double y, double z, double& a, double& b, bool& clipped) override;
    void
    toPixels(double x, double y, double z, double& a, double& b, double& c, bool& clipped) override;
    void
    lookAt(double px, double py, double pz, double tx, double ty, double tz, double ux, double uy,
        double uz) override;
    void
    scale(double sx, double sy, double sz) override;
    void
    mapPoint(double x, double y, double z, double& a, double& b, double& c) override;
    void
    project(double xmin, double xmax, double ymin, double ymax, double zmin, double zmax) override;
    void
    projectPerspective(
        double left, double right, double bottom, double top, double znear, double zfar) override;
    void
    viewport(double x0, double y0, double width, double height) override;
    void
    clipToEntireFrame() override;
    void
    quad(double x1, double y1, double z1, double x2, double y2, double z2, double x3, double y3,
        double z3, double x4, double y4, double z4) override;
    void
    quadline(double x1, double y1, double z1, double x2, double y2, double z2, double x3, double y3,
        double z3, double x4, double y4, double z4) override;
    void
    tri(double x1, double y1, double z1, double x2, double y2, double z2, double x3, double y3,
        double z3) override;
    void
    triLine(double x1, double y1, double z1, double x2, double y2, double z2, double x3, double y3,
        double z3) override;
    void color(std::vector<double>) override;
    void
    setLineStyle(const std::wstring& style) override;
    void
    lineWidth(double n) override;
    double
    pointsToPixels(double points) override;
    void
    line(double x1, double y1, double z1, double x2, double y2, double z2) override;
    void
    line(double x1, double y1, double x2, double y2) override;
    void
    lineSegments3D(const std::vector<double>& xyz) override;
    void
    lineSegments2D(const std::vector<double>& xy) override;
    void
    lineSeries(std::vector<double> xs, std::vector<double> ys, std::vector<double> zs) override;
    bool
    pointMarkers(const std::vector<double>& xs, const std::vector<double>& ys,
        const std::vector<double>& markerSizes, const std::vector<double>& rgbaColors) override;
    void
    setupDirectDraw() override;
    void
    releaseDirectDraw() override;
    void
    setClipBox(double x0, double y0, double width, double height) override;
    void
    clearClipBox() override;
    void
    setDataClipBox(const RenderClipBox& box) override;
    void
    clearDataClipBox() override;
    void
    pushModelMatrix(const double* matrix16ColumnMajor) override;
    void
    popModelMatrix() override;
    void
    setSurfaceTexture(const RGBAImageView& texture) override;
    void
    clearSurfaceTexture() override;
    bool
    supportsSurfaceTexture() override
    {
        return true;
    }
    void
    getModelviewMatrix(std::vector<double>& model) override;
    void
    putText(double x, double y, std::wstring txt, std::vector<double> color, AlignmentFlag xflag,
        AlignmentFlag yflag, const FontSpec& fnt, double rotation) override;
    void
    measureText(std::wstring txt, const FontSpec& fnt, AlignmentFlag xflag, AlignmentFlag yflag,
        int& width, int& height, int& xoffset, int& yoffset) override;
    void
    depth(bool) override;
    void
    rect(double x1, double y1, double x2, double y2) override;
    void
    rectFill(double x1, double y1, double x2, double y2) override;
    void
    circle(double x1, double y1, double radius) override;
    void
    circleFill(double x1, double y1, double radius) override;
    bool
    markerBatch(int shape, const std::vector<double>& xs, const std::vector<double>& ys,
        const std::vector<double>& markerSizes, const std::vector<double>& faceRgba,
        const std::vector<double>& edgeRgba, double edgeWidth,
        const std::vector<double>& depths = {}) override;
    bool
    rectBatch(const std::vector<double>& corners, const std::vector<double>& faceColor,
        const std::vector<double>& edgeColor, double edgeWidth) override;
    // Vertex count of the disc circle and circleFill draw for a radius in pixels.
    static int
    circleSegmentCount(double radius);
    void
    drawPieSlice(double centerX, double centerY, double outerRadius, double innerRadius,
        double startAngle, double spanAngle, const std::vector<double>& faceColor,
        const std::vector<double>& edgeColor, double lineWidth) override;
    void
    drawImage(int x1, int y1, const RGBAImageView& pic) override;
    void
    drawImage(double x1, double y1, double x2, double y2, const RGBAImageView& pic) override;
    void
    drawImage(const std::vector<double>& xp, const std::vector<double>& yp,
        const std::vector<double>& xLim, const std::vector<double>& yLim, bool xFlip, bool yFlip,
        const RGBAImageView& pic) override;
    void
    setLightingScene(const RenderLightingScene& scene) override;
    void
    setLightingMaterial(const RenderLightingMaterial& material) override;
    void
    quadStrips(const std::vector<std::vector<coloredPoint>>& faces, bool flatfaces,
        const std::vector<std::vector<coloredPoint>>& edges, bool flatedges, meshStyle meshstyle,
        double lineWidth, const std::wstring& lineStyle,
        enum LightingMode::LightingMode faceLighting = LightingMode::None,
        enum LightingMode::LightingMode edgeLighting = LightingMode::None,
        uint64_t geometryToken = 0) override;
    void
    drawPatch(const FaceList& faces, const PatchRenderOptions& options) override;
};
//=============================================================================
