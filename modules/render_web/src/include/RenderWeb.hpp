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
#if _MSC_VER
#pragma warning(disable : 4251 4275)
#endif
//=============================================================================
#include <array>
#include <cstdint>
#include <string>
#include <vector>
#include "RenderInterface.hpp"
#include "nlsRenderWeb_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Records the figure as a flat list of 2D draw primitives that the web frontend
// replays on a canvas. It uses the same orthographic projection as
// RenderQt/RenderOpenGL. The list is Qt-free and JSON-serializable; text metrics
// come from the font_metrics module.
//=============================================================================
struct WebDrawPrimitive
{
    std::string kind; // "rect" | "line" | "segments" | "polyline" | "polygon" | "text"
    double x = 0, y = 0, w = 0, h = 0;
    double x1 = 0, y1 = 0, x2 = 0, y2 = 0;
    std::vector<std::pair<double, double>> points;
    std::string color;
    std::string stroke; // polygon outline (surface mesh); empty = no outline
    std::vector<double> dash; // dash pattern in pixels; empty = solid
    double width = 1.0;
    std::string text;
    std::string image; // "data:image/png;base64,..." for kind == "image"
    std::vector<unsigned char> imageRgba; // tightly packed RGBA8 for browser animation frames
    int imageWidth = 0;
    int imageHeight = 0;
    // The web animation transport can send a raster payload once, then reference
    // it by this stable content token. Plain display lists keep imagePayload true.
    std::string imageToken;
    bool imagePayload = true;
    std::string align = "left";
    double rotation = 0.0; // degrees, counter-clockwise (e.g. ylabel)
    double size = 12.0;
    // Text font: CSS generic family + style, so every consumer (browser canvas,
    // SVG, PDF, software rasterizer) renders with the font the figure asked for.
    std::string fontFamily = "sans-serif"; // "sans-serif" | "serif" | "monospace"
    bool fontBold = false;
    bool fontItalic = false;
    double depth = 0.0; // mean clip-space z, for painter-ordering polygons
    // A stroke that outlines an opaque 3D solid (a histogram2 bar edge), tagged
    // while drawing inside a surface group. flush3d sorts it by its true depth
    // with no camera-ward bias, so it is hidden by any nearer opaque face.
    bool solidEdge = false;
    bool webglFallback = false; // retained for software capture, skipped by WebGL display
    // kind == "points": a batch of markers of one shape, in screen pixels.
    // pointColors is the per-point fill and pointEdgeColors the per-point outline
    // (empty = none); a marker may have one, the other, or both. pointShape is a
    // RenderInterface::SymbolType.
    int pointShape = 0;
    std::vector<float> pointCenters; // 2N interleaved: x0,y0,x1,y1,...
    std::vector<float> pointRadii; // N marker half-sizes (outer radius)
    std::vector<uint8_t> pointColors; // 4N packed RGBA fill (empty = no fill)
    std::vector<uint8_t> pointEdgeColors; // 4N packed RGBA outline (empty = no outline)
    double pointEdgeWidth = 1.0; // outline stroke width in pixels
    // kind == "rects": a batch of axis-aligned rectangles (bar / histogram bars)
    // in screen pixels, 4 floats per rect. They share one fill (`color`) and one
    // outline (`stroke`, width `width`).
    std::vector<float> rectXYWH; // 4N: x0,y0,w0,h0,x1,y1,w1,h1,...
};
//=============================================================================
struct WebGlMesh
{
    std::vector<float> positions;
    std::vector<float> vertexColors;
    std::vector<uint32_t> triangleIndices;
    std::vector<uint32_t> lineIndices;
    size_t positionsLength = 0;
    size_t vertexColorsLength = 0;
    size_t triangleIndicesLength = 0;
    size_t lineIndicesLength = 0;
    std::array<double, 16> model { };
    std::array<double, 16> projection { };
    std::array<int, 4> viewport { };
    std::array<float, 4> lineColor { 0, 0, 0, 1 };
    double lineWidth = 1.0;
    bool drawFaces = false;
    uint64_t revision = 0;
};
//=============================================================================
// A uicontrol as the web frontend builds it: a real HTML control, not a snapshot
// of the Qt widget.
struct WebUIControl
{
    long long handle = 0;
    std::string style; // pushbutton, togglebutton, checkbox, radiobutton, edit,
                       // text, slider, popupmenu, listbox
    std::string text;
    std::vector<std::string> items; // popupmenu / listbox entries
    std::string tooltip;
    std::string background;
    std::string foreground;
    std::string units;
    double x = 0, y = 0, w = 0, h = 0; // screen pixels, top-left origin
    double value = 0, minimum = 0, maximum = 1;
    double fontSize = 12.0;
    bool enabled = true;
    // Enable == 'inactive': the control renders normally (not greyed) but does
    // not react to user input, while ButtonDownFcn still fires. When true,
    // enabled is also true so the widget keeps its normal appearance.
    bool inactive = false;
    // pushbutton / togglebutton label placement
    std::string horizontalAlignment = "center"; // left, center, right
    std::string verticalAlignment = "center"; // top, center, bottom
    std::string extraJson; // rich payload (uitable/uitree) as a JSON object
};
//=============================================================================
// A vertex of the software z-buffer raster: screen pixel position (top-left
// origin, possibly oversampled), clip-space depth (nearer = smaller), straight
// alpha colour, and the texture coordinate of a 'texturemap' surface.
struct WebSceneVertex
{
    double x = 0;
    double y = 0;
    double z = 0;
    double r = 0;
    double g = 0;
    double b = 0;
    double a = 1;
    double u = 0;
    double v = 0;
};
//=============================================================================
// A translucent patch face (one finite run of its outline), captured in figure
// pixels and already triangulated when the patch was drawn. It is rasterized at
// the end of the 3D scene (RenderWeb::flushDeferredTranslucent) so it is
// depth-tested against every opaque surface and patch of the axes whatever the
// draw order, and blended back-to-front with the other translucent faces. Each
// pixel of a face is blended once, however many triangles cover it.
struct WebDeferredTranslucentPolygon
{
    std::vector<WebSceneVertex> vertices;
    std::vector<std::array<std::size_t, 3>> triangles; // indices into vertices
    double depth = 0.0; // mean clip-space z
    bool webglFallback = false;
};
//=============================================================================
struct WebDeferredTranslucentEdge
{
    WebSceneVertex a;
    WebSceneVertex b;
    double halfWidth = 0.5; // figure pixels
    bool webglFallback = false;
};
//=============================================================================
class NLSRENDERWEB_IMPEXP RenderWeb : public RenderInterface
{
public:
    RenderWeb(double x1, double y1, double width, double height,
        bool softwareFallbackEnabled = true, bool encodeImagePngPayloads = true);
    ~RenderWeb() override = default;

    // Emits any buffered 3D primitives (depth-sorted). Called automatically
    // before direct draw; call once after painting to flush the tail.
    void
    flush3d();

    // RenderInterface hook: flush the deferred queue so later primitives draw on top.
    void
    flushDeferred() override
    {
        flush3d();
    }

    const std::vector<WebUIControl>&
    controls() const
    {
        return m_controls;
    }
    void
    addControl(const WebUIControl& control)
    {
        m_controls.push_back(control);
    }

    const std::vector<WebDrawPrimitive>&
    primitives() const
    {
        return m_prims;
    }
    const std::vector<WebGlMesh>&
    webglMeshes() const
    {
        return m_webglMeshes;
    }
    // Force every raw image primitive to carry its pixel payload, bypassing the
    // cross-render dedup cache. The dedup assumes a client that kept the pixels of
    // an earlier frame, which only holds for the streaming animation path. A
    // stateless display-list fetch (figure.getDisplayList) may come from a fresh
    // page, so it must ship the pixels every time.
    void
    setAlwaysIncludeRawImagePayload(bool enabled)
    {
        m_alwaysIncludeRawImagePayload = enabled;
    }
    uint64_t
    displaySignature(bool includeImagePayload = true) const;
    uint64_t
    layoutSignature() const;
    void
    currentWebGlTransform(WebGlMesh& mesh) const;
    const double*
    bounds() const
    {
        return m_bounds;
    }
    // True once a 3D surface has been drawn (the figure is rotatable). The surface
    // is already baked into primitives(), so no raw 3D geometry leaves the engine.
    bool
    hasScene3d() const
    {
        return m_has3d;
    }
    double
    canvasWidth() const
    {
        return m_width;
    }
    double
    canvasHeight() const
    {
        return m_height;
    }

    std::wstring
    getRenderName() override
    {
        return L"Web";
    }
    bool
    getGraphicsSmoothing() override
    {
        return m_smoothing;
    }
    void
    setGraphicsSmoothing(bool on) override
    {
        m_smoothing = on;
    }
    void
    debug() override
    {
    }
    double
    width() override
    {
        return m_width;
    }
    double
    height() override
    {
        return m_height;
    }
    void clear(std::vector<double>) override;
    bool
    pushRegion(double x, double y, double width, double height) override;
    void
    popRegion() override;

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
    getModelviewMatrix(std::vector<double>& model) override;

    void color(std::vector<double>) override;
    void
    setLineStyle(const std::wstring& style) override;
    void
    lineWidth(double n) override;
    double
    pointsToPixels(double points) override;

    void
    quad(double x1, double y1, double z1, double x2, double y2, double z2, double x3, double y3,
        double z3, double x4, double y4, double z4) override;
    void
    quadline(double x1, double y1, double z1, double x2, double y2, double z2, double x3, double y3,
        double z3, double x4, double y4, double z4) override;
    void
    tri(double x1, double y1, double z1, double x2, double y2, double z2, double x3, double y3,
        double z3) override;
    bool
    polygonFill(const std::vector<std::vector<double>>& points,
        const std::vector<double>& faceColor) override;
    void
    triLine(double x1, double y1, double z1, double x2, double y2, double z2, double x3, double y3,
        double z3) override;
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
    void
    setupDirectDraw() override;
    void
    releaseDirectDraw() override;
    void
    beginSurfaceGroup() override;
    void
    endSurfaceGroup() override;
    void
    putText(double x, double y, std::wstring txt, std::vector<double> color, AlignmentFlag xflag,
        AlignmentFlag yflag, const FontSpec& fnt, double rotation) override;
    void
    measureText(std::wstring txt, const FontSpec& fnt, AlignmentFlag xflag, AlignmentFlag yflag,
        int& width, int& height, int& xoffset, int& yoffset) override;
    void
    depth(bool) override
    {
    }
    void
    rect(double x1, double y1, double x2, double y2) override;
    void
    rectFill(double x1, double y1, double x2, double y2) override;
    void
    circle(double x1, double y1, double radius) override;
    void
    circleFill(double x1, double y1, double radius) override;
    bool
    pointMarkers(const std::vector<double>& xs, const std::vector<double>& ys,
        const std::vector<double>& markerSizes, const std::vector<double>& rgbaColors) override;
    bool
    markerBatch(int shape, const std::vector<double>& xs, const std::vector<double>& ys,
        const std::vector<double>& markerSizes, const std::vector<double>& faceRgba,
        const std::vector<double>& edgeRgba, double edgeWidth,
        const std::vector<double>& depths = { }) override;
    bool
    rectBatch(const std::vector<double>& corners, const std::vector<double>& faceColor,
        const std::vector<double>& edgeColor, double edgeWidth) override;
    void
    drawPieSlice(double cx, double cy, double outerR, double innerR, double startAngle,
        double spanAngle, const std::vector<double>& faceColor,
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
    quadStrips(const std::vector<std::vector<coloredPoint>>& faces, bool flatfaces,
        const std::vector<std::vector<coloredPoint>>& edges, bool flatedges, meshStyle meshstyle,
        double lineWidth, const std::wstring& lineStyle,
        enum LightingMode::LightingMode faceLighting, enum LightingMode::LightingMode edgeLighting,
        uint64_t geometryToken = 0) override;
    void
    drawPatch(const FaceList& faces, const PatchRenderOptions& options) override;
    void
    setLightingScene(const RenderLightingScene& scene) override;
    void
    setLightingMaterial(const RenderLightingMaterial& material) override;
    void
    setDataClipBox(const RenderClipBox& box) override;
    void
    clearDataClipBox() override;
    void
    setClipBox(double x0, double y0, double width, double height) override;
    void
    clearClipBox() override;
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

private:
    double model[16];
    double proj[16];
    int viewp[4];
    double save_model[16];
    double save_proj[16];
    int save_viewp[4];
    double m_x1, m_y1, m_width, m_height;
    // A region paints into the display list as if it were the whole surface;
    // popRegion then shifts everything emitted since the mark onto the
    // sub-rectangle. Innermost region last.
    struct SavedRegion
    {
        size_t primitiveMark, meshMark;
        double x1, y1, width, height;
        double offsetX, offsetTop, offsetBottom;
    };
    std::vector<SavedRegion> m_regions;
    // Surface texture bound by setSurfaceTexture, sampled per fragment by the
    // z-buffered surface raster while FaceColor is 'texturemap'. Pixels are copied
    // (RGBA8, row-major) so the view's storage need not outlive the draw.
    std::vector<unsigned char> m_surfaceTextureRgba;
    int m_surfaceTextureW = 0;
    int m_surfaceTextureH = 0;
    bool m_surfaceTextureActive = false;
    // Modelview matrices saved by pushModelMatrix for the hgtransform stack;
    // popModelMatrix restores the top.
    std::vector<std::array<double, 16>> m_modelStack;
    // Data-space clipping is suspended while a transform is active: the clip cuboid
    // is in untransformed data coordinates, so testing the raw coordinates of a
    // transformed child against it would clip in the wrong frame. Each push saves
    // the enabled flag and clears it; each pop restores it.
    std::vector<char> m_dataClipEnabledStack;
    // Depth of screen-space clip rectangles pushed by setClipBox (balanced by
    // clearClipBox), so an unmatched clearClipBox never emits a stray restore.
    int m_clipBoxDepth = 0;
    bool m_smoothing = true;
    std::string m_color = "rgba(0,0,0,1)";
    std::wstring m_lineStyle = L"-";
    double m_lineWidth = 1.0;
    // Qt's dash patterns for the current style, in pixels; empty when solid.
    std::vector<double>
    dashArray() const;
    // "none" means the line must not be drawn at all.
    bool
    lineHidden() const
    {
        return m_lineStyle == L"none";
    }
    std::vector<WebDrawPrimitive> m_prims;
    std::vector<WebUIControl> m_controls;
    std::vector<WebGlMesh> m_webglMeshes;
    // 3D primitives are buffered with their mean depth and flushed far-to-near, as
    // RenderQt's BSP collection does: the 2D canvas has no depth buffer.
    std::vector<WebDrawPrimitive> m_pending3d;
    // Direct draw paints straight to screen pixels in call order; depth buffering
    // is suspended between setupDirectDraw() and releaseDirectDraw().
    bool m_inDirect = false;
    // Nesting depth of begin/endSurfaceGroup. Edge strokes emitted inside a group
    // are tagged solidEdge so flush3d sorts them by true depth: the camera-ward bias
    // that keeps a surface's coplanar mesh line on top of its fill would let a far
    // bar's edge show through a nearer opaque face (histogram2).
    int m_surfaceGroupDepth = 0;
    // Axes data-limit clip cuboid (mapped coordinates); applied to 3D lines and
    // polygons so plotted geometry does not spill outside the axes box.
    RenderClipBox m_dataClip;
    bool m_has3d = false;
    bool m_softwareFallbackEnabled = true;
    bool m_encodeImagePngPayloads = true;
    bool m_alwaysIncludeRawImagePayload = false;
    double m_bounds[6] = { -1, 1, -1, 1, -1, 1 };
    // Lighting state captured from the axes traversal (GOAxis::drawChildren); used
    // to bake lit vertex colors in the software raster and forwarded to the WebGL
    // meshes.
    RenderLightingScene m_lightingScene;
    RenderLightingMaterial m_lightingMaterial;
    // Figure-resolution nearest-depth buffer (clip-space z, nearer = smaller)
    // written by opaque z-buffered surfaces. The 2D canvas has no depth buffer, so
    // markers painted later test their centres against it and are dropped when a
    // surface hides them. Empty until a surface fills it; sized m_depthW x m_depthH.
    std::vector<double> m_sceneDepth;
    int m_depthW = 0;
    int m_depthH = 0;
    void
    writeSceneDepth(int px, int py, double z);
    bool
    markerOccluded(double px, double py, double z) const override;
    // Translucent patch faces and edges waiting for the end of the 3D scene (the
    // outermost endSurfaceGroup), when the opaque scene depth is complete.
    std::vector<WebDeferredTranslucentPolygon> m_deferredTranslucentPolygons;
    std::vector<WebDeferredTranslucentEdge> m_deferredTranslucentEdges;
    void
    flushDeferredTranslucent();
    bool
    tryPushScenePatch(const FaceList& faces, const PatchRenderOptions& options, bool webglFallback);
    bool
    pushSceneDepthImage(const std::vector<unsigned char>& rgba, const std::vector<double>& zbuffer,
        int width, int height, int oversample, bool writeDepth, bool webglFallback);

    void
    mapXY(double x, double y, double z, double& px, double& py);
    double
    mapXYZ(double x, double y, double z, double& px, double& py);
    std::string
    colorString(const std::vector<double>& col) const;
    void
    pushImage(const RGBAImageView& pic, double left, double top, double right, double bottom);
    bool
    tryPushZBufferedSurface(const std::vector<std::vector<coloredPoint>>& faces, bool flatLayout,
        bool flatShade, const std::vector<std::vector<coloredPoint>>& edges, bool drawEdges,
        meshStyle meshstyle, double lineWidth, bool webglFallback, int oversample,
        bool transparent = false);
    bool
    tryPushZBufferedPatch(
        const FaceList& faces, const PatchRenderOptions& options, bool webglFallback);
    bool
    tryPushWebGlPatch(const FaceList& faces, const PatchRenderOptions& options);
    bool
    tryPushWebGlSurface(const std::vector<std::vector<coloredPoint>>& faces, bool flatfaces,
        const std::vector<std::vector<coloredPoint>>& edges, meshStyle meshstyle, double lineWidth,
        const std::wstring& lineStyle, uint64_t geometryToken);
};
//=============================================================================
// Qt-free software rasterizer for a web display list (the primitives RenderWeb
// records). Text is drawn with stb_truetype, embedded rasters are decoded with
// stb_image, 2x supersampled for anti-aliasing. rgbOut is row-major RGB8
// (width*height*3). Used by getframe and the web PNG export in WEB_GUI mode.
NLSRENDERWEB_IMPEXP bool
rasterizeWebPrimitives(const std::vector<WebDrawPrimitive>& primitives, int width, int height,
    std::vector<unsigned char>& rgbOut);
NLSRENDERWEB_IMPEXP bool
rasterizeWebPrimitivesAndControls(const std::vector<WebDrawPrimitive>& primitives,
    const std::vector<WebUIControl>& controls, int width, int height,
    std::vector<unsigned char>& rgbOut);
//=============================================================================
// Same, then encoded as PNG (stb_image_write) and base64'd for a data URI.
NLSRENDERWEB_IMPEXP bool
rasterizeWebPrimitivesToPngBase64(
    const std::vector<WebDrawPrimitive>& primitives, int width, int height, std::string& base64Out);
NLSRENDERWEB_IMPEXP bool
rasterizeWebPrimitivesAndControlsToPngBase64(const std::vector<WebDrawPrimitive>& primitives,
    const std::vector<WebUIControl>& controls, int width, int height, std::string& base64Out);
//=============================================================================
// A display list recorded at the figure's logical size, lifted to a larger
// surface (export DPI, device pixel ratio) by one factor, so the layout is
// never computed on the enlarged canvas.
NLSRENDERWEB_IMPEXP std::vector<WebDrawPrimitive>
scalePrimitivesToSurface(const std::vector<WebDrawPrimitive>& primitives, double scale);
NLSRENDERWEB_IMPEXP std::vector<WebUIControl>
scaleControlsToSurface(const std::vector<WebUIControl>& controls, double scale);
//=============================================================================
// Qt-free vector exports from a web display list. Raster file exports consume
// rasterizeWebPrimitives through the graphics_io codec registry.
// width/height are the internal (export DPI) resolution used for the viewBox and
// all primitive coordinates; displayScale multiplies them into the logical size
// the SVG declares, so a viewer paints the vector content at its intended
// on-screen size while keeping the higher-resolution detail. Deriving both
// declared dimensions from the same factor keeps the declared aspect ratio equal
// to the viewBox. The default 1.0 declares the internal resolution as-is.
NLSRENDERWEB_IMPEXP bool
webPrimitivesToSvgFile(const std::vector<WebDrawPrimitive>& primitives, int width, int height,
    const std::wstring& filename, double displayScale = 1.0);
NLSRENDERWEB_IMPEXP bool
webPrimitivesToPdfFile(const std::vector<WebDrawPrimitive>& primitives, int width, int height,
    const std::wstring& filename);
//=============================================================================
} // namespace Nelson
//=============================================================================
