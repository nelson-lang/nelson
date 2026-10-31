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
#include <cstdint>
#include <vector>
#include <string>
#include "ColoredPoint.hpp"
#include "RGBAColorData.hpp"
#include "RenderClipping.hpp"
//=============================================================================
// Fill a column-major perspective (frustum) projection matrix with the
// proj[col * 4 + row] layout of the backends' orthographic project(). The
// perspective term sits in proj[11] = -1 and proj[15] = 0, which is what the
// `wclip = proj[3] x + proj[7] y + proj[11] z + proj[15]` divide in each
// toPixels() expects.
inline void
buildFrustumProjectionMatrix(
    double* proj, double left, double right, double bottom, double top, double znear, double zfar)
{
    for (int i = 0; i < 16; ++i) {
        proj[i] = 0.0;
    }
    proj[0] = 2.0 * znear / (right - left);
    proj[5] = 2.0 * znear / (top - bottom);
    proj[8] = (right + left) / (right - left);
    proj[9] = (top + bottom) / (top - bottom);
    proj[10] = -(zfar + znear) / (zfar - znear);
    proj[11] = -1.0;
    proj[14] = -2.0 * zfar * znear / (zfar - znear);
    proj[15] = 0.0;
}
//=============================================================================
struct FontSpec
{
    // Weights use the CSS / OpenType usWeightClass scale (100..900), the same as
    // QFont::Weight in Qt 6 (Light 300, Normal 400, DemiBold 600, Bold 700).
    enum Weight
    {
        WeightLight = 300,
        WeightNormal = 400,
        WeightDemiBold = 600,
        WeightBold = 700,
    };

    std::wstring family;
    double pointSizeF = 10.0; // points (1/72 in); used when pixelSize <= 0
    double pixelSize = 0.0; // device pixels; > 0 wins over pointSizeF
    bool bold = false; // kept in sync with weight (>= WeightDemiBold)
    bool italic = false;
    bool underline = false;
    bool antialias = true;
    int weight = WeightNormal;
    double stretch = 1.0; // horizontal stretch factor (1.0 = none)

    void
    setWeight(int cssWeight)
    {
        weight = cssWeight;
        bold = cssWeight >= WeightDemiBold;
    }
};
//=============================================================================
// Effective pixel size of a FontSpec (96 dpi convention).
inline double
fontSpecPixelSize(const FontSpec& font)
{
    if (font.pixelSize > 0) {
        return font.pixelSize;
    }
    if (font.pointSizeF > 0) {
        return font.pointSizeF * (96.0 / 72.0);
    }
    return 12.0;
}
//=============================================================================
// Qt-free view over 8-bit RGBA pixels (row-major). The pixel storage must
// outlive the draw call; backends copy what they keep.
struct RGBAImageView
{
    int width = 0;
    int height = 0;
    int bytesPerLine = 0; // stride in bytes; 0 means width * 4
    const unsigned char* pixels = nullptr;
    uint64_t resourceKey = 0;
    uint64_t contentRevision = 0;
    bool opaque = false;
};
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
//=============================================================================
} // namespace ColorMode
//=============================================================================
namespace AlphaMode {
enum AlphaMode
{
    Scalar,
    Flat,
    Interp
};
//=============================================================================
} // namespace AlphaMode
//=============================================================================
namespace EraseMode {
enum EraseMode
{
    Normal,
    None,
    Xor,
    Background
};
//=============================================================================
} // namespace EraseMode
//=============================================================================
namespace LightingMode {
enum LightingMode
{
    None,
    Flat,
    Gouraud,
    Phong
};
//=============================================================================
} // namespace LightingMode
//=============================================================================
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
    point(double X, double Y, double Z) : x(X), y(Y), z(Z) { };
};
//=============================================================================
class Face
{
public:
    std::vector<point> vertices;
    std::vector<point> normals;
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
class PatchRenderOptions
{
public:
    double lineWidth { 0.5 };
    std::wstring lineStyle { L"-" };
    std::wstring lineJoin { L"round" };
    bool uniqueEdges { false };
    uint64_t geometryToken { 0 };
    enum LightingMode::LightingMode faceLighting { LightingMode::None };
    enum LightingMode::LightingMode edgeLighting { LightingMode::None };
};
//=============================================================================
class RenderLight
{
public:
    std::vector<double> color { 1, 1, 1 };
    std::vector<double> position { 1, 0, 1 };
    bool isLocal = false;
};
//=============================================================================
class RenderLightingScene
{
public:
    bool enabled = false;
    std::vector<RenderLight> lights;
    std::vector<double> ambientLightColor { 1, 1, 1 };
};
//=============================================================================
class RenderLightingMaterial
{
public:
    double ambientStrength = 0.3;
    double diffuseStrength = 0.6;
    double specularStrength = 0.9;
    double specularExponent = 10;
    double specularColorReflectance = 1;
};
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
    clearDepthBuffer()
    {
    }
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
    // Perspective (frustum) projection. left/right/bottom/top are the frustum
    // extents on the near plane, znear/zfar are positive distances along the view
    // direction. Backends without perspective inherit the orthographic fallback.
    virtual void
    projectPerspective(
        double left, double right, double bottom, double top, double znear, double zfar)
    {
        project(left, right, bottom, top, znear, zfar);
    }
    //=============================================================================
    virtual void
    viewport(double x0, double y0, double width, double height)
        = 0;

    // Restrict the renderer to a sub-rectangle of its surface, in pixels of the
    // current frame with a bottom-left origin. width()/height() then report the
    // sub-rectangle and everything drawn is placed and clipped inside it. uipanel
    // and uitab use it for their children. Returns false when the backend cannot
    // do it.
    virtual bool
    pushRegion(double x, double y, double width, double height)
    {
        return false;
    }

    virtual void
    popRegion()
    {
    }
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
    virtual bool
    polygonFill(
        const std::vector<std::vector<double>>& points, const std::vector<double>& faceColor)
    {
        return false;
    }
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
        t.reserve(3);
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
    virtual double
    pointsToPixels(double points)
        = 0;
    //=============================================================================
    virtual void
    line(double x1, double y1, double z1, double x2, double y2, double z2)
        = 0;
    virtual void
    line(double x1, double y1, double x2, double y2)
        = 0;
    virtual void
    lineSegments3D(const std::vector<double>& xyz)
    {
        for (size_t k = 0; k + 5 < xyz.size(); k += 6) {
            line(xyz[k], xyz[k + 1], xyz[k + 2], xyz[k + 3], xyz[k + 4], xyz[k + 5]);
        }
    }
    virtual void
    lineSegments2D(const std::vector<double>& xy)
    {
        for (size_t k = 0; k + 3 < xy.size(); k += 4) {
            line(xy[k], xy[k + 1], xy[k + 2], xy[k + 3]);
        }
    }
    //=============================================================================
    virtual void
    lineSeries(std::vector<double> xs, std::vector<double> ys, std::vector<double> zs)
        = 0;
    virtual bool
    pointMarkers(const std::vector<double>& xs, const std::vector<double>& ys,
        const std::vector<double>& markerSizes, const std::vector<double>& rgbaColors)
    {
        return false;
    }
    virtual void
    setupDirectDraw()
        = 0;
    virtual void
    setClipBox(double, double, double, double)
    {
    }
    virtual void
    clearClipBox()
    {
    }
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
        AlignmentFlag yflag, const FontSpec& fnt, double rotation)
        = 0;
    //=============================================================================
    virtual void
    measureText(std::wstring txt, const FontSpec& fnt, AlignmentFlag xflag, AlignmentFlag yflag,
        int& width, int& height, int& xoffset, int& yoffset)
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
    drawPieSlice(double centerX, double centerY, double outerRadius, double innerRadius,
        double startAngle, double spanAngle, const std::vector<double>& faceColor,
        const std::vector<double>& edgeColor, double lineWidth)
        = 0;
    //=============================================================================
    virtual void
    drawImage(int x1, int y1, const RGBAImageView& pic)
        = 0;
    //=============================================================================
    virtual void
    drawImage(double x1, double y1, double x2, double y2, const RGBAImageView& pic)
        = 0;
    //=============================================================================
    virtual void
    drawImage(const std::vector<double>& xp, const std::vector<double>& yp,
        const std::vector<double>& xLim, const std::vector<double>& yLim, bool xFlip, bool yFlip,
        const RGBAImageView& pic)
        = 0;
    //=============================================================================
    virtual void
    setLightingScene(const RenderLightingScene&) { };
    //=============================================================================
    virtual void
    setLightingMaterial(const RenderLightingMaterial&) { };
    //=============================================================================
    virtual void
    beginSurfaceGroup() { };
    //=============================================================================
    virtual void
    endSurfaceGroup() { };
    //=============================================================================
    virtual void
    quadStrips(const std::vector<std::vector<coloredPoint>>& faces, bool flatfaces,
        const std::vector<std::vector<coloredPoint>>& edges, bool flatedges, meshStyle meshstyle,
        double lineWidth, const std::wstring& lineStyle,
        enum LightingMode::LightingMode faceLighting = LightingMode::None,
        enum LightingMode::LightingMode edgeLighting = LightingMode::None,
        uint64_t geometryToken = 0)
        = 0;
    //=============================================================================
    virtual void
    drawPatch(const FaceList& faces, const PatchRenderOptions& options)
        = 0;
    //=============================================================================
    // Data-space clip cuboid, in the axes "mapped" coordinate system (see
    // RenderClipping.hpp). An axes with Clipping == 'on' sets it around the
    // painting of its children. Backends that do not support it ignore the hooks.
    // Declared last to keep the vtable slots of existing methods unchanged (ABI
    // stability for modules that are not recompiled).
    virtual void
    setDataClipBox(const RenderClipBox&)
    {
    }
    virtual void
    clearDataClipBox()
    {
    }
    // Batch of markers of one shape drawn in a single primitive. xs/ys are pre-map
    // pixel centres, markerSizes the per-point size (as passed to DrawSymbol).
    // faceRgba and edgeRgba hold 4 values per point, or are empty. shape is a
    // SymbolType. depths holds the per-point clip-space z (as returned by the
    // 6-argument toPixels, NDC in [-1, 1], nearer = smaller), or is empty for a 2D
    // plot. A backend uses it to hide markers behind an opaque surface. A backend
    // that cannot batch returns false and the caller falls back to DrawSymbol.
    // Kept last in the class to preserve vtable slots.
    virtual bool
    markerBatch(int shape, const std::vector<double>& xs, const std::vector<double>& ys,
        const std::vector<double>& markerSizes, const std::vector<double>& faceRgba,
        const std::vector<double>& edgeRgba, double edgeWidth,
        const std::vector<double>& depths = {})
    {
        return false;
    }
    // Batch of axis-aligned rectangles sharing one fill and one outline colour
    // (bar and histogram bars) drawn in a single primitive. corners holds two
    // opposite data-space corners per rect (x1,y1,x2,y2). faceColor and edgeColor
    // are single colours (first component -1 or size < 3 means 'none'). A backend
    // that cannot batch returns false and the caller draws bar by bar. Kept last to
    // preserve vtable slots.
    virtual bool
    rectBatch(const std::vector<double>& corners, const std::vector<double>& faceColor,
        const std::vector<double>& edgeColor, double edgeWidth)
    {
        return false;
    }
    // Widen the pixel clip from the axes rectangle (set by the preceding
    // viewport()) to the whole frame, without changing the NDC viewport mapping.
    // Geometry overflowing the axes box then spills into the figure. An axes with
    // Clipping == 'off' calls this right after viewport(). Default is a no-op.
    // Kept last to preserve vtable slots.
    virtual void
    clipToEntireFrame()
    {
    }
    //=============================================================================
    // Project a data-space point to pixels like the (x,y,z,a,b,clipped) overload,
    // and also return its clip-space depth c (NDC z in [-1, 1], nearer = smaller).
    // Callers use the depth to test screen-space markers against surfaces. Default
    // forwards to the 2D overload with c = 0; 3D backends override it. Kept last to
    // preserve vtable slots.
    virtual void
    toPixels(double x, double y, double z, double& a, double& b, double& c, bool& clipped)
    {
        c = 0.0;
        toPixels(x, y, z, a, b, clipped);
    }
    //=============================================================================
    // Post-multiply the current modelview by a 4x4 homogeneous matrix (16 doubles,
    // column-major) and save the previous modelview on an internal stack. A
    // transform group (hgtransform) pushes its Matrix around the painting of its
    // children, so a child point p maps as model_axes * M * p. popModelMatrix
    // restores the saved modelview. Default is a no-op for backends without a
    // modelview. Kept last to preserve vtable slots.
    virtual void
    pushModelMatrix(const double* /*matrix16ColumnMajor*/)
    {
    }
    virtual void
    popModelMatrix()
    {
    }
    //=============================================================================
    // Bind an RGBA texture that the next quadStrips() call samples per fragment at
    // each vertex's (u, v), modulated by the vertex colour so baked lighting still
    // applies. Used by a surface with FaceColor 'texturemap'. The pixel storage must
    // outlive the draw; a backend that keeps it copies it. clearSurfaceTexture()
    // unbinds it. Default is a no-op: the vertices' own (white) colour is rendered.
    // Kept last to preserve vtable slots.
    virtual void
    setSurfaceTexture(const RGBAImageView& /*texture*/)
    {
    }
    virtual void
    clearSurfaceTexture()
    {
    }
    // Whether this backend samples a bound surface texture per fragment. When it
    // does, a 'texturemap' surface feeds it white, UV-tagged vertices and the
    // texture. Otherwise the caller bakes a grid-sampled approximation of the image
    // into the vertex colours. Kept last to preserve vtable slots.
    virtual bool
    supportsSurfaceTexture()
    {
        return false;
    }
    // Whether an opaque surface or patch face already drawn in this axes hides the
    // marker whose centre is the direct-draw pixel (px, py) at clip-space depth z (the
    // value the 7-argument toPixels returns). The per-marker DrawSymbol fallback asks
    // this when markerBatch declined, so a backend without a hardware depth buffer can
    // still drop the markers behind a surface. Default: nothing occludes. Kept last to
    // preserve vtable slots.
    virtual bool
    markerOccluded(double /*px*/, double /*py*/, double /*z*/) const
    {
        return false;
    }
    //=============================================================================
};
//=============================================================================
