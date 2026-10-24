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
#if defined(__APPLE__) && !defined(GL_SILENCE_DEPRECATION)
#define GL_SILENCE_DEPRECATION
#endif
#if defined(_WIN32) && !defined(NOMINMAX)
#define NOMINMAX
#endif
#include <cmath>
#include <cstring>
#include <QtGui/QGuiApplication>
#include <QtGui/QScreen>
#include <algorithm>
#include <array>
#include <functional>
#if defined(_WIN32)
#include <windows.h>
#ifdef min
#undef min
#endif
#ifdef max
#undef max
#endif
#ifdef near
#undef near
#endif
#ifdef far
#undef far
#endif
#endif
#if defined(__APPLE__)
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif
#include "RenderOpenGL.hpp"
#include "RenderOpenGLSurfaceArrays.hpp"
#include "GOPropertyValues.hpp"
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
double
RenderOpenGL::deviceScale() const
{
    // QOpenGLWidget grabs its frame buffer at device resolution (logical size *
    // devicePixelRatio) while qtScreenPointsToPixels yields logical pixels. Scale by
    // the ratio so markers and line widths keep their physical size on HiDPI.
    double ratio = widget ? static_cast<double>(widget->devicePixelRatioF()) : 1.0;
    return ratio > 0.0 ? ratio : 1.0;
}
//=============================================================================
static void
combineRenderCacheHash(uint64_t& seed, uint64_t value)
{
    seed ^= value + 0x9e3779b97f4a7c15ULL + (seed << 6) + (seed >> 2);
}
//=============================================================================
static void
combineRenderCacheHashDouble(uint64_t& seed, double value)
{
    uint64_t bits = 0;
    static_assert(sizeof(bits) == sizeof(value), "double must be 64-bit");
    std::memcpy(&bits, &value, sizeof(bits));
    combineRenderCacheHash(seed, bits);
}
//=============================================================================
static uint64_t
surfaceDisplayListCacheKey(uint64_t geometryToken, bool flatfaces, bool flatedges,
    meshStyle meshstyle, double lineWidthValue, const std::wstring& lineStyle,
    enum LightingMode::LightingMode faceLighting, enum LightingMode::LightingMode edgeLighting)
{
    uint64_t seed = geometryToken;
    combineRenderCacheHash(seed, flatfaces ? 1 : 0);
    combineRenderCacheHash(seed, flatedges ? 1 : 0);
    combineRenderCacheHash(seed, static_cast<uint64_t>(meshstyle));
    combineRenderCacheHash(seed, static_cast<uint64_t>(std::llround(lineWidthValue * 1000.0)));
    combineRenderCacheHash(seed, static_cast<uint64_t>(std::hash<std::wstring> {}(lineStyle)));
    combineRenderCacheHash(seed, static_cast<uint64_t>(faceLighting));
    combineRenderCacheHash(seed, static_cast<uint64_t>(edgeLighting));
    return seed;
}
//=============================================================================
// Lighting commands are recorded in the display list, so the light set and
// material contribute to its cache key. The model matrix is included because
// local light positions are transformed when the list is replayed.
static void
combineLightingStateCacheHash(uint64_t& seed, const RenderLightingScene& scene,
    const RenderLightingMaterial& material, const double* model)
{
    combineRenderCacheHash(seed, scene.enabled ? 1 : 0);
    for (double component : scene.ambientLightColor) {
        combineRenderCacheHashDouble(seed, component);
    }
    combineRenderCacheHash(seed, static_cast<uint64_t>(scene.lights.size()));
    for (const RenderLight& light : scene.lights) {
        combineRenderCacheHash(seed, light.isLocal ? 1 : 0);
        for (double component : light.position) {
            combineRenderCacheHashDouble(seed, component);
        }
        for (double component : light.color) {
            combineRenderCacheHashDouble(seed, component);
        }
    }
    combineRenderCacheHashDouble(seed, material.ambientStrength);
    combineRenderCacheHashDouble(seed, material.diffuseStrength);
    combineRenderCacheHashDouble(seed, material.specularStrength);
    combineRenderCacheHashDouble(seed, material.specularExponent);
    combineRenderCacheHashDouble(seed, material.specularColorReflectance);
    for (int i = 0; i < 16; ++i) {
        combineRenderCacheHashDouble(seed, model[i]);
    }
}
//=============================================================================
void
RenderOpenGL::resetState(
    double x1, double y1, double width, double height, const std::wstring& name)
{
    m_x1 = x1;
    m_y1 = y1;
    m_width = width;
    m_height = height;
    renderName = name;
    for (int i = 0; i < 16; i++) {
        model[i] = 0;
        proj[i] = 0;
        save_model[i] = 0;
        save_proj[i] = 0;
    }
    model[0] = model[5] = model[10] = model[15] = 1;
    proj[0] = proj[5] = proj[10] = proj[15] = 1;
    viewp[0] = 0;
    viewp[1] = 0;
    viewp[2] = static_cast<int>(width);
    viewp[3] = static_cast<int>(height);
    if (widget) {
        glViewport(viewp[0], viewp[1], viewp[2], viewp[3]);
        glDisable(GL_SCISSOR_TEST);
        glDisable(GL_TEXTURE_2D);
        glDisable(GL_CULL_FACE);
#if defined(GL_COLOR_LOGIC_OP)
        glDisable(GL_COLOR_LOGIC_OP);
#endif
        glDepthMask(GL_TRUE);
        glDepthFunc(GL_LEQUAL);
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    }
}
//=============================================================================
RenderOpenGL::RenderOpenGL(QOpenGLWidget* glWidget, double x1, double y1, double width,
    double height, const std::wstring& _renderName)
{
    widget = glWidget;
    resetState(x1, y1, width, height, _renderName);
}
//=============================================================================
RenderOpenGL::~RenderOpenGL()
{
    clearDisplayListCaches();
    clearTextTextureCache();
    clearImageTextureCache();
}
//=============================================================================
void
RenderOpenGL::resetSurface(
    double x1, double y1, double width, double height, const std::wstring& name)
{
    resetState(x1, y1, width, height, name);
}
//=============================================================================
void
RenderOpenGL::clearTextTextureCache()
{
    for (const auto& item : textTextureCache) {
        GLuint texture = static_cast<GLuint>(item.second.texture);
        if (texture != 0 && glIsTexture(texture)) {
            glDeleteTextures(1, &texture);
        }
    }
    textTextureCache.clear();
}
//=============================================================================
void
RenderOpenGL::clearDisplayListCaches()
{
    for (const auto& item : surfaceDisplayListCache) {
        GLuint list = static_cast<GLuint>(item.second);
        if (list != 0 && glIsList(list)) {
            glDeleteLists(list, 1);
        }
    }
    surfaceDisplayListCache.clear();
    for (const auto& item : patchDisplayListCache) {
        GLuint list = static_cast<GLuint>(item.second);
        if (list != 0 && glIsList(list)) {
            glDeleteLists(list, 1);
        }
    }
    patchDisplayListCache.clear();
    surfaceDisplayListCandidates.clear();
    patchDisplayListCandidates.clear();
    patchVertexArrayCache.clear();
    surfaceVertexArrayCache.clear();
}
//=============================================================================
// Direct glColor* calls (per-vertex colors of surfaces/patches, color arrays)
// leave the GL current color out of sync with the tracked currentColor member;
// call this after such draws so cached-color consumers (putText, drawImage)
// restore the color the caller actually set through color().
void
RenderOpenGL::restoreCurrentColor()
{
    glColor4dv(currentColor);
}
//=============================================================================
std::wstring
RenderOpenGL::getRenderName()
{
    return renderName;
}
//=============================================================================
bool
RenderOpenGL::getGraphicsSmoothing()
{
    return graphicsSmoothing;
}
//=============================================================================
void
RenderOpenGL::setGraphicsSmoothing(bool on)
{
    graphicsSmoothing = on;
    if (!widget) {
        return;
    }
    if (on) {
        glEnable(GL_MULTISAMPLE);
        glEnable(GL_LINE_SMOOTH);
        glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
    } else {
        glDisable(GL_LINE_SMOOTH);
        glDisable(GL_MULTISAMPLE);
    }
}
//=============================================================================
void
RenderOpenGL::debug()
{
    debugFlag = !debugFlag;
}
//=============================================================================
double
RenderOpenGL::width()
{
    return m_width;
}
//=============================================================================
double
RenderOpenGL::height()
{
    return m_height;
}
//=============================================================================
void
RenderOpenGL::loadModelMatrix()
{
    if (!widget) {
        return;
    }
    glMatrixMode(GL_MODELVIEW);
    glLoadMatrixd(model);
}
//=============================================================================
void
RenderOpenGL::loadProjectionMatrix()
{
    if (!widget) {
        return;
    }
    glMatrixMode(GL_PROJECTION);
    glLoadMatrixd(proj);
}
//=============================================================================
void
RenderOpenGL::clear(std::vector<double> col)
{
    if (col.size() >= 3) {
        disableLighting();
        glDisable(GL_SCISSOR_TEST);
        glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
        glDepthMask(GL_TRUE);
        glDepthFunc(GL_LEQUAL);
        glEnable(GL_DEPTH_TEST);
        glClearColor(static_cast<GLfloat>(col[0]), static_cast<GLfloat>(col[1]),
            static_cast<GLfloat>(col[2]), 1.0f);
        glClearDepth(1.0);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glDisable(GL_CULL_FACE);
        glDisable(GL_TEXTURE_2D);
    }
}
//=============================================================================
void
RenderOpenGL::clearDepthBuffer()
{
    if (!widget) {
        return;
    }
    glDisable(GL_SCISSOR_TEST);
    glDepthMask(GL_TRUE);
    glClearDepth(1.0);
    glClear(GL_DEPTH_BUFFER_BIT);
}
//=============================================================================
static GLfloat
toGLClamped(double value)
{
    return static_cast<GLfloat>(std::max(0.0, std::min(1.0, value)));
}
//=============================================================================
static void
resetBlendState()
{
#if defined(GL_COLOR_LOGIC_OP)
    glDisable(GL_COLOR_LOGIC_OP);
#endif
#if defined(GL_LOGIC_OP)
    glDisable(GL_LOGIC_OP);
#endif
#if defined(GL_COPY)
    glLogicOp(GL_COPY);
#endif
#if defined(GL_POLYGON_STIPPLE)
    glDisable(GL_POLYGON_STIPPLE);
#endif
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
}
//=============================================================================
static GLfloat
toGLShininess(double value)
{
    return static_cast<GLfloat>(std::max(0.0, std::min(128.0, value)));
}
//=============================================================================
void
RenderOpenGL::disableLighting()
{
    if (!widget) {
        return;
    }
    glDisable(GL_LIGHTING);
    glDisable(GL_COLOR_MATERIAL);
    glDisable(GL_NORMALIZE);
    for (int k = 0; k < 8; ++k) {
        glDisable(static_cast<GLenum>(GL_LIGHT0 + k));
    }
}
//=============================================================================
void
RenderOpenGL::configureLighting(bool enabled)
{
    if (!widget || !enabled || !lightingScene.enabled || lightingScene.lights.empty()) {
        disableLighting();
        return;
    }
    glEnable(GL_LIGHTING);
    glEnable(GL_NORMALIZE);
    glEnable(GL_COLOR_MATERIAL);
    glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
    glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
    glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE);
#if defined(GL_LIGHT_MODEL_COLOR_CONTROL) && defined(GL_SEPARATE_SPECULAR_COLOR)
    glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL, GL_SEPARATE_SPECULAR_COLOR);
#endif

    GLfloat globalAmbient[4]
        = { toGLClamped(lightingMaterial.ambientStrength * lightingScene.ambientLightColor[0]),
              toGLClamped(lightingMaterial.ambientStrength * lightingScene.ambientLightColor[1]),
              toGLClamped(lightingMaterial.ambientStrength * lightingScene.ambientLightColor[2]),
              1.0f };
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, globalAmbient);

    GLfloat materialSpecular[4] = { toGLClamped(lightingMaterial.specularStrength
                                        * lightingMaterial.specularColorReflectance),
        toGLClamped(lightingMaterial.specularStrength * lightingMaterial.specularColorReflectance),
        toGLClamped(lightingMaterial.specularStrength * lightingMaterial.specularColorReflectance),
        1.0f };
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, materialSpecular);
    glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, toGLShininess(lightingMaterial.specularExponent));

    GLfloat zero[4] = { 0.0f, 0.0f, 0.0f, 1.0f };
    for (int k = 0; k < 8; ++k) {
        GLenum lightName = static_cast<GLenum>(GL_LIGHT0 + k);
        if (k >= static_cast<int>(lightingScene.lights.size())) {
            glDisable(lightName);
            continue;
        }
        const RenderLight& light = lightingScene.lights[k];
        GLfloat diffuse[4] = { toGLClamped(lightingMaterial.diffuseStrength * light.color[0]),
            toGLClamped(lightingMaterial.diffuseStrength * light.color[1]),
            toGLClamped(lightingMaterial.diffuseStrength * light.color[2]), 1.0f };
        GLfloat specular[4] = { toGLClamped(light.color[0]), toGLClamped(light.color[1]),
            toGLClamped(light.color[2]), 1.0f };
        GLfloat position[4]
            = { static_cast<GLfloat>(light.position[0]), static_cast<GLfloat>(light.position[1]),
                  static_cast<GLfloat>(light.position[2]), light.isLocal ? 1.0f : 0.0f };

        glEnable(lightName);
        glLightfv(lightName, GL_AMBIENT, zero);
        glLightfv(lightName, GL_DIFFUSE, diffuse);
        glLightfv(lightName, GL_SPECULAR, specular);
        glLightfv(lightName, GL_POSITION, position);
    }
}
//=============================================================================
void
RenderOpenGL::setLightingScene(const RenderLightingScene& scene)
{
    lightingScene = scene;
}
//=============================================================================
void
RenderOpenGL::setLightingMaterial(const RenderLightingMaterial& material)
{
    lightingMaterial = material;
}
//=============================================================================
void
RenderOpenGL::color(std::vector<double> col)
{
    if (col.size() >= 4) {
        currentColor[0] = col[0];
        currentColor[1] = col[1];
        currentColor[2] = col[2];
        currentColor[3] = col[3];
        resetBlendState();
        glColor4dv(currentColor);
    } else if (col.size() >= 3) {
        currentColor[0] = col[0];
        currentColor[1] = col[1];
        currentColor[2] = col[2];
        currentColor[3] = 1;
#if defined(GL_POLYGON_STIPPLE)
        glDisable(GL_POLYGON_STIPPLE);
#endif
        glColor4dv(currentColor);
    }
}
//=============================================================================
void
RenderOpenGL::setLineStyleGL(const std::wstring& style)
{
    if (style == L"-") {
        glDisable(GL_LINE_STIPPLE);
    } else if (style == L"--") {
        glEnable(GL_LINE_STIPPLE);
        glLineStipple(1, 0x00FF);
    } else if (style == L":") {
        glEnable(GL_LINE_STIPPLE);
        glLineStipple(1, 0xDDDD);
    } else if (style == L"-.") {
        glEnable(GL_LINE_STIPPLE);
        glLineStipple(1, 0xF0D0);
    } else if (style == GO_PROPERTY_VALUE_NONE_STR) {
        glEnable(GL_LINE_STIPPLE);
        glLineStipple(1, 0x0000);
    }
}
//=============================================================================
void
RenderOpenGL::setLineStyle(const std::wstring& style)
{
    setLineStyleGL(style);
}
//=============================================================================
void
RenderOpenGL::lineWidth(double n)
{
    glLineWidth(static_cast<GLfloat>(std::max(1.0, pointsToPixels(n))));
}
//=============================================================================
static void
drawVertexArray(GLenum primitive, const std::vector<GLdouble>& vertices)
{
    if (vertices.empty()) {
        return;
    }
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(3, GL_DOUBLE, 0, vertices.data());
    glDrawArrays(primitive, 0, static_cast<GLsizei>(vertices.size() / 3));
    glDisableClientState(GL_VERTEX_ARRAY);
}
//=============================================================================
static void
drawVertexArrayRaw(GLenum primitive, const GLdouble* vertices, GLsizei vertexCount)
{
    if (vertexCount <= 0) {
        return;
    }
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(3, GL_DOUBLE, 0, vertices);
    glDrawArrays(primitive, 0, vertexCount);
    glDisableClientState(GL_VERTEX_ARRAY);
}
//=============================================================================
static void
drawVertexArray2D(GLenum primitive, const std::vector<double>& vertices)
{
    if (vertices.empty()) {
        return;
    }
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(2, GL_DOUBLE, 0, vertices.data());
    glDrawArrays(primitive, 0, static_cast<GLsizei>(vertices.size() / 2));
    glDisableClientState(GL_VERTEX_ARRAY);
}
//=============================================================================
static bool
isFiniteLinePoint(const std::vector<double>& xs, const std::vector<double>& ys,
    const std::vector<double>& zs, size_t index)
{
    double z = index < zs.size() ? zs[index] : 0;
    return std::isfinite(xs[index]) && std::isfinite(ys[index]) && std::isfinite(z);
}
//=============================================================================
static void
appendLineVertex(const std::vector<double>& xs, const std::vector<double>& ys,
    const std::vector<double>& zs, size_t index, std::vector<GLdouble>& vertices)
{
    double z = index < zs.size() ? zs[index] : 0;
    vertices.push_back(xs[index]);
    vertices.push_back(ys[index]);
    vertices.push_back(z);
}
//=============================================================================
static bool
drawSeparatedLineSegments(
    const std::vector<double>& xs, const std::vector<double>& ys, const std::vector<double>& zs)
{
    const size_t pointCount = std::min(xs.size(), ys.size());
    std::vector<GLdouble> vertices;
    vertices.reserve(pointCount * 3);
    size_t index = 0;
    while (index < pointCount) {
        while (index < pointCount && !isFiniteLinePoint(xs, ys, zs, index)) {
            index++;
        }
        const size_t start = index;
        while (index < pointCount && isFiniteLinePoint(xs, ys, zs, index)) {
            index++;
        }
        const size_t runLength = index - start;
        if (runLength == 0) {
            continue;
        }
        if (runLength != 2) {
            return false;
        }
        appendLineVertex(xs, ys, zs, start, vertices);
        appendLineVertex(xs, ys, zs, start + 1, vertices);
    }
    if (vertices.empty()) {
        return false;
    }
    drawVertexArray(GL_LINES, vertices);
    return true;
}
//=============================================================================
void
RenderOpenGL::quad(double x1, double y1, double z1, double x2, double y2, double z2, double x3,
    double y3, double z3, double x4, double y4, double z4)
{
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    drawVertexArray(GL_QUADS, { x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4 });
}
//=============================================================================
void
RenderOpenGL::quadline(double x1, double y1, double z1, double x2, double y2, double z2, double x3,
    double y3, double z3, double x4, double y4, double z4)
{
    drawVertexArray(GL_LINE_LOOP, { x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4 });
}
//=============================================================================
void
RenderOpenGL::tri(double x1, double y1, double z1, double x2, double y2, double z2, double x3,
    double y3, double z3)
{
    drawVertexArray(GL_TRIANGLES, { x1, y1, z1, x2, y2, z2, x3, y3, z3 });
}
//=============================================================================
void
RenderOpenGL::triLine(double x1, double y1, double z1, double x2, double y2, double z2, double x3,
    double y3, double z3)
{
    drawVertexArray(GL_LINE_LOOP, { x1, y1, z1, x2, y2, z2, x3, y3, z3 });
}
//=============================================================================
void
RenderOpenGL::line(double x1, double y1, double z1, double x2, double y2, double z2)
{
    const GLdouble vertices[] = { x1, y1, z1, x2, y2, z2 };
    drawVertexArrayRaw(GL_LINES, vertices, 2);
}
//=============================================================================
void
RenderOpenGL::line(double x1, double y1, double x2, double y2)
{
    line(x1, y1, 0, x2, y2, 0);
}
//=============================================================================
void
RenderOpenGL::lineSegments3D(const std::vector<double>& xyz)
{
    drawVertexArray(GL_LINES, xyz);
}
//=============================================================================
void
RenderOpenGL::lineSegments2D(const std::vector<double>& xy)
{
    drawVertexArray2D(GL_LINES, xy);
}
//=============================================================================
void
RenderOpenGL::lineSeries(std::vector<double> xs, std::vector<double> ys, std::vector<double> zs)
{
    const size_t pointCount = std::min(xs.size(), ys.size());
    if (pointCount < 2) {
        return;
    }
    if (drawSeparatedLineSegments(xs, ys, zs)) {
        return;
    }
    size_t index = 0;
    while (index < pointCount) {
        while (index < pointCount && !isFiniteLinePoint(xs, ys, zs, index)) {
            index++;
        }
        std::vector<GLdouble> vertices;
        vertices.reserve((pointCount - index) * 3);
        while (index < pointCount && isFiniteLinePoint(xs, ys, zs, index)) {
            appendLineVertex(xs, ys, zs, index, vertices);
            index++;
        }
        if (vertices.size() >= 6) {
            drawVertexArray(GL_LINE_STRIP, vertices);
        }
    }
}
//=============================================================================
struct PointMarkerBucket
{
    std::vector<GLfloat> vertices;
    std::vector<GLfloat> colors;
};
//=============================================================================
static constexpr int MAX_POINT_MARKER_PIXEL_SIZE = 128;
//=============================================================================
static int
pointMarkerPixelSize(double markerSize, double deviceScale)
{
    // The point marker '.' renders at a third of the nominal MarkerSize.
    double pixelSize = qtScreenPointsToPixels(markerSize) * deviceScale / 3.0;
    pixelSize
        = std::max(1.0, std::min(static_cast<double>(MAX_POINT_MARKER_PIXEL_SIZE), pixelSize));
    return static_cast<int>(std::round(pixelSize));
}
//=============================================================================
double
RenderOpenGL::pointsToPixels(double points)
{
    return qtScreenPointsToPixels(points) * deviceScale();
}
//=============================================================================
bool
RenderOpenGL::pointMarkers(const std::vector<double>& xs, const std::vector<double>& ys,
    const std::vector<double>& markerSizes, const std::vector<double>& rgbaColors)
{
    size_t pointCount = std::min(xs.size(), ys.size());
    if (!widget || pointCount == 0 || rgbaColors.size() != pointCount * 4) {
        return false;
    }
    if (markerSizes.size() != 1 && markerSizes.size() < pointCount) {
        return false;
    }

    std::array<PointMarkerBucket, MAX_POINT_MARKER_PIXEL_SIZE + 1> buckets;
    std::vector<int> activeSizes;
    activeSizes.reserve(8);
    const double markerDeviceScale = deviceScale();
    for (size_t i = 0; i < pointCount; ++i) {
        if (!std::isfinite(xs[i]) || !std::isfinite(ys[i])) {
            continue;
        }
        double markerSize = markerSizes.size() == 1 ? markerSizes[0] : markerSizes[i];
        int sizeKey = pointMarkerPixelSize(markerSize, markerDeviceScale);
        PointMarkerBucket& bucket = buckets[sizeKey];
        if (bucket.vertices.empty()) {
            activeSizes.push_back(sizeKey);
        }
        bucket.vertices.push_back(static_cast<GLfloat>(xs[i]));
        bucket.vertices.push_back(static_cast<GLfloat>(ys[i]));
        bucket.vertices.push_back(0.0f);
        size_t colorIndex = i * 4;
        bucket.colors.push_back(static_cast<GLfloat>(rgbaColors[colorIndex]));
        bucket.colors.push_back(static_cast<GLfloat>(rgbaColors[colorIndex + 1]));
        bucket.colors.push_back(static_cast<GLfloat>(rgbaColors[colorIndex + 2]));
        bucket.colors.push_back(static_cast<GLfloat>(rgbaColors[colorIndex + 3]));
    }
    if (activeSizes.empty()) {
        return true;
    }

    disableLighting();
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_CULL_FACE);
    glDepthMask(GL_FALSE);
    glDisable(GL_DEPTH_TEST);
    // Filled scatter markers are round: antialias the points into circles
    // (plain GL_POINTS are square). Smoothing needs alpha blending.
    glEnable(GL_POINT_SMOOTH);
    glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
    const GLboolean blendWasEnabled = glIsEnabled(GL_BLEND);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
    for (int sizeKey : activeSizes) {
        PointMarkerBucket& bucket = buckets[sizeKey];
        glPointSize(static_cast<GLfloat>(sizeKey));
        glVertexPointer(3, GL_FLOAT, 0, bucket.vertices.data());
        glColorPointer(4, GL_FLOAT, 0, bucket.colors.data());
        glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(bucket.vertices.size() / 3));
    }
    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);
    glPointSize(1.0f);
    glDisable(GL_POINT_SMOOTH);
    if (!blendWasEnabled) {
        glDisable(GL_BLEND);
    }
    glDepthMask(GL_TRUE);
    return true;
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
void
RenderOpenGL::lookAt(double eyex, double eyey, double eyez, double centerx, double centery,
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
    double tx = -(sx * eyex + sy * eyey + sz * eyez);
    double ty = -(ux * eyex + uy * eyey + uz * eyez);
    double tz = fx * eyex + fy * eyey + fz * eyez;
    model[0] = sx;
    model[4] = sy;
    model[8] = sz;
    model[12] = tx;
    model[1] = ux;
    model[5] = uy;
    model[9] = uz;
    model[13] = ty;
    model[2] = -fx;
    model[6] = -fy;
    model[10] = -fz;
    model[14] = tz;
    model[3] = 0;
    model[7] = 0;
    model[11] = 0;
    model[15] = 1;
    loadModelMatrix();
}
//=============================================================================
void
RenderOpenGL::scale(double sx, double sy, double sz)
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
    loadModelMatrix();
}
//=============================================================================
void
RenderOpenGL::pushModelMatrix(const double* matrix16ColumnMajor)
{
    std::array<double, 16> saved;
    for (int i = 0; i < 16; ++i) {
        saved[i] = model[i];
    }
    m_modelStack.push_back(saved);
    m_dataClipEnabledStack.push_back(m_dataClip.enabled ? 1 : 0);
    m_dataClip.enabled = false;
    if (matrix16ColumnMajor) {
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
    loadModelMatrix();
}
//=============================================================================
void
RenderOpenGL::popModelMatrix()
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
    loadModelMatrix();
}
//=============================================================================
void
RenderOpenGL::setSurfaceTexture(const RGBAImageView& texture)
{
    if (texture.pixels == nullptr || texture.width <= 0 || texture.height <= 0) {
        clearSurfaceTexture();
        return;
    }
    m_surfaceTextureW = texture.width;
    m_surfaceTextureH = texture.height;
    const int stride = texture.bytesPerLine > 0 ? texture.bytesPerLine : texture.width * 4;
    m_surfaceTextureRgba.resize(static_cast<size_t>(texture.width) * texture.height * 4);
    for (int row = 0; row < texture.height; ++row) {
        std::memcpy(m_surfaceTextureRgba.data() + static_cast<size_t>(row) * texture.width * 4,
            texture.pixels + static_cast<size_t>(row) * stride,
            static_cast<size_t>(texture.width) * 4);
    }
    // The surface opacity is the vertex alpha (FaceAlpha), as in the software
    // renderers: the CData->RGBA bake leaves the texel alpha at zero, and with
    // GL_MODULATE and blending that alpha would make the whole surface vanish.
    for (size_t k = 3; k < m_surfaceTextureRgba.size(); k += 4) {
        m_surfaceTextureRgba[k] = 255;
    }
    m_surfaceTextureActive = true;
}
//=============================================================================
void
RenderOpenGL::clearSurfaceTexture()
{
    m_surfaceTextureActive = false;
    m_surfaceTextureW = 0;
    m_surfaceTextureH = 0;
    m_surfaceTextureRgba.clear();
}
//=============================================================================
void
RenderOpenGL::mapPoint(double x, double y, double z, double& a, double& b, double& c)
{
    a = model[0] * x + model[4] * y + model[8] * z + model[12];
    b = model[1] * x + model[5] * y + model[9] * z + model[13];
    c = model[2] * x + model[6] * y + model[10] * z + model[14];
}
//=============================================================================
void
RenderOpenGL::project(
    double left, double right, double bottom, double top, double znear, double zfar)
{
    double tx = -(right + left) / (right - left);
    double ty = -(top + bottom) / (top - bottom);
    double tz = -(zfar + znear) / (zfar - znear);
    for (double& i : proj) {
        i = 0;
    }
    proj[0] = 2 / (right - left);
    proj[5] = 2 / (top - bottom);
    proj[10] = -2 / (zfar - znear);
    proj[12] = tx;
    proj[13] = ty;
    proj[14] = tz;
    proj[15] = 1;
    loadProjectionMatrix();
}
//=============================================================================
void
RenderOpenGL::projectPerspective(
    double left, double right, double bottom, double top, double znear, double zfar)
{
    buildFrustumProjectionMatrix(proj, left, right, bottom, top, znear, zfar);
    loadProjectionMatrix();
}
//=============================================================================
void
RenderOpenGL::viewport(double x0, double y0, double width, double height)
{
    viewp[0] = static_cast<int>(x0);
    viewp[1] = static_cast<int>(y0);
    viewp[2] = static_cast<int>(width);
    viewp[3] = static_cast<int>(height);
    if (widget) {
        glViewport(viewp[0], viewp[1], viewp[2], viewp[3]);
    }
}
//=============================================================================
void
RenderOpenGL::clipToEntireFrame()
{
    // Axes Clipping == 'off': widen the GL clip from the axes rect to the whole frame
    // so overflowing geometry spills into the figure. The NDC mapping must still
    // place content in the axes sub-rect, so bake an NDC scale+translate into the
    // projection; toPixels and the GL pipeline then agree on pixel positions.
    double ax = viewp[0];
    double ay = viewp[1];
    double aw = viewp[2];
    double ah = viewp[3];
    if (aw <= 0 || ah <= 0 || m_width <= 0 || m_height <= 0) {
        return;
    }
    double sx = aw / m_width;
    double sy = ah / m_height;
    double tx = (2.0 * (ax - m_x1) + aw - m_width) / m_width;
    double ty = (2.0 * (ay - m_y1) + ah - m_height) / m_height;
    // proj is column-major (proj[col * 4 + row]); row 0 is x-clip, row 1 y-clip,
    // row 3 the perspective w. The remap is applied after the divide by w, i.e.
    // clip' = s * clip + t * w, so it works for both the orthographic (w == 1)
    // and the perspective projection.
    for (int c = 0; c < 4; ++c) {
        double r0 = proj[c * 4 + 0];
        double r1 = proj[c * 4 + 1];
        double r3 = proj[c * 4 + 3];
        proj[c * 4 + 0] = sx * r0 + tx * r3;
        proj[c * 4 + 1] = sy * r1 + ty * r3;
    }
    viewp[0] = static_cast<int>(m_x1);
    viewp[1] = static_cast<int>(m_y1);
    viewp[2] = static_cast<int>(m_width);
    viewp[3] = static_cast<int>(m_height);
    if (widget) {
        glViewport(viewp[0], viewp[1], viewp[2], viewp[3]);
    }
    loadProjectionMatrix();
}
//=============================================================================
void
RenderOpenGL::toPixelsImpl(
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
    clipped
        = (xclip < -1) || (xclip > 1) || (yclip < -1) || (yclip > 1) || (zclip < -1) || (zclip > 1);
}
//=============================================================================
void
RenderOpenGL::toPixels(double x, double y, double z, double& a, double& b, double& c, bool& clipped)
{
    double xclip, yclip, zclip;
    toPixelsImpl(x, y, z, xclip, yclip, zclip, clipped);
    a = viewp[0] + (1 + xclip) / 2.0 * viewp[2];
    b = viewp[1] + (1 + yclip) / 2.0 * viewp[3];
    c = zclip;
}
//=============================================================================
void
RenderOpenGL::toPixels(double x, double y, double z, double& a, double& b, bool& clipped)
{
    double c;
    toPixels(x, y, z, a, b, c, clipped);
}
//=============================================================================
void
RenderOpenGL::toPixels(double x, double y, double z, double& a, double& b)
{
    bool clipped;
    toPixels(x, y, z, a, b, clipped);
}
//=============================================================================
void
RenderOpenGL::toPixels(double x, double y, double z, int& a, int& b)
{
    double da, db;
    toPixels(x, y, z, da, db);
    a = static_cast<int>(da);
    b = static_cast<int>(db);
}
//=============================================================================
QPointF
RenderOpenGL::map(double x, double y, double z)
{
    double a, b;
    toPixels(x, y, z, a, b);
    return { a, b };
}
//=============================================================================
void
RenderOpenGL::setupDirectDraw()
{
    for (int i = 0; i < 16; i++) {
        save_model[i] = model[i];
        save_proj[i] = proj[i];
        model[i] = 0;
        proj[i] = 0;
    }
    for (int i = 0; i < 4; i++) {
        save_viewp[i] = viewp[i];
    }
    model[0] = 1;
    model[5] = 1;
    model[10] = 1;
    model[15] = 1;

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
    glMatrixMode(GL_PROJECTION);
    glPushMatrix();

    loadModelMatrix();
    viewport(m_x1, m_y1, m_width, m_height);
    project(m_x1, m_x1 + m_width, m_y1, m_y1 + m_height, -1, 1);
    disableLighting();
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_CULL_FACE);
#if defined(GL_COLOR_LOGIC_OP)
    glDisable(GL_COLOR_LOGIC_OP);
#endif
#if defined(GL_POLYGON_OFFSET_FILL)
    glDisable(GL_POLYGON_OFFSET_FILL);
#endif
#if defined(GL_POLYGON_OFFSET_LINE)
    glDisable(GL_POLYGON_OFFSET_LINE);
#endif
    glDepthMask(GL_FALSE);
    glDisable(GL_DEPTH_TEST);
    // Direct draw switches to a pixel-space projection; the data-limit clip
    // planes (defined in the axes coordinate frame) must not apply to markers
    // and text drawn here, so pause them until releaseDirectDraw.
    disableDataClipPlanes();
    resetBlendState();
    inDirect = true;
}
//=============================================================================
void
RenderOpenGL::releaseDirectDraw()
{
    glMatrixMode(GL_PROJECTION);
    glPopMatrix();
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix();
    for (int i = 0; i < 16; i++) {
        model[i] = save_model[i];
        proj[i] = save_proj[i];
    }
    for (int i = 0; i < 4; i++) {
        viewp[i] = save_viewp[i];
    }
    glViewport(viewp[0], viewp[1], viewp[2], viewp[3]);
    if (widget) {
        glDisable(GL_SCISSOR_TEST);
    }
    loadProjectionMatrix();
    loadModelMatrix();
    glDepthMask(GL_TRUE);
    glEnable(GL_DEPTH_TEST);
    applyDataClipPlanes();
    inDirect = false;
}
//=============================================================================
void
RenderOpenGL::setClipBox(double x0, double y0, double width, double height)
{
    if (!widget) {
        return;
    }
    int ix = static_cast<int>(std::floor(x0));
    int iy = static_cast<int>(std::floor(y0));
    int iw = std::max(0, static_cast<int>(std::ceil(width)));
    int ih = std::max(0, static_cast<int>(std::ceil(height)));
    glEnable(GL_SCISSOR_TEST);
    glScissor(ix, iy, iw, ih);
}
//=============================================================================
void
RenderOpenGL::clearClipBox()
{
    if (widget) {
        glDisable(GL_SCISSOR_TEST);
    }
}
//=============================================================================
void
RenderOpenGL::applyDataClipPlanes()
{
    if (!widget || !m_dataClip.enabled) {
        return;
    }
    // Clip plane equations are given in the axes "mapped" (object) coordinate
    // system; glClipPlane transforms them by the current modelview, so load it
    // first. Each plane keeps the half-space where a * x + b * y + c * z + d >= 0.
    glMatrixMode(GL_MODELVIEW);
    glLoadMatrixd(model);
    const GLdouble planes[6][4] = {
        { 1.0, 0.0, 0.0, -m_dataClip.xmin },
        { -1.0, 0.0, 0.0, m_dataClip.xmax },
        { 0.0, 1.0, 0.0, -m_dataClip.ymin },
        { 0.0, -1.0, 0.0, m_dataClip.ymax },
        { 0.0, 0.0, 1.0, -m_dataClip.zmin },
        { 0.0, 0.0, -1.0, m_dataClip.zmax },
    };
    for (int i = 0; i < 6; i++) {
        glClipPlane(GL_CLIP_PLANE0 + i, planes[i]);
        glEnable(GL_CLIP_PLANE0 + i);
    }
}
//=============================================================================
void
RenderOpenGL::disableDataClipPlanes()
{
    if (!widget) {
        return;
    }
    for (int i = 0; i < 6; i++) {
        glDisable(GL_CLIP_PLANE0 + i);
    }
}
//=============================================================================
void
RenderOpenGL::setDataClipBox(const RenderClipBox& box)
{
    m_dataClip = box;
    applyDataClipPlanes();
}
//=============================================================================
void
RenderOpenGL::clearDataClipBox()
{
    m_dataClip.enabled = false;
    disableDataClipPlanes();
}
//=============================================================================
void
RenderOpenGL::getModelviewMatrix(std::vector<double>& amodel)
{
    amodel.assign(std::begin(model), std::end(model));
}
//=============================================================================
void
RenderOpenGL::depth(bool on)
{
    if (on) {
        glEnable(GL_DEPTH_TEST);
    } else {
        glDisable(GL_DEPTH_TEST);
    }
}
//=============================================================================
void
RenderOpenGL::rect(double x1, double y1, double x2, double y2)
{
    glBegin(GL_LINE_LOOP);
    glVertex2d(x1, y1);
    glVertex2d(x2, y1);
    glVertex2d(x2, y2);
    glVertex2d(x1, y2);
    glEnd();
}
//=============================================================================
void
RenderOpenGL::rectFill(double x1, double y1, double x2, double y2)
{
    glRectd(x1, y1, x2, y2);
}
//=============================================================================
int
RenderOpenGL::circleSegmentCount(double radius)
{
    int segments = static_cast<int>(std::ceil(radius * 4.0));
    return std::clamp(segments, 24, 96);
}
//=============================================================================
void
RenderOpenGL::circle(double x, double y, double radius)
{
    if (radius <= 0) {
        return;
    }
    int segments = circleSegmentCount(radius);
    glBegin(GL_LINE_LOOP);
    for (int i = 0; i < segments; ++i) {
        double theta = 2.0 * M_PI * static_cast<double>(i) / static_cast<double>(segments);
        glVertex2d(x + radius * std::cos(theta), y + radius * std::sin(theta));
    }
    glEnd();
}
//=============================================================================
void
RenderOpenGL::circleFill(double x, double y, double radius)
{
    if (radius <= 0) {
        return;
    }
    int segments = circleSegmentCount(radius);
    GLboolean polygonSmoothEnabled = glIsEnabled(GL_POLYGON_SMOOTH);
    if (graphicsSmoothing) {
        glEnable(GL_POLYGON_SMOOTH);
        glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
    }
    glBegin(GL_TRIANGLE_FAN);
    glVertex2d(x, y);
    for (int i = 0; i <= segments; ++i) {
        double theta = 2.0 * M_PI * static_cast<double>(i) / static_cast<double>(segments);
        glVertex2d(x + radius * std::cos(theta), y + radius * std::sin(theta));
    }
    glEnd();
    if (!polygonSmoothEnabled) {
        glDisable(GL_POLYGON_SMOOTH);
    }
}
//=============================================================================
static bool
isNoColor(const std::vector<double>& color)
{
    return color.empty() || color[0] < 0;
}
//=============================================================================
static void
setGLColor(const std::vector<double>& color)
{
    double alpha = color.size() > 3 ? color[3] : 1.0;
    glColor4d(color[0], color.size() > 1 ? color[1] : color[0],
        color.size() > 2 ? color[2] : color[0], alpha);
}
//=============================================================================
static double
pieAngleRadians(double angleDegrees)
{
    return -angleDegrees * M_PI / 180.0;
}
//=============================================================================
void
RenderOpenGL::drawPieSlice(double centerX, double centerY, double outerRadius, double innerRadius,
    double startAngle, double spanAngle, const std::vector<double>& faceColor,
    const std::vector<double>& edgeColor, double edgeLineWidth)
{
    if (outerRadius <= 0 || std::fabs(spanAngle) <= 1e-12) {
        return;
    }
    innerRadius = std::max(0.0, std::min(innerRadius, outerRadius));
    const int segments = std::max(8, static_cast<int>(std::ceil(std::fabs(spanAngle) / 4.0)));

    if (!isNoColor(faceColor)) {
        setGLColor(faceColor);
        if (innerRadius > 0) {
            glBegin(GL_QUAD_STRIP);
            for (int i = 0; i <= segments; ++i) {
                double t = startAngle + spanAngle * static_cast<double>(i) / segments;
                double radians = pieAngleRadians(t);
                double cs = std::cos(radians);
                double sn = std::sin(radians);
                glVertex2d(centerX + outerRadius * cs, centerY + outerRadius * sn);
                glVertex2d(centerX + innerRadius * cs, centerY + innerRadius * sn);
            }
            glEnd();
        } else {
            glBegin(GL_TRIANGLE_FAN);
            glVertex2d(centerX, centerY);
            for (int i = 0; i <= segments; ++i) {
                double t = startAngle + spanAngle * static_cast<double>(i) / segments;
                double radians = pieAngleRadians(t);
                glVertex2d(centerX + outerRadius * std::cos(radians),
                    centerY + outerRadius * std::sin(radians));
            }
            glEnd();
        }
    }

    if (!isNoColor(edgeColor) && edgeLineWidth > 0) {
        setGLColor(edgeColor);
        lineWidth(edgeLineWidth);
        glBegin(GL_LINE_STRIP);
        for (int i = 0; i <= segments; ++i) {
            double t = startAngle + spanAngle * static_cast<double>(i) / segments;
            double radians = pieAngleRadians(t);
            glVertex2d(centerX + outerRadius * std::cos(radians),
                centerY + outerRadius * std::sin(radians));
        }
        if (innerRadius <= 0) {
            glVertex2d(centerX, centerY);
            double radians = pieAngleRadians(startAngle);
            glVertex2d(centerX + outerRadius * std::cos(radians),
                centerY + outerRadius * std::sin(radians));
        }
        glEnd();
    }
    restoreCurrentColor();
}
//=============================================================================
bool
RenderOpenGL::shouldCompileDisplayList(std::unordered_set<uint64_t>& candidates, uint64_t cacheKey)
{
    constexpr size_t MAX_DISPLAY_LIST_CANDIDATES = 256;
    auto [candidate, inserted] = candidates.insert(cacheKey);
    if (inserted) {
        if (candidates.size() > MAX_DISPLAY_LIST_CANDIDATES) {
            candidates.clear();
            candidates.insert(cacheKey);
        }
        return false;
    }
    candidates.erase(candidate);
    return true;
}
//=============================================================================
void
RenderOpenGL::quadStrips(const std::vector<std::vector<coloredPoint>>& faces, bool flatfaces,
    const std::vector<std::vector<coloredPoint>>& edges, bool flatedges, meshStyle meshstyle,
    double lineWidthValue, const std::wstring& lineStyle,
    enum LightingMode::LightingMode faceLighting, enum LightingMode::LightingMode edgeLighting,
    uint64_t geometryToken)
{
    const bool hasLights = lightingScene.enabled && !lightingScene.lights.empty();
    bool lightFaces = hasLights && faceLighting != LightingMode::None;
    bool lightEdges = hasLights && edgeLighting != LightingMode::None;
    // A translucent surface (FaceAlpha < 1) is depth-sorted for the current view,
    // so it must be drawn back-to-front with depth writes off and never cached as
    // a display list (the sort would go stale after a rotation).
    bool translucentSurface = false;
    for (const std::vector<coloredPoint>& strip : faces) {
        for (const coloredPoint& p : strip) {
            if (p.a < 0.999) {
                translucentSurface = true;
                break;
            }
        }
        if (translucentSurface) {
            break;
        }
    }
    // Dense-mesh fast path (surf/mesh/pcolor rotation): an opaque, unlit, untextured
    // surface is drawn from cached packed float vertex arrays. Lit, textured or
    // translucent surfaces need per-frame state and keep the path below.
    const bool canUseFloatArrays = geometryToken != 0 && !m_surfaceTextureActive
        && !translucentSurface && !lightFaces && !lightEdges;
    if (canUseFloatArrays) {
        const uint64_t vaKey = surfaceDisplayListCacheKey(geometryToken, flatfaces, flatedges,
            meshstyle, lineWidthValue, lineStyle, faceLighting, edgeLighting);
        auto found = surfaceVertexArrayCache.find(vaKey);
        if (found == surfaceVertexArrayCache.end()) {
            const bool wantEdges = lineStyle != GO_PROPERTY_VALUE_NONE_STR;
            SurfaceVertexArrays arrays;
            buildSurfaceVertexArrays(faces, edges, meshstyle, wantEdges, arrays.facePos,
                arrays.faceColor, arrays.edgePos, arrays.edgeColor);
            if (surfaceVertexArrayCache.size() > 32) {
                surfaceVertexArrayCache.clear();
            }
            found = surfaceVertexArrayCache.emplace(vaKey, std::move(arrays)).first;
        }
        const SurfaceVertexArrays& va = found->second;
        glDisable(GL_TEXTURE_2D);
        glDisable(GL_CULL_FACE);
        glDepthFunc(GL_LEQUAL);
        glEnable(GL_DEPTH_TEST);
        glEnableClientState(GL_VERTEX_ARRAY);
        glEnableClientState(GL_COLOR_ARRAY);
        if (!va.facePos.empty()) {
            glEnable(GL_POLYGON_OFFSET_FILL);
            glPolygonOffset(2, 2);
            glDepthMask(GL_TRUE);
            glShadeModel(flatfaces && faceLighting != LightingMode::Gouraud ? GL_FLAT : GL_SMOOTH);
            glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
            glVertexPointer(3, GL_FLOAT, 0, va.facePos.data());
            glColorPointer(4, GL_FLOAT, 0, va.faceColor.data());
            glDrawArrays(GL_TRIANGLES, 0, static_cast<GLsizei>(va.facePos.size() / 3));
            glDisable(GL_POLYGON_OFFSET_FILL);
        }
        if (!va.edgePos.empty()) {
            lineWidth(lineWidthValue);
            setLineStyleGL(lineStyle);
            glDepthMask(GL_FALSE);
            glShadeModel(flatedges && edgeLighting != LightingMode::Gouraud ? GL_FLAT : GL_SMOOTH);
            glVertexPointer(3, GL_FLOAT, 0, va.edgePos.data());
            glColorPointer(4, GL_FLOAT, 0, va.edgeColor.data());
            glDrawArrays(GL_LINES, 0, static_cast<GLsizei>(va.edgePos.size() / 3));
            glDepthMask(GL_TRUE);
            glDisable(GL_LINE_STIPPLE);
        }
        glDisableClientState(GL_COLOR_ARRAY);
        glDisableClientState(GL_VERTEX_ARRAY);
        restoreCurrentColor();
        return;
    }
    // A cache hit replays static geometry. Cache admission happens only after
    // the same key is seen twice, so a stream of one-shot animation tokens is
    // drawn directly instead of compiling display lists that cannot be reused.
    // Textured surfaces bind a transient GL texture per draw and are never cached.
    const bool canCacheDisplayList
        = geometryToken != 0 && !m_surfaceTextureActive && !translucentSurface;
    uint64_t cacheKey = 0;
    if (canCacheDisplayList) {
        cacheKey = surfaceDisplayListCacheKey(geometryToken, flatfaces, flatedges, meshstyle,
            lineWidthValue, lineStyle, faceLighting, edgeLighting);
        if (lightFaces || lightEdges) {
            combineLightingStateCacheHash(cacheKey, lightingScene, lightingMaterial, model);
        }
        auto found = surfaceDisplayListCache.find(cacheKey);
        if (found != surfaceDisplayListCache.end() && glIsList(found->second)) {
            glCallList(found->second);
            restoreCurrentColor();
            return;
        }
    }
    auto drawSurfaceNow = [&]() {
        configureLighting(lightFaces);
        // A surface emits its quads clockwise as seen from the camera. With the default
        // counter-clockwise convention they all count as back facing and two-sided
        // lighting flips the normal. Culling is off, so this only selects the normal
        // direction. Restored before returning: a patch winding comes from the caller.
        glFrontFace(GL_CW);
        glDisable(GL_CULL_FACE);
        glDepthMask(GL_TRUE);
        glDepthFunc(GL_LEQUAL);
        glEnable(GL_DEPTH_TEST);
        glEnable(GL_POLYGON_OFFSET_FILL);
        glPolygonOffset(2, 2);
        glShadeModel(flatfaces && faceLighting != LightingMode::Gouraud ? GL_FLAT : GL_SMOOTH);
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
        // 'texturemap' FaceColor: sample the image as a GL texture per fragment.
        // GL_MODULATE multiplies the texel by the lit vertex colour, so the texture
        // stays crisp at any mesh resolution and still receives lighting.
        GLuint surfaceTexId = 0;
        const bool textured = m_surfaceTextureActive && m_surfaceTextureW > 0
            && m_surfaceTextureH > 0 && !m_surfaceTextureRgba.empty();
        if (textured) {
            glGenTextures(1, &surfaceTexId);
            glBindTexture(GL_TEXTURE_2D, surfaceTexId);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
            glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
            glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, m_surfaceTextureW, m_surfaceTextureH, 0,
                GL_RGBA, GL_UNSIGNED_BYTE, m_surfaceTextureRgba.data());
            glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
            glEnable(GL_TEXTURE_2D);
        } else {
            glDisable(GL_TEXTURE_2D);
        }
        const bool allFaceQuads = std::all_of(faces.begin(), faces.end(),
            [](const std::vector<coloredPoint>& qlist) { return qlist.size() == 4; });
        const bool packedFaceQuads
            = faces.size() == 1 && faces[0].size() > 4 && (faces[0].size() % 4) == 0;
        if (textured) {
            // Immediate mode so each vertex carries its texture coordinate; the
            // array fast paths below do not upload one. The texture was uploaded
            // with its first (top) row as data row 0, which is GL texel row 0, so
            // (u, v) maps a vertex to the same texel the web backend samples.
            for (const auto& qlist : faces) {
                glBegin(GL_QUAD_STRIP);
                for (const auto& pt : qlist) {
                    glColor4d(pt.r, pt.g, pt.b, pt.a);
                    glNormal3d(pt.nx, pt.ny, pt.nz);
                    glTexCoord2d(pt.u, pt.v);
                    glVertex3d(pt.x, pt.y, pt.z);
                }
                glEnd();
            }
        } else if (translucentSurface) {
            // Paint independent quads back-to-front with depth writes off so
            // overlapping translucent faces composite instead of culling each other.
            std::vector<coloredPoint> quadVertices;
            if (packedFaceQuads) {
                const std::vector<coloredPoint>& packed = faces[0];
                quadVertices.reserve(packed.size());
                for (size_t k = 0; k + 3 < packed.size(); k += 4) {
                    quadVertices.push_back(packed[k]);
                    quadVertices.push_back(packed[k + 1]);
                    quadVertices.push_back(packed[k + 3]);
                    quadVertices.push_back(packed[k + 2]);
                }
            } else {
                for (const std::vector<coloredPoint>& strip : faces) {
                    for (size_t b = 0; b + 3 < strip.size(); b += 2) {
                        quadVertices.push_back(strip[b]);
                        quadVertices.push_back(strip[b + 1]);
                        quadVertices.push_back(strip[b + 3]);
                        quadVertices.push_back(strip[b + 2]);
                    }
                }
            }
            glEnable(GL_BLEND);
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
            // Keep the grabbed frame opaque: masking the alpha channel off stops the
            // source-over blend from dropping the destination alpha below one, which
            // would otherwise wash the surface out when the RGBA frame is flattened
            // over the white page (see drawPatch for the full rationale).
            glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_FALSE);
            glDepthMask(GL_FALSE);
            drawSurfaceQuadsSortedByDepth(quadVertices, model);
            glDepthMask(GL_TRUE);
            glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
        } else if (packedFaceQuads) {
            drawSurfacePackedQuadsArray(faces[0]);
        } else if (allFaceQuads) {
            drawSurfaceQuadsArray(faces);
        } else {
            for (const auto& qlist : faces) {
                glBegin(GL_QUAD_STRIP);
                for (const auto& pt : qlist) {
                    glColor4d(pt.r, pt.g, pt.b, pt.a);
                    glNormal3d(pt.nx, pt.ny, pt.nz);
                    glVertex3d(pt.x, pt.y, pt.z);
                }
                glEnd();
            }
        }
        if (textured) {
            glDisable(GL_TEXTURE_2D);
            glBindTexture(GL_TEXTURE_2D, 0);
            glDeleteTextures(1, &surfaceTexId);
        }
        glDisable(GL_POLYGON_OFFSET_FILL);

        if (lineStyle == GO_PROPERTY_VALUE_NONE_STR) {
            glFrontFace(GL_CCW);
            disableLighting();
            return;
        }
        configureLighting(lightEdges);
        lineWidth(lineWidthValue);
        setLineStyleGL(lineStyle);
        glDepthMask(GL_FALSE);
        glShadeModel(flatedges && edgeLighting != LightingMode::Gouraud ? GL_FLAT : GL_SMOOTH);
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
        drawSurfaceQuadStripEdgesArray(edges, meshstyle);
        glDepthMask(GL_TRUE);
        glDisable(GL_LINE_STIPPLE);
        glFrontFace(GL_CCW);
        disableLighting();
    };

    if (!canCacheDisplayList) {
        drawSurfaceNow();
        restoreCurrentColor();
        return;
    }
    if (!shouldCompileDisplayList(surfaceDisplayListCandidates, cacheKey)) {
        drawSurfaceNow();
        restoreCurrentColor();
        return;
    }
    auto& cache = surfaceDisplayListCache;
    if (cache.size() > 256) {
        clearDisplayListCaches();
    }
    GLuint list = glGenLists(1);
    if (list == 0) {
        drawSurfaceNow();
        restoreCurrentColor();
        return;
    }
    glNewList(list, GL_COMPILE);
    drawSurfaceNow();
    glEndList();
    cache[cacheKey] = list;
    glCallList(list);
    restoreCurrentColor();
}
//=============================================================================
