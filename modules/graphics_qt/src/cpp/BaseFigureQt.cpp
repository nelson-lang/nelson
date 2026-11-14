//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtGui/QWindow>
#include <QtGui/QGuiApplication>
#include <QtGui/QImage>
#include <QtGui/QPainter>
#include <QtGui/QPixmap>
#include <QtGui/QRegion>
#include <QtGui/QSurfaceFormat>
#include <QtGui/QOpenGLExtraFunctions>
#include <QtOpenGL/QOpenGLFramebufferObject>
#include <QtWidgets/QApplication>
#include <QtCore/QCoreApplication>
#include <QtCore/QElapsedTimer>
#include <QtCore/QEvent>
#include <QtCore/QEventLoop>
#if defined(_WIN32)
#include <windows.h>
#endif
#if defined(__APPLE__)
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif
#include "BaseFigureQt.hpp"
#include "GraphicsNativeWidgetCapture.hpp"
#include "RenderOpenGL.hpp"
#include "RenderQt.hpp"
#include "GOWindow.hpp"
#include "GOPropertyNames.hpp"
#include "GOHelpers.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOAxis.hpp"
#include "ScreenInfo.hpp"
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
windowCanPresentFrame(const QWidget* widget);
//=============================================================================
static bool
imageHasDynamicRange(const QImage& image)
{
    if (image.isNull() || image.width() <= 0 || image.height() <= 0) {
        return false;
    }
    QRgb firstPixel = image.pixel(0, 0);
    for (int y = 0; y < image.height(); ++y) {
        for (int x = 0; x < image.width(); ++x) {
            if (image.pixel(x, y) != firstPixel) {
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
static int
toDevicePixels(int logicalSize, qreal ratio)
{
    int deviceSize = static_cast<int>(logicalSize * ratio + 0.5);
    return deviceSize > 0 ? deviceSize : logicalSize;
}
//=============================================================================
constexpr double DEVICE_PIXEL_RATIO_TOLERANCE = 1e-6;
constexpr int RESIZE_COALESCE_DELAY_MS = 80;
//=============================================================================
static bool
isCloseTo(int a, int b)
{
    int diff = a > b ? a - b : b - a;
    return diff <= 1;
}
//=============================================================================
static bool
figureRequestsSize(GOFigure* figure, const QSize& size)
{
    if (!figure) {
        return false;
    }
    std::vector<double> position = figure->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    return position.size() >= 4 && std::fabs(position[2] - size.width()) <= 1.0
        && std::fabs(position[3] - size.height()) <= 1.0;
}
//=============================================================================
static bool
hasPrimaryScreen()
{
    return QGuiApplication::primaryScreen() != nullptr;
}
//=============================================================================
// getframe captures a figure with axes at device resolution (Position x
// DevicePixelRatio) and a figure without axes at its logical pixel size. This tells
// which case applies (GOPolarAxes derives from GOAxis, so it counts as axes).
static bool
figureHasAxes(GOFigure* figure)
{
    if (!figure) {
        return false;
    }
    GOGObjectsProperty* children = static_cast<GOGObjectsProperty*>(
        figure->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return false;
    }
    for (int64 handle : children->data()) {
        if (dynamic_cast<GOAxis*>(findGraphicsObject(handle, false))) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static bool
hasVisibleChildWidgets(QWidget* widget)
{
    if (!widget) {
        return false;
    }
    for (QObject* object : widget->children()) {
        QWidget* child = qobject_cast<QWidget*>(object);
        if (child && !child->isWindow() && child->isVisibleTo(widget)
            && !child->geometry().isEmpty()) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static void
renderChildWidgets(QWidget* widget, QImage& image)
{
    if (!widget || image.isNull() || widget->width() <= 0 || widget->height() <= 0) {
        return;
    }
    if (shouldAvoidNativeWidgetCapture()) {
        return;
    }

    const double xScale = static_cast<double>(image.width()) / static_cast<double>(widget->width());
    const double yScale
        = static_cast<double>(image.height()) / static_cast<double>(widget->height());

    image.setDevicePixelRatio(1.0);
    QPainter painter(&image);
    for (QObject* object : widget->children()) {
        QWidget* child = qobject_cast<QWidget*>(object);
        if (!child || child->isWindow() || !child->isVisibleTo(widget)
            || child->geometry().isEmpty()) {
            continue;
        }
        QImage childImage = child->grab().toImage().convertToFormat(QImage::Format_ARGB32);
        if (childImage.isNull()) {
            continue;
        }
        QRectF target(child->x() * xScale, child->y() * yScale, child->width() * xScale,
            child->height() * yScale);
        painter.drawImage(target, childImage);
    }
}
//=============================================================================
static QImage
renderFigureToImage(BaseFigureQt* widget, GOFigure* figure)
{
    if (!widget || !figure) {
        return QImage();
    }
    // Start from the figure's logical pixel size. getLayoutWidth()/getLayoutHeight()
    // already divide the render size by the device pixel ratio; widget->width()/height()
    // are the logical fallback for a hidden figure whose GL size was never established.
    int renderWidth = figure->getLayoutWidth();
    int renderHeight = figure->getLayoutHeight();
    if (renderWidth <= 0 || renderHeight <= 0) {
        renderWidth = widget->width();
        renderHeight = widget->height();
    }
    if (renderWidth <= 0 || renderHeight <= 0) {
        return QImage();
    }
    // A figure with axes is captured at device resolution (a 500x400 figure with a plot
    // at 125% yields 625x500). An empty figure keeps its logical pixel size.
    if (figureHasAxes(figure)) {
        double ratio = figure->findScalarDoubleProperty(GO_DEVICE_PIXEL_RATIO_PROPERTY_NAME_STR);
        if (ratio > 0.0) {
            renderWidth = static_cast<int>(std::lround(renderWidth * ratio));
            renderHeight = static_cast<int>(std::lround(renderHeight * ratio));
        }
    }
    QImage image(renderWidth, renderHeight, QImage::Format_ARGB32);
    QPainter painter(&image);
    RenderQt gc(&painter, 0, 0, renderWidth, renderHeight, L"Image");
    figure->paintMe(gc);
    painter.end();
    renderChildWidgets(widget, image);
    return image.convertToFormat(QImage::Format_ARGB32);
}
//=============================================================================
static QImage
captureWidgetToImage(BaseFigureQt* widget)
{
    if (!widget) {
        return QImage();
    }
    if (shouldAvoidNativeWidgetCapture() && hasVisibleChildWidgets(widget)) {
        return QImage();
    }
    QImage grabbed = widget->grab().toImage().convertToFormat(QImage::Format_ARGB32);
    if (imageHasDynamicRange(grabbed)) {
        return grabbed;
    }

    int renderWidth = widget->getRenderWidth();
    int renderHeight = widget->getRenderHeight();
    if (renderWidth <= 0 || renderHeight <= 0) {
        qreal ratio = widget->devicePixelRatioF();
        renderWidth = toDevicePixels(widget->width(), ratio);
        renderHeight = toDevicePixels(widget->height(), ratio);
    }
    if (renderWidth <= 0 || renderHeight <= 0) {
        return QImage();
    }

    QImage image(renderWidth, renderHeight, QImage::Format_ARGB32);
    image.fill(Qt::transparent);
    image.setDevicePixelRatio(1.0);
    QPainter painter(&image);
    double xScale = static_cast<double>(renderWidth) / static_cast<double>(widget->width());
    double yScale = static_cast<double>(renderHeight) / static_cast<double>(widget->height());
    painter.scale(xScale, yScale);
    widget->render(
        &painter, QPoint(0, 0), QRegion(), QWidget::DrawWindowBackground | QWidget::DrawChildren);
    painter.end();
    return image.convertToFormat(QImage::Format_ARGB32);
}
//=============================================================================
void
BaseFigureQt::syncFigureSize(int logicalWidth, int logicalHeight)
{
    qreal ratio = devicePixelRatioF();
    if (hfig && ratio > 0) {
        double previousRatio
            = hfig->findScalarDoubleProperty(GO_DEVICE_PIXEL_RATIO_PROPERTY_NAME_STR);
        if (std::fabs(previousRatio - static_cast<double>(ratio)) > DEVICE_PIXEL_RATIO_TOLERANCE) {
            hfig->setScalarDoubleDefault(
                GO_DEVICE_PIXEL_RATIO_PROPERTY_NAME_STR, static_cast<double>(ratio));
        }
    }
    int widgetDeviceWidth = toDevicePixels(width(), ratio);
    int widgetDeviceHeight = toDevicePixels(height(), ratio);

    if (isCloseTo(logicalWidth, widgetDeviceWidth)
        && isCloseTo(logicalHeight, widgetDeviceHeight)) {
        glWidth = logicalWidth;
        glHeight = logicalHeight;
    } else {
        glWidth = toDevicePixels(logicalWidth, ratio);
        glHeight = toDevicePixels(logicalHeight, ratio);
    }
    hfig->resizeGL(glWidth, glHeight);
}
//=============================================================================
QImage
BaseFigureQt::grabCurrentFrameWithoutRender()
{
    if (!initializedGL || glWidth <= 0 || glHeight <= 0) {
        return QImage();
    }
    // makeCurrent() binds the widget framebuffer, which still holds the last frame: nothing is
    // drawn here
    makeCurrent();
    QImage image;
    // the widget framebuffer can be multisampled (setSamples below), glReadPixels on it is a GL
    // error: blit it into a single sample framebuffer first, as QOpenGLWidget::grabFramebuffer()
    // does
    if (QOpenGLFramebufferObject::hasOpenGLFramebufferBlit()) {
        // a multisample resolve needs the same internal format on both sides
        QOpenGLFramebufferObjectFormat resolvedFormat;
        resolvedFormat.setInternalTextureFormat(textureFormat());
        QOpenGLFramebufferObject resolved(glWidth, glHeight, resolvedFormat);
        // the context functions, not the static gl* entry points: with the software opengl32sw.dll
        // those belong to another library and see no current context
        QOpenGLExtraFunctions* functions = context()->extraFunctions();
        // drain pending errors, bounded so a dead context can not loop forever
        for (int i = 0; i < 16 && functions->glGetError() != GL_NO_ERROR; ++i) { }
        functions->glBindFramebuffer(GL_READ_FRAMEBUFFER, defaultFramebufferObject());
        functions->glBindFramebuffer(GL_DRAW_FRAMEBUFFER, resolved.handle());
        functions->glBlitFramebuffer(
            0, 0, glWidth, glHeight, 0, 0, glWidth, glHeight, GL_COLOR_BUFFER_BIT, GL_NEAREST);
        if (functions->glGetError() == GL_NO_ERROR) {
            image = resolved.toImage();
        }
        functions->glBindFramebuffer(GL_FRAMEBUFFER, defaultFramebufferObject());
    }
    doneCurrent();
    // a null image makes the caller fall back to grabFramebuffer()
    if (image.isNull()) {
        return QImage();
    }
    image.setDevicePixelRatio(devicePixelRatioF());
    return image;
}
//=============================================================================
bool
BaseFigureQt::hasCleanRenderedFrame()
{
    // A frame already painted on screen, at the expected size, with no pending
    // invalidation: reading it back needs no new render.
    return initializedGL && paintSerial > 0 && hfig && !hfig->isRenderingStateInvalid()
        && hasExpectedRenderingSize();
}
//=============================================================================
QImage
BaseFigureQt::grabFrameReusingLastRender()
{
    // grabFramebuffer() forces a full GL re-render (paintGL/glDrawArrays). On a
    // GPU-less host (CI) that render runs on Apple's software OpenGL and can crash
    // with SIGBUS, so there the existing framebuffer is read back instead. But that
    // read-back returns stale or garbled pixels on real hardware whose on-screen
    // surface is not validly composited (a headless or disconnected remote desktop):
    // the frame never settles, so capture comes out blank, partial or shifted.
    // Hardware re-renders correctly and fast through grabFramebuffer(), so the
    // read-back shortcut is taken only on a software renderer, where re-rendering is
    // what is unsafe. Elsewhere the widget framebuffer holds uninitialized memory, so
    // the re-render and its fallbacks stay.
#if defined(__APPLE__)
    if (openglRendererIsSoftware() && hasCleanRenderedFrame()) {
        QImage image = grabCurrentFrameWithoutRender();
        if (!image.isNull()) {
            return image;
        }
    }
#endif
    return grabFramebuffer();
}
#if defined(__APPLE__)
//=============================================================================
// Whether this widget's OpenGL context is served by a software rasterizer (the
// GPU-less CI host) rather than the GPU. Only there does grabFramebuffer()'s
// re-render risk a SIGBUS, so only there is the framebuffer read-back preferred.
// The renderer does not change within a process, so the answer is cached.
bool
BaseFigureQt::openglRendererIsSoftware()
{
    if (m_openglSoftwareState != 0) {
        return m_openglSoftwareState < 0;
    }
    bool software = false;
    if (context() && context()->isValid()) {
        makeCurrent();
        const GLubyte* renderer = context()->functions()->glGetString(GL_RENDERER);
        doneCurrent();
        if (renderer) {
            QString name = QString::fromLatin1(reinterpret_cast<const char*>(renderer)).toLower();
            software = name.contains(QStringLiteral("software"))
                || name.contains(QStringLiteral("llvmpipe"))
                || name.contains(QStringLiteral("swiftshader"));
        }
    }
    m_openglSoftwareState = software ? -1 : 1;
    return software;
}
#endif
//=============================================================================
bool
BaseFigureQt::hasExpectedRenderingSize()
{
    if (width() <= 0 || height() <= 0) {
        return true;
    }
    qreal ratio = devicePixelRatioF();
    return isCloseTo(glWidth, toDevicePixels(width(), ratio))
        && isCloseTo(glHeight, toDevicePixels(height(), ratio));
}
//=============================================================================
bool
BaseFigureQt::event(QEvent* e)
{
    switch (e->type()) {
#if QT_VERSION >= QT_VERSION_CHECK(6, 6, 0)
    case QEvent::DevicePixelRatioChange:
#endif
    case QEvent::ScreenChangeInternal: {
        if (hfig && !hasExpectedRenderingSize()) {
            syncFigureSize(width(), height());
            hfig->setRenderingStateInvalid(true);
            update();
        }
    } break;
    default: {
    } break;
    }
    return QOpenGLWidget::event(e);
}
//=============================================================================
void
BaseFigureQt::resizeEvent(QResizeEvent* e)
{
    const bool initialResize = firstTime;
    firstTime = false;
    if (!e->spontaneous() && !figureRequestsSize(hfig, e->size())) {
        if (GOWindow* parentWindow = dynamic_cast<GOWindow*>(parentWidget())) {
            if (parentWindow->isVisible()) {
                parentWindow->refitDrawingAreaToRequest();
            }
        }
    }
    const bool interactiveResize
        = !initialResize && (e->spontaneous() || !figureRequestsSize(hfig, e->size()));
    if (interactiveResize) {
        if (!resizing) {
            resizeStartSize = e->oldSize();
            if (initializedGL) {
                // Reuse the last rendered frame instead of grabFramebuffer(), which
                // forces a full GL re-render during Qt's geometry animation. On a GPU-less
                // host (CI) that render runs on Apple's software OpenGL and crashes in
                // glDrawArrays (SIGBUS). Reading the existing framebuffer is safe.
#if defined(__APPLE__)
                resizeSnapshot = grabCurrentFrameWithoutRender();
#else
                resizeSnapshot = grabFramebuffer();
#endif
            }
        }
        resizing = true;
        resizeTimer.stop();
        resizeTimer.start(RESIZE_COALESCE_DELAY_MS, this);
#ifdef _MSC_VER
        if (GOWindow* parentWindow = dynamic_cast<GOWindow*>(parentWidget())) {
            parentWindow->serviceNativeResize(false);
        }
#endif
        return;
    }
    resizing = false;
    QOpenGLWidget::resizeEvent(e);
    resizeTimer.stop();
    resizeSnapshot = QImage();
    resizeStartSize = QSize();
    syncFigureSize(width(), height());
    hfig->setRenderingStateInvalid(true);
    update();
}
//=============================================================================
void
BaseFigureQt::flushPendingResize()
{
    if (!hfig) {
        return;
    }
    resizeTimer.stop();
    resizing = false;
    QResizeEvent resizeEvent(size(), resizeStartSize);
    QOpenGLWidget::resizeEvent(&resizeEvent);
    syncFigureSize(width(), height());
    resizeSnapshot = QImage();
    resizeStartSize = QSize();
    hfig->setRenderingStateInvalid(true);
    update();
}
//=============================================================================
void
BaseFigureQt::timerEvent(QTimerEvent* event)
{
    if (event->timerId() == resizeTimer.timerId()) {
        flushPendingResize();
        return;
    }
    QOpenGLWidget::timerEvent(event);
}
//=============================================================================
QImage
BaseFigureQt::getFrame()
{
    // A capture must show the figure at its settled size. While a resize is
    // coalesced, paintGL only replays the pre-resize snapshot, so a grab now returns a
    // stale frame. The coalescing timer never runs while a script holds the interpreter.
    if (resizing) {
        flushPendingResize();
    }
    if (!hasPrimaryScreen()) {
        return renderFigureToImage(this, hfig);
    }
    if (isVisible()) {
        QImage image = grabFrameReusingLastRender().convertToFormat(QImage::Format_ARGB32);
        bool renderedByQt = false;
        // A uniform framebuffer is either a plain figure or a GL surface that was
        // not rendered (black in a disconnected remote session or at a fractional
        // scale). QWidget::grab reuses that same framebuffer: with child widgets
        // on top it only looks valid because of them, and the figure background
        // comes out black. Render such a figure in software instead.
        if (!imageHasDynamicRange(image) && !hasVisibleChildWidgets(this)) {
            image = captureWidgetToImage(this);
        }
        if (!imageHasDynamicRange(image)) {
            image = renderFigureToImage(this, hfig);
            renderedByQt = true;
        }
        // An empty figure (no axes) is reported at its logical pixel size, but
        // grabFramebuffer/captureWidgetToImage return a device-resolution image on a HiDPI
        // display. Scale that grab back to the logical layout size. A figure with axes keeps
        // the device-resolution grab.
        if (!renderedByQt && hfig && !figureHasAxes(hfig)) {
            int layoutWidth = hfig->getLayoutWidth();
            int layoutHeight = hfig->getLayoutHeight();
            if (layoutWidth > 0 && layoutHeight > 0
                && (image.width() != layoutWidth || image.height() != layoutHeight)) {
                image = image.scaled(
                    layoutWidth, layoutHeight, Qt::IgnoreAspectRatio, Qt::SmoothTransformation);
            }
        }
        renderChildWidgets(this, image);
        return image;
    }
    // Hidden figure (headless getframe / Visible='off'): grabbing a hidden
    // QOpenGLWidget is unreliable. It can return a malformed frame that passes the
    // dynamic-range check and corrupts alpha colors (red@FaceAlpha 0.4 came out teal).
    // Render with RenderQt instead: it composites alpha correctly and also paints
    // annotations and child widgets, so uicontrols are captured.
    return renderFigureToImage(this, hfig);
}
//=============================================================================
QImage
BaseFigureQt::getOpenGLFrame()
{
    // A capture must show the figure at its settled size. While a resize is
    // coalesced, paintGL only replays the pre-resize snapshot, so a grab now returns a
    // stale frame. The coalescing timer never runs while a script holds the interpreter.
    if (resizing) {
        flushPendingResize();
    }
    if (!hasPrimaryScreen()) {
        return getFrame();
    }
    if (glWidth <= 0 || glHeight <= 0) {
        syncFigureSize(width(), height());
    }
    return grabFrameReusingLastRender().convertToFormat(QImage::Format_ARGB32);
}
//=============================================================================
bool
BaseFigureQt::waitForRenderedFrame(int timeoutMs)
{
    QElapsedTimer timer;
    timer.start();

    if (!isVisible()) {
        // A hidden window never receives paint events, so there is no
        // on-screen frame to wait for: waiting would always burn the full
        // timeout (1s per drawnow on figures created with Visible 'off').
        // Deliver pending events once so a queued show() can take effect,
        // then bail out if the window is still hidden.
        QCoreApplication::sendPostedEvents(nullptr, QEvent::UpdateRequest);
        QCoreApplication::processEvents(QEventLoop::ExcludeUserInputEvents, 10);
        if (!isVisible()) {
            return false;
        }
    }

    while ((!isVisible() || width() <= 0 || height() <= 0) && timer.elapsed() <= timeoutMs) {
        QCoreApplication::sendPostedEvents(nullptr, QEvent::UpdateRequest);
        QCoreApplication::processEvents(QEventLoop::ExcludeUserInputEvents, 10);
    }
    if (!isVisible() || width() <= 0 || height() <= 0) {
        return false;
    }
    if (initializedGL && paintSerial > 0 && hfig && !hfig->isRenderingStateInvalid()) {
        return true;
    }
    // A window that already painted but cannot present now (minimized, or
    // placed off every screen) gets no paint event: waiting would burn the
    // full timeout on each drawnow (250 ms per frame, about 4 frames/s for an
    // animation). The repaint stays requested and lands once the window shows
    // again. The first frame keeps waiting: the window may be about to expose.
    if (initializedGL && paintSerial > 0 && !windowCanPresentFrame(this)) {
        return false;
    }

    const quint64 observedPaintSerial = paintSerial;
    update();

    while (timer.elapsed() <= timeoutMs) {
        if (initializedGL && paintSerial != observedPaintSerial) {
            return true;
        }
        if (initializedGL && paintSerial > 0 && hfig && !hfig->isRenderingStateInvalid()) {
            return true;
        }
        QCoreApplication::sendPostedEvents(nullptr, QEvent::UpdateRequest);
        QCoreApplication::processEvents(QEventLoop::ExcludeUserInputEvents, 10);
    }
    return initializedGL && paintSerial > 0;
}
//=============================================================================
void
BaseFigureQt::prerenderFirstFrame()
{
    if (width() <= 0 || height() <= 0) {
        return;
    }
    // Render one frame into the framebuffer object while the native window is hidden.
    // QOpenGLWidget initializes its GL context on the first on-screen paint, so otherwise
    // the window maps with a black framebuffer. No clean frame exists yet, so this forces
    // initializeGL and paintGL; an already-rendered frame is reused instead of re-rendered.
    grabFrameReusingLastRender();
}
//=============================================================================
void
BaseFigureQt::initializeGL()
{
    initializedGL = true;
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LEQUAL);
    glDepthMask(GL_TRUE);
    glDisable(GL_SCISSOR_TEST);
    glEnable(GL_MULTISAMPLE);
    glEnable(GL_LINE_SMOOTH);
    glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    // Pre-clear the framebuffer with the figure background color so the first frame
    // is not the default black.
    GLfloat r = static_cast<GLfloat>(GOFigure::LIGHT_BACKGROUND_GRAY);
    GLfloat g = r;
    GLfloat b = r;
    if (hfig) {
        std::vector<double> color = hfig->findVectorDoubleProperty(GO_COLOR_PROPERTY_NAME_STR);
        if (color.size() >= 3) {
            r = static_cast<GLfloat>(color[0]);
            g = static_cast<GLfloat>(color[1]);
            b = static_cast<GLfloat>(color[2]);
        }
    }
    glClearColor(r, g, b, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    hfig->setRenderingStateInvalid(true);
}
//=============================================================================
void
BaseFigureQt::paintGL()
{
    if (resizing) {
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        if (!resizeSnapshot.isNull()) {
            QPainter painter(this);
            if (hfig->stringCheck(
                    GO_AUTO_RESIZE_CHILDREN_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
                painter.drawImage(rect(), resizeSnapshot);
            } else {
                painter.drawImage(0, 0, resizeSnapshot);
            }
            painter.end();
        }
        ++paintSerial;
        return;
    }
    if (glWidth <= 0 || glHeight <= 0 || !hasExpectedRenderingSize()) {
#if defined(__APPLE__)
        // macOS animates setFrame for a programmatic size and paints mid-animation; a full
        // re-render then crashes Apple software GL in glDrawArrays on a GPU-less host. Skip the
        // heavy render on a transient resize frame once a valid size already exists; glClear is
        // safe and the settled frame or an explicit grab renders normally.
        if (glWidth > 0 && glHeight > 0) {
            glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
            syncFigureSize(width(), height());
            update();
            return;
        }
#endif
        syncFigureSize(width(), height());
    }
    int renderWidth = glWidth;
    int renderHeight = glHeight;
    if (!glRenderer) {
        glRenderer = std::make_unique<RenderOpenGL>(this, 0, 0, renderWidth, renderHeight, L"GL");
    } else {
        glRenderer->resetSurface(0, 0, renderWidth, renderHeight, L"GL");
    }
    hfig->paintMe(*glRenderer);
    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing);
    painter.setRenderHint(QPainter::TextAntialiasing);
    // The widget painter works in logical pixels (Qt applies the device pixel
    // ratio), unlike the GL surface above.
    RenderQt annotationRenderer(&painter, 0, 0, width(), height(), L"GLOverlay");
    hfig->paintAnnotations(annotationRenderer);
    painter.end();
    ++paintSerial;
}
//=============================================================================
void
BaseFigureQt::resizeGL(int w, int h)
{
    if (resizing) {
        return;
    }
    syncFigureSize(w, h);
    hfig->setRenderingStateInvalid(true);
}
//=============================================================================
BaseFigureQt::BaseFigureQt(QWidget* parent, GOFigure* fig) : QOpenGLWidget(parent)
{
    QSurfaceFormat format;
    format.setVersion(2, 1);
    format.setProfile(QSurfaceFormat::CompatibilityProfile);
    format.setDepthBufferSize(24);
    format.setStencilBufferSize(8);
#if defined(Q_OS_MACOS)
    format.setSamples(4);
#else
    format.setSamples(8);
#endif
    // Do not block the compose/flush on vertical sync: drawnow paces animation
    // loops, and a vsync-locked swap adds up to a display period of latency to
    // every synchronously presented frame. Composition through the desktop
    // compositor stays tear-free without it.
    format.setSwapInterval(0);
    setFormat(format);
    setTextureFormat(GL_RGB);
    setAttribute(Qt::WA_OpaquePaintEvent);
    setUpdateBehavior(QOpenGLWidget::NoPartialUpdate);
    hfig = fig;
}
//=============================================================================
BaseFigureQt::~BaseFigureQt()
{
    if (initializedGL) {
        makeCurrent();
        glRenderer.reset();
        doneCurrent();
    } else {
        glRenderer.reset();
    }
}
//=============================================================================
bool
BaseFigureQt::currentScreenResolution(int& w, int& h)
{
    return Nelson::currentScreenResolution(w, h);
}
//=============================================================================
int
BaseFigureQt::getCurrentScreenHeight()
{
    int w = 0;
    int h = 0;
    currentScreenResolution(w, h);
    return h;
}
//=============================================================================
int
BaseFigureQt::getCurrentScreenWidth()
{
    int w = 0;
    int h = 0;
    currentScreenResolution(w, h);
    return w;
}
//=============================================================================
// True when the window system can paint the top-level window of the widget:
// not minimized, exposed, and its frame overlaps at least one screen.
static bool
windowCanPresentFrame(const QWidget* widget)
{
    const QWidget* topLevel = widget ? widget->window() : nullptr;
    if (!topLevel) {
        return false;
    }
    if (topLevel->isMinimized()) {
        return false;
    }
    const QWindow* nativeWindow = topLevel->windowHandle();
    if (nativeWindow && !nativeWindow->isExposed()) {
        return false;
    }
    const QRect frame = topLevel->frameGeometry();
    const QList<QScreen*> screens = QGuiApplication::screens();
    for (const QScreen* screen : screens) {
        if (screen && screen->geometry().intersects(frame)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
