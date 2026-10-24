//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FigureFrameSample.hpp"
#include <QtCore/QElapsedTimer>
#include <QtCore/QFile>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>
#include <QtCore/QPointer>
#include <QtCore/QTimer>
#include <QtGui/QScreen>
#include <QtOpenGLWidgets/QOpenGLWidget>
#include <QtWidgets/QApplication>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QLayout>
#include <QtWidgets/QMainWindow>
//=============================================================================
namespace {
class FigureFrameProbe : public QObject
{
    QString title;
    QJsonObject request;
    QTimer finder;
    QElapsedTimer clock;
    QPointer<QMainWindow> window;
    QPointer<QOpenGLWidget> canvas;
    FigureFrameSample sample;
    bool resized = false;
    bool painted = false;

    bool
    eventFilter(QObject* object, QEvent* event) override
    {
        if (object == canvas && event->type() == QEvent::Paint) {
            painted = true;
        }
        return QObject::eventFilter(object, event);
    }

    void
    finish(const QString& error = {})
    {
        finder.stop();
        QJsonObject report { { "error", error }, { "frames", sample.frames() },
            { "seconds", sample.duration() / 1.0e9 }, { "fps", sample.fps() },
            { "warmupFrames", request["warmupFrames"] },
            { "requestedFrames", request["measuredFrames"] } };
        if (canvas && window) {
            const auto* dock = window->findChild<QDockWidget*>("figurePropertiesDock");
            report["properties"] = dock && dock->isVisible();
            report["canvasWidth"] = canvas->width();
            report["canvasHeight"] = canvas->height();
            report["devicePixelRatio"] = canvas->devicePixelRatioF();
            report["windowWidth"] = window->width();
            report["windowHeight"] = window->height();
            if (auto* screen = canvas->screen()) {
                report["screen"] = screen->name();
                report["refreshRate"] = screen->refreshRate();
            }
            QObject::disconnect(canvas, nullptr, this, nullptr);
            canvas->removeEventFilter(this);
        }
        QFile output(request["path"].toString());
        if (output.open(QIODevice::WriteOnly)) {
            output.write(QJsonDocument(report).toJson());
        }
        deleteLater();
    }

    void
    attach()
    {
        if (!canvas) {
            for (auto* widget : QApplication::allWidgets()) {
                auto* candidate = qobject_cast<QMainWindow*>(widget);
                if (candidate && candidate->windowTitle() == title && candidate->isVisible()) {
                    window = candidate;
                    canvas = qobject_cast<QOpenGLWidget*>(window->centralWidget());
                    if (canvas) {
                        clock.restart();
                        return;
                    }
                }
            }
            if (clock.elapsed() > 10000) {
                finish("Visible OpenGL figure not found.");
            }
            return;
        }
        if (clock.elapsed() < 500) {
            return;
        }
        if (!resized) {
            window->layout()->activate();
            window->resize(window->size()
                + QSize(request["canvasWidth"].toInt() - canvas->width(),
                    request["canvasHeight"].toInt() - canvas->height()));
            if (auto* screen = canvas->screen()) {
                window->move(screen->availableGeometry().center() - window->rect().center());
            }
            window->raise();
            resized = true;
            clock.restart();
            return;
        }
        if (canvas->width() != request["canvasWidth"].toInt()
            || canvas->height() != request["canvasHeight"].toInt()) {
            finish("Drawing area did not reach the requested size.");
            return;
        }
        finder.stop();
        clock.restart();
        canvas->installEventFilter(this);
        QObject::connect(canvas, &QOpenGLWidget::frameSwapped, this, [this]() {
            if (!painted) {
                return;
            }
            painted = false;
            if (sample.frame(clock.nsecsElapsed())) {
                finish();
            }
        });
    }

public:
    FigureFrameProbe(const QString& name, const QJsonObject& options)
        : QObject(QApplication::instance())
        , title(name)
        , request(options)
        , sample(options["warmupFrames"].toInt(), options["measuredFrames"].toInt())
    {
        finder.setInterval(10);
        QObject::connect(&finder, &QTimer::timeout, this, [this]() { attach(); });
        clock.start();
        finder.start();
    }
};
//=============================================================================
} // namespace
//=============================================================================
int
armFigureFrames(const QString& title, const QJsonObject& request)
{
    if (request["warmupFrames"].toInt() < 1 || request["measuredFrames"].toInt() < 1
        || request["canvasWidth"].toInt() < 1 || request["canvasHeight"].toInt() < 1
        || request["path"].toString().isEmpty()) {
        return -4;
    }
    new FigureFrameProbe(title, request);
    return 1;
}
//=============================================================================
