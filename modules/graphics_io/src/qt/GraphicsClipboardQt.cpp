//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GraphicsClipboard.hpp"
#include "GOFigureHost.hpp"
#include <QtCore/QMetaObject>
#include <QtCore/QThread>
#include <QtGui/QClipboard>
#include <QtGui/QImage>
#include <QtWidgets/QApplication>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    bool
    copyToDesktopClipboard(GOFigureHost* host)
    {
        auto* application = qobject_cast<QApplication*>(QApplication::instance());
        if (!application || !host) {
            return false;
        }
        bool copied = false;
        auto writeClipboard = [&]() {
            std::vector<unsigned char> rgba;
            int width = 0;
            int height = 0;
            if (!host->grabHostFrameRGBA(false, rgba, width, height) || width <= 0 || height <= 0) {
                return;
            }
            const QImage image
                = QImage(rgba.data(), width, height, width * 4, QImage::Format_RGBA8888).copy();
            QClipboard* clipboard = QApplication::clipboard();
            if (clipboard) {
                clipboard->setImage(image);
                copied = true;
            }
        };
        if (QThread::currentThread() == application->thread()) {
            writeClipboard();
            return copied;
        }
        return QMetaObject::invokeMethod(application, writeClipboard, Qt::BlockingQueuedConnection)
            && copied;
    }
    //=============================================================================
    struct ClipboardRegistration
    {
        ClipboardRegistration() { setGraphicsClipboardProvider(&copyToDesktopClipboard); }
        ~ClipboardRegistration() { setGraphicsClipboardProvider(nullptr); }
    } registration;
    //=============================================================================
} // namespace
//=============================================================================
} // namespace Nelson
//=============================================================================
