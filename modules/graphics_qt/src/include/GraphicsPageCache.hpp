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
#include <functional>
#include <QtGui/QImage>
#include <QtGui/QPainter>
#include <QtGui/QPaintEvent>
#include <QtGui/QRegion>
#include <QtWidgets/QWidget>
//=============================================================================
namespace Nelson {
//=============================================================================
// Graphics of a uitab/uipanel page: a full-page paint renders them again, a partial one
// (a uicontrol repainting itself on hover, focus or popup) reuses the last image.
class GraphicsPageCache
{
public:
    void
    paint(QWidget* page, QPaintEvent* event, const std::function<void(QPainter&)>& render)
    {
        const qreal ratio = page->devicePixelRatioF();
        const QSize pixels = page->size() * ratio;
        if (pixels.isEmpty()) {
            return;
        }
        // A page partly outside its parent only gets clipped events: compare with its visible part.
        const QRegion visible = page->visibleRegion();
        const bool fullPage = event == nullptr || event->rect().contains(page->rect())
            || (!visible.isEmpty() && (visible - event->region()).isEmpty());
        if (fullPage || image.size() != pixels || image.devicePixelRatio() != ratio) {
            image = QImage(pixels, QImage::Format_ARGB32_Premultiplied);
            image.setDevicePixelRatio(ratio);
            image.fill(Qt::transparent);
            QPainter imagePainter(&image);
            imagePainter.setRenderHint(QPainter::Antialiasing);
            imagePainter.setRenderHint(QPainter::TextAntialiasing);
            render(imagePainter);
        }
        QPainter painter(page);
        if (event) {
            painter.setClipRegion(event->region());
        }
        painter.drawImage(QPointF(0, 0), image);
    }

private:
    QImage image;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
