//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QRectF>
#include <QtGui/QFont>
#include <QtGui/QGuiApplication>
#include <QtGui/QPainter>
#include <QtGui/QPalette>
#include <QtGui/QPixmap>
#include "DesktopActionBadge.hpp"
//=============================================================================
static QIcon
badgedIcon(const QIcon& icon, int count);
//=============================================================================
DesktopActionBadge::DesktopActionBadge(QAction* action)
    : QObject(action), action(action), baseIcon(action->icon()), baseText(action->text())
{
}
//=============================================================================
void
DesktopActionBadge::setBaseIcon(const QIcon& icon)
{
    baseIcon = icon;
    apply();
}
//=============================================================================
void
DesktopActionBadge::setCount(int newCount)
{
    if (newCount == count) {
        return;
    }
    count = newCount;
    apply();
}
//=============================================================================
void
DesktopActionBadge::apply()
{
    action->setIcon(count > 0 ? badgedIcon(baseIcon, count) : baseIcon);
    action->setText(count > 0 ? QString("%1 (%2)").arg(baseText).arg(count) : baseText);
}
//=============================================================================
static QIcon
badgedIcon(const QIcon& icon, int count)
{
    // A filled accent disc at the top right of each icon size, with the count
    // ("9+" beyond nine) in the accent text color.
    const QPalette palette = QGuiApplication::palette();
    const QString label = count > 9 ? QString("9+") : QString::number(count);
    QIcon result;
    for (int side : { 16, 20, 24, 32, 48 }) {
        QPixmap pixmap = icon.pixmap(QSize(side, side));
        if (pixmap.isNull()) {
            continue;
        }
        const qreal diameter = side * 0.62;
        const qreal width = label.size() > 1 ? diameter * 1.35 : diameter;
        const QRectF badge(side - width, 0, width, diameter);
        QPainter painter(&pixmap);
        painter.setRenderHint(QPainter::Antialiasing);
        painter.setPen(Qt::NoPen);
        painter.setBrush(palette.color(QPalette::Highlight));
        painter.drawRoundedRect(badge, diameter / 2, diameter / 2);
        QFont font = painter.font();
        font.setBold(true);
        font.setPixelSize(qMax(7, static_cast<int>(diameter * 0.72)));
        painter.setFont(font);
        painter.setPen(palette.color(QPalette::HighlightedText));
        painter.drawText(badge, Qt::AlignCenter, label);
        painter.end();
        result.addPixmap(pixmap);
    }
    return result;
}
//=============================================================================
