//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtLineNumber.h"
#include <QtCore/QRectF>
#include <QtGui/QPainter>
#include <QtGui/QTextBlock>
#include <QtGui/QTextLayout>
#include <QtWidgets/QScrollBar>
//=============================================================================
QtLineNumber::QtLineNumber(QtTextEdit* textEditor) : QWidget(), tEditor(textEditor)
{
    int width;
    width = QFontMetrics(font()).horizontalAdvance(QLatin1String("0000")) + 5;
    setFixedWidth(width);
    connect((QTextEdit*)tEditor->document()->documentLayout(), SIGNAL(update(const QRectF&)), this,
        SLOT(update()));
    connect(
        (QTextEdit*)tEditor->verticalScrollBar(), SIGNAL(valueChanged(int)), this, SLOT(update()));
}
//=============================================================================
void
QtLineNumber::paintEvent(QPaintEvent* paintEvent)
{
    int contentsY = tEditor->verticalScrollBar()->value();
    qreal pageBottom = contentsY + tEditor->viewport()->height();
    int lineNumber = 1;
    setFont(tEditor->font());
    const QFontMetrics fm = fontMetrics();
    const int ascent = fontMetrics().ascent() + 1; // height = ascent + descent + 1
    QPainter p(this);
    for (QTextBlock block = tEditor->document()->begin(); block.isValid();
         block = block.next(), ++lineNumber) {
        QTextLayout* layout = block.layout();
        const QRectF boundingRect = layout->boundingRect();
        QPointF position = layout->position();
        if (position.y() + boundingRect.height() < contentsY) {
            continue;
        }
        if (position.y() > pageBottom) {
            break;
        }
        const QString txt = QString::number(lineNumber);
        p.drawText(
            width() - fm.horizontalAdvance(txt), qRound(position.y()) - contentsY + ascent, txt);
    }
}
//=============================================================================
