//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "QtLineNumber.h"
#include <QtCore/QRectF>
#include <QtGui/QPainter>
#include <QtGui/QTextBlock>
#include <QtGui/QTextLayout>
#include <QtWidgets/QScrollBar>
//=============================================================================
QtLineNumber::QtLineNumber(QtTextEdit* editor) : QWidget(), tEditor(editor)
{
    setFixedWidth(fontMetrics().width(QLatin1String("0000") + 5));
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
        p.drawText(width() - fm.width(txt), qRound(position.y()) - contentsY + ascent, txt);
    }
}
//=============================================================================
