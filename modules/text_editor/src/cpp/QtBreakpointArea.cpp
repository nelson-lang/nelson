//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtBreakpointArea.h"
#include <QtCore/QRectF>
#include <QtGui/QPainter>
#include <QtGui/QTextBlock>
#include <QtGui/QTextLayout>
#include <QtGui/QMouseEvent>
#include <QtWidgets/QScrollBar>
#include "QStringConverter.hpp"
//=============================================================================
static const int BREAKPOINT_AREA_WIDTH = 16;
static const int BREAKPOINT_MARKER_RADIUS = 5;
//=============================================================================
QtBreakpointArea::QtBreakpointArea(QtTextEdit* textEditor, Evaluator* eval)
    : QWidget(), tEditor(textEditor), nlsEvaluator(eval)
{
    setFixedWidth(BREAKPOINT_AREA_WIDTH);
    setCursor(Qt::PointingHandCursor);
    setToolTip(tr("Click to toggle breakpoint"));

    connect((QTextEdit*)tEditor->document()->documentLayout(), SIGNAL(update(const QRectF&)), this,
        SLOT(update()));
    connect(
        (QTextEdit*)tEditor->verticalScrollBar(), SIGNAL(valueChanged(int)), this, SLOT(update()));
}
//=============================================================================
void
QtBreakpointArea::setFilename(const QString& filename)
{
    currentFilename = filename;
    refreshBreakpoints();
}
//=============================================================================
QString
QtBreakpointArea::getFilename() const
{
    return currentFilename;
}
//=============================================================================
void
QtBreakpointArea::refreshBreakpoints()
{
    breakpointLines.clear();
    if (nlsEvaluator && !currentFilename.isEmpty()) {
        std::wstring wfilename = Nelson::QStringTowstring(currentFilename);
        std::vector<size_t> lines = nlsEvaluator->getBreakpointLines(wfilename);
        for (size_t line : lines) {
            breakpointLines.insert(static_cast<int>(line));
        }
    }
    update();
}
//=============================================================================
void
QtBreakpointArea::toggleBreakpoint(int line)
{
    if (line <= 0 || currentFilename.isEmpty()) {
        return;
    }

    std::wstring wfilename = Nelson::QStringTowstring(currentFilename);

    if (breakpointLines.contains(line)) {
        // Remove breakpoint
        if (nlsEvaluator) {
            nlsEvaluator->removeBreakpoint(wfilename, static_cast<size_t>(line));
        }
        breakpointLines.remove(line);
        emit breakpointToggled(line, false);
    } else {
        // Add breakpoint
        if (nlsEvaluator) {
            Breakpoint bp;
            bp.filename = wfilename;
            bp.line = static_cast<size_t>(line);
            bp.enabled = true;
            bp.stepMode = false;
            nlsEvaluator->addBreakpoint(bp);
        }
        breakpointLines.insert(line);
        emit breakpointToggled(line, true);
    }
    update();
}
//=============================================================================
bool
QtBreakpointArea::hasBreakpoint(int line) const
{
    return breakpointLines.contains(line);
}
//=============================================================================
QSet<int>
QtBreakpointArea::getBreakpointLines() const
{
    return breakpointLines;
}
//=============================================================================
void
QtBreakpointArea::setExecutionLine(int line)
{
    currentExecutionLine = line;
    update();
}
//=============================================================================
void
QtBreakpointArea::clearExecutionLine()
{
    currentExecutionLine = -1;
    update();
}
//=============================================================================
int
QtBreakpointArea::getExecutionLine() const
{
    return currentExecutionLine;
}
//=============================================================================
int
QtBreakpointArea::lineNumberAtPosition(int y) const
{
    int contentsY = tEditor->verticalScrollBar()->value();
    int lineNumber = 1;

    for (QTextBlock block = tEditor->document()->begin(); block.isValid();
         block = block.next(), ++lineNumber) {
        QTextLayout* layout = block.layout();
        if (layout == nullptr) {
            continue;
        }
        const QRectF boundingRect = layout->boundingRect();
        QPointF position = layout->position();

        qreal blockTop = position.y() - contentsY;
        qreal blockBottom = blockTop + boundingRect.height();

        if (y >= blockTop && y < blockBottom) {
            return lineNumber;
        }
    }
    return -1;
}
//=============================================================================
void
QtBreakpointArea::paintEvent(QPaintEvent* paintEvent)
{
    QPainter p(this);
    p.fillRect(rect(), palette().color(QPalette::Window).darker(105));

    if (tEditor == nullptr) {
        return;
    }

    int contentsY = tEditor->verticalScrollBar()->value();
    qreal pageBottom = contentsY + tEditor->viewport()->height();
    int lineNumber = 1;

    setFont(tEditor->font());
    const int lineHeight = fontMetrics().height();

    for (QTextBlock block = tEditor->document()->begin(); block.isValid();
         block = block.next(), ++lineNumber) {
        QTextLayout* layout = block.layout();
        if (layout == nullptr) {
            continue;
        }
        const QRectF boundingRect = layout->boundingRect();
        QPointF position = layout->position();

        if (position.y() + boundingRect.height() < contentsY) {
            continue;
        }
        if (position.y() > pageBottom) {
            break;
        }

        int yPos = qRound(position.y()) - contentsY;
        int centerX = width() / 2;
        int centerY = yPos + lineHeight / 2;

        // Draw execution arrow (yellow, takes priority)
        if (lineNumber == currentExecutionLine) {
            p.setRenderHint(QPainter::Antialiasing, true);
            QPolygon arrow;
            arrow << QPoint(2, centerY - 5) << QPoint(width() - 2, centerY)
                  << QPoint(2, centerY + 5);
            p.setBrush(QBrush(QColor(255, 220, 0))); // Gold/yellow
            p.setPen(QColor(200, 160, 0));
            p.drawPolygon(arrow);
        }
        // Draw breakpoint circle (red, behind arrow if both present)
        else if (breakpointLines.contains(lineNumber)) {
            p.setRenderHint(QPainter::Antialiasing, true);
            p.setBrush(QBrush(Qt::red));
            p.setPen(Qt::darkRed);
            p.drawEllipse(
                QPoint(centerX, centerY), BREAKPOINT_MARKER_RADIUS, BREAKPOINT_MARKER_RADIUS);
        }
    }
}
//=============================================================================
void
QtBreakpointArea::mousePressEvent(QMouseEvent* event)
{
    if (event->button() == Qt::LeftButton) {
        int line = lineNumberAtPosition(event->pos().y());
        if (line > 0) {
            toggleBreakpoint(line);
        }
    }
    QWidget::mousePressEvent(event);
}
//=============================================================================
