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
#include <QtCore/QFileInfo>
#include <QtGui/QPainter>
#include <QtGui/QTextBlock>
#include <QtGui/QTextLayout>
#include <QtGui/QMouseEvent>
#include <QtWidgets/QScrollBar>
#include <QtCore/QFile>
#include <QtCore/QTextStream>
#include <QtCore/QRegularExpression>
#include "QStringConverter.hpp"
//=============================================================================
static const int BREAKPOINT_AREA_WIDTH = 16;
static const int BREAKPOINT_MARKER_RADIUS = 5;
//=============================================================================
static std::string
extractFunctionNameAtLine(const QString& filename, int lineNumber);
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
    // Ensure we store absolute path for consistent breakpoint matching
    QFileInfo fileInfo(filename);
    currentFilename = fileInfo.absoluteFilePath();
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
            std::wstring wfilename = Nelson::QStringTowstring(currentFilename);

            // Adjust line number to nearest executable statement using Evaluator method
            size_t adjustedLine = static_cast<size_t>(line);
            std::wstring errorMessage;
            if (!nlsEvaluator->adjustBreakpointLine(
                    wfilename, static_cast<size_t>(line), adjustedLine, errorMessage)) {
                // If adjustment fails, use the original line (might be a script with no parsing)
                adjustedLine = static_cast<size_t>(line);
            }

            Breakpoint bp;
            bp.filename = wfilename;
            bp.line = adjustedLine;
            bp.enabled = true;
            bp.stepMode = false;
            // Try to extract function name from the file
            bp.functionName = extractFunctionNameAtLine(currentFilename, line);
            nlsEvaluator->addBreakpoint(bp);

            // Update UI to show breakpoint at adjusted line if different
            if (adjustedLine != static_cast<size_t>(line)) {
                breakpointLines.insert(static_cast<int>(adjustedLine));
                emit breakpointToggled(static_cast<int>(adjustedLine), true);
            } else {
                breakpointLines.insert(line);
                emit breakpointToggled(line, true);
            }
        } else {
            breakpointLines.insert(line);
            emit breakpointToggled(line, true);
        }
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
// Helper function to extract function name from file at a given line
std::string
extractFunctionNameAtLine(const QString& filename, int lineNumber)
{
    QFile file(filename);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) {
        return "";
    }

    QTextStream in(&file);
    QStringList lines;
    while (!in.atEnd()) {
        lines.append(in.readLine());
    }
    file.close();

    if (lineNumber < 1 || lineNumber > lines.size()) {
        return "";
    }

    // Look backwards from the target line to find the nearest function definition
    QRegularExpression funcPattern("^\\s*function\\s+(?:\\[.*?\\]\\s*=\\s*|\\w+\\s*=\\s*)?(\\w+)");

    for (int i = lineNumber - 1; i >= 0; i--) {
        QRegularExpressionMatch match = funcPattern.match(lines[i]);
        if (match.hasMatch()) {
            QString funcName = match.captured(1);
            return funcName.toStdString();
        }
    }

    return "";
}
//=============================================================================
