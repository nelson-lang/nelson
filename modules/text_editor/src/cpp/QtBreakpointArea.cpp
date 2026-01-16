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
bool
QtBreakpointArea::isValidBreakpointLine(int lineNumber)
{
    // Check if line is valid for breakpoint placement (not empty, not commented)
    QTextDocument* doc = tEditor->document();
    if (!doc) {
        return false;
    }

    QTextBlock block = doc->findBlockByLineNumber(lineNumber - 1);
    if (!block.isValid()) {
        return false;
    }

    QString text = block.text().trimmed();

    // Empty lines cannot have breakpoints
    if (text.isEmpty()) {
        return false;
    }

    // Check if line is commented (starts with %)
    if (text.startsWith('%')) {
        return false;
    }

    // Check for inline comments that make the whole line a comment
    int commentPos = text.indexOf('%');
    if (commentPos != -1 && commentPos == 0) {
        return false;
    }

    return true;
}
//=============================================================================
int
QtBreakpointArea::findNextValidBreakpointLine(int lineNumber)
{
    // Search forward for the next valid breakpoint line
    // Use evaluator to find the proper executable line (handles multi-line statements)
    QTextDocument* doc = tEditor->document();
    if (!doc) {
        return -1;
    }

    int totalLines = doc->blockCount();
    std::wstring wfilename = Nelson::QStringTowstring(currentFilename);

    for (int i = lineNumber; i <= totalLines; ++i) {
        if (isValidBreakpointLine(i)) {
            // Try to adjust the line to get the actual executable line
            if (nlsEvaluator) {
                size_t adjustedLine = static_cast<size_t>(i);
                std::wstring errorMessage;
                if (nlsEvaluator->adjustBreakpointLine(
                        wfilename, static_cast<size_t>(i), adjustedLine, errorMessage)) {
                    // Return the adjusted line number
                    return static_cast<int>(adjustedLine);
                }
            }
            // If no evaluator or adjustment fails, return the line as-is
            return i;
        }
    }

    return -1; // No valid line found
}
//=============================================================================
void
QtBreakpointArea::toggleBreakpoint(int line)
{
    if (line <= 0 || currentFilename.isEmpty()) {
        return;
    }

    std::wstring wfilename = Nelson::QStringTowstring(currentFilename);

    // First, try to use the evaluator to adjust the line (handles multi-line statements)
    if (nlsEvaluator) {
        size_t adjustedLine = static_cast<size_t>(line);
        std::wstring errorMessage;
        if (nlsEvaluator->adjustBreakpointLine(
                wfilename, static_cast<size_t>(line), adjustedLine, errorMessage)) {
            // Successfully adjusted - use this line
            line = static_cast<int>(adjustedLine);
        } else {
            // Adjustment failed - check if line is at least textually valid
            if (!isValidBreakpointLine(line)) {
                // Try to find the next valid line
                line = findNextValidBreakpointLine(line);
                if (line <= 0) {
                    // No valid line found
                    return;
                }
            }
        }
    } else {
        // No evaluator - fall back to text-based validation
        if (!isValidBreakpointLine(line)) {
            line = findNextValidBreakpointLine(line);
            if (line <= 0) {
                return;
            }
        }
    }

    if (breakpointLines.contains(line)) {
        // Remove breakpoint
        if (nlsEvaluator) {
            nlsEvaluator->removeBreakpoint(wfilename, static_cast<size_t>(line));
        }
        breakpointLines.remove(line);
        emit breakpointToggled(line, false);
    } else {
        // Add breakpoint - line is already adjusted above
        if (nlsEvaluator) {
            Breakpoint bp;
            bp.filename = wfilename;
            bp.line = static_cast<size_t>(line);
            bp.enabled = true;
            bp.stepMode = false;
            // Try to extract function name from the file
            bp.functionName = extractFunctionNameAtLine(currentFilename, line);
            nlsEvaluator->addBreakpoint(bp);

            breakpointLines.insert(line);
            emit breakpointToggled(line, true);
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
