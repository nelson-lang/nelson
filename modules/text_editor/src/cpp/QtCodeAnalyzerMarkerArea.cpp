//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtCodeAnalyzerMarkerArea.h"
#include <QtGui/QAbstractTextDocumentLayout>
#include <QtGui/QMouseEvent>
#include <QtGui/QPainter>
#include <QtGui/QTextBlock>
#include <QtGui/QTextLayout>
#include <QtWidgets/QScrollBar>
//=============================================================================
static const int CODE_ANALYZER_MARKER_WIDTH = 12;
//=============================================================================
QtCodeAnalyzerMarkerArea::QtCodeAnalyzerMarkerArea(QtTextEdit* textEditor)
    : QWidget(), tEditor(textEditor)
{
    setFixedWidth(CODE_ANALYZER_MARKER_WIDTH);
    setCursor(Qt::PointingHandCursor);
    setToolTip(tr("Code issues"));
    connect(
        tEditor->document()->documentLayout(), SIGNAL(update(const QRectF&)), this, SLOT(update()));
    connect(tEditor->verticalScrollBar(), SIGNAL(valueChanged(int)), this, SLOT(update()));
}
//=============================================================================
void
QtCodeAnalyzerMarkerArea::setDiagnostics(
    const std::vector<Nelson::CodeAnalyzerDiagnostic>& newDiagnostics)
{
    diagnostics = newDiagnostics;
    update();
}
//=============================================================================
std::string
QtCodeAnalyzerMarkerArea::severityForLine(int line) const
{
    std::string severity;
    for (const auto& diagnostic : diagnostics) {
        if (diagnostic.suppressed || diagnostic.lineStart != line) {
            continue;
        }
        if (diagnostic.severity == "error") {
            return "error";
        }
        if (diagnostic.severity == "warning") {
            severity = "warning";
        } else if (severity.empty()) {
            severity = diagnostic.severity;
        }
    }
    return severity;
}
//=============================================================================
void
QtCodeAnalyzerMarkerArea::paintEvent(QPaintEvent*)
{
    QPainter p(this);
    p.fillRect(rect(), palette().color(QPalette::Window).darker(103));
    if (tEditor == nullptr) {
        return;
    }
    int contentsY = tEditor->verticalScrollBar()->value();
    qreal pageBottom = contentsY + tEditor->viewport()->height();
    int lineNumber = 1;
    for (QTextBlock block = tEditor->document()->begin(); block.isValid();
        block = block.next(), ++lineNumber) {
        QTextLayout* layout = block.layout();
        if (layout == nullptr) {
            continue;
        }
        QRectF boundingRect = layout->boundingRect();
        QPointF position = layout->position();
        if (position.y() + boundingRect.height() < contentsY) {
            continue;
        }
        if (position.y() > pageBottom) {
            break;
        }
        std::string severity = severityForLine(lineNumber);
        if (severity.empty()) {
            continue;
        }
        QColor color = QColor(74, 144, 226);
        if (severity == "error") {
            color = QColor(206, 51, 51);
        } else if (severity == "warning") {
            color = QColor(222, 143, 35);
        }
        p.setRenderHint(QPainter::Antialiasing, true);
        p.setBrush(color);
        p.setPen(color.darker(120));
        int y = qRound(position.y()) - contentsY + qRound(boundingRect.height() / 2.0);
        p.drawEllipse(QPoint(width() / 2, y), 4, 4);
    }
}
//=============================================================================
int
QtCodeAnalyzerMarkerArea::lineNumberAtPosition(int y) const
{
    int contentsY = tEditor->verticalScrollBar()->value();
    int lineNumber = 1;
    for (QTextBlock block = tEditor->document()->begin(); block.isValid();
        block = block.next(), ++lineNumber) {
        QTextLayout* layout = block.layout();
        if (layout == nullptr) {
            continue;
        }
        QRectF boundingRect = layout->boundingRect();
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
QtCodeAnalyzerMarkerArea::mousePressEvent(QMouseEvent* event)
{
    if (event->button() == Qt::LeftButton) {
        int line = lineNumberAtPosition(event->pos().y());
        if (line > 0 && !severityForLine(line).empty()) {
            emit issueMarkerClicked(line);
        }
    }
    QWidget::mousePressEvent(event);
}
//=============================================================================
