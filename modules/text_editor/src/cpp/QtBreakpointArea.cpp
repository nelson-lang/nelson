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
#include <QtGui/QContextMenuEvent>
#include <QtGui/QHelpEvent>
#include <QtWidgets/QScrollBar>
#include <QtWidgets/QMenu>
#include <QtWidgets/QInputDialog>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QToolTip>
#include "QStringConverter.hpp"
#include "QtTranslation.hpp"
#include "EditorBreakpoints.hpp"
//=============================================================================
static const int BREAKPOINT_AREA_WIDTH = 16;
static const int BREAKPOINT_MARKER_RADIUS = 5;
//=============================================================================
QtBreakpointArea::QtBreakpointArea(QtTextEdit* textEditor, Evaluator* eval)
    : QWidget(), tEditor(textEditor), nlsEvaluator(eval)
{
    setFixedWidth(BREAKPOINT_AREA_WIDTH);
    setCursor(Qt::PointingHandCursor);
    setMouseTracking(true);

    connect((QTextEdit*)tEditor->document()->documentLayout(), SIGNAL(update(const QRectF&)), this,
        SLOT(update()));
    connect(
        (QTextEdit*)tEditor->verticalScrollBar(), SIGNAL(valueChanged(int)), this, SLOT(update()));
}
//=============================================================================
QColor
QtBreakpointArea::activeColor()
{
    return QColor(220, 30, 30);
}
//=============================================================================
QColor
QtBreakpointArea::disabledColor()
{
    return QColor(190, 190, 190);
}
//=============================================================================
QColor
QtBreakpointArea::invalidColor()
{
    return QColor(110, 110, 110);
}
//=============================================================================
QColor
QtBreakpointArea::conditionalColor()
{
    return QColor(255, 140, 0);
}
//=============================================================================
QColor
QtBreakpointArea::executionColor()
{
    return QColor(0, 160, 0);
}
//=============================================================================
void
QtBreakpointArea::setFilename(const QString& filename)
{
    // absolute path, so breakpoints match however the file was opened
    QFileInfo fileInfo(filename);
    currentFilename = fileInfo.absoluteFilePath();
    breakpoints.clear();
    pendingRemovals.clear();
    refreshBreakpoints();
}
//=============================================================================
QString
QtBreakpointArea::getFilename() const
{
    return currentFilename;
}
//=============================================================================
QTextBlock
QtBreakpointArea::blockForLine(int line) const
{
    QTextDocument* doc = tEditor->document();
    if (doc == nullptr || line <= 0) {
        return QTextBlock();
    }
    return doc->findBlockByLineNumber(line - 1);
}
//=============================================================================
int
QtBreakpointArea::indexOfLine(int line) const
{
    for (int k = 0; k < breakpoints.size(); ++k) {
        if (breakpoints[k].currentLine() == line) {
            return k;
        }
    }
    return -1;
}
//=============================================================================
void
QtBreakpointArea::refreshBreakpoints()
{
    if (nlsEvaluator == nullptr || currentFilename.isEmpty()) {
        update();
        return;
    }
    std::vector<Breakpoint> stored
        = nlsEvaluator->getUserBreakpoints(Nelson::QStringTowstring(currentFilename));
    if (!documentModified) {
        // the document matches the file on disk: the evaluator is the truth
        breakpoints.clear();
        pendingRemovals.clear();
        for (const Breakpoint& bp : stored) {
            EditorBreakpoint item;
            item.block = blockForLine(static_cast<int>(bp.line));
            item.lastKnownLine = static_cast<int>(bp.line);
            item.enabled = bp.enabled;
            item.condition = Nelson::wstringToQString(bp.condition);
            if (item.block.isValid()) {
                breakpoints.push_back(item);
            }
        }
        update();
        return;
    }
    // edits in progress: keep the anchored markers, follow changes made from the console or panel
    QList<EditorBreakpoint> kept;
    for (const EditorBreakpoint& item : breakpoints) {
        if (!item.block.isValid()) {
            continue;
        }
        if (item.lastKnownLine == 0) {
            kept.push_back(item);
            continue;
        }
        bool found = false;
        for (const Breakpoint& bp : stored) {
            if (static_cast<int>(bp.line) == item.lastKnownLine) {
                EditorBreakpoint updated = item;
                updated.enabled = bp.enabled;
                updated.condition = Nelson::wstringToQString(bp.condition);
                kept.push_back(updated);
                found = true;
                break;
            }
        }
        // not found: removed by dbclear or from the panel, the marker goes away
        (void)found;
    }
    for (const Breakpoint& bp : stored) {
        bool known = false;
        for (const EditorBreakpoint& item : kept) {
            if (item.lastKnownLine == static_cast<int>(bp.line)) {
                known = true;
                break;
            }
        }
        if (!known && !pendingRemovals.contains(static_cast<int>(bp.line))) {
            EditorBreakpoint item;
            item.block = blockForLine(static_cast<int>(bp.line));
            item.lastKnownLine = static_cast<int>(bp.line);
            item.enabled = bp.enabled;
            item.condition = Nelson::wstringToQString(bp.condition);
            if (item.block.isValid()) {
                kept.push_back(item);
            }
        }
    }
    breakpoints = kept;
    update();
}
//=============================================================================
bool
QtBreakpointArea::isValidBreakpointLine(int lineNumber)
{
    QTextDocument* doc = tEditor->document();
    if (!doc) {
        return false;
    }
    QTextBlock block = doc->findBlockByLineNumber(lineNumber - 1);
    if (!block.isValid()) {
        return false;
    }
    QString text = block.text().trimmed();
    if (text.isEmpty()) {
        return false;
    }
    if (isCommentOnlyLine(lineNumber)) {
        return false;
    }
    return true;
}
//=============================================================================
int
QtBreakpointArea::findNextValidBreakpointLine(int lineNumber)
{
    QTextDocument* doc = tEditor->document();
    if (!doc) {
        return -1;
    }
    int totalLines = doc->blockCount();
    std::wstring wfilename = Nelson::QStringTowstring(currentFilename);
    for (int i = lineNumber; i <= totalLines; ++i) {
        if (isValidBreakpointLine(i)) {
            if (nlsEvaluator != nullptr && !documentModified) {
                size_t adjustedLine = static_cast<size_t>(i);
                std::wstring errorMessage;
                if (nlsEvaluator->adjustBreakpointLine(
                        wfilename, static_cast<size_t>(i), adjustedLine, errorMessage)) {
                    return static_cast<int>(adjustedLine);
                }
            }
            return i;
        }
    }
    return -1;
}
//=============================================================================
void
QtBreakpointArea::addBreakpointAt(int line)
{
    std::wstring wfilename = Nelson::QStringTowstring(currentFilename);
    if (nlsEvaluator != nullptr && !documentModified) {
        // the saved file is the truth: the evaluator adjusts the line
        std::wstring errorMessage;
        size_t usedLine
            = addEditorBreakpoint(nlsEvaluator, wfilename, static_cast<size_t>(line), errorMessage);
        if (usedLine == 0) {
            return;
        }
        refreshBreakpoints();
        emit breakpointToggled(static_cast<int>(usedLine), true);
        emit breakpointsChanged();
        return;
    }
    // unsaved edits: the marker waits for the save, on the next executable line
    if (!isValidBreakpointLine(line)) {
        line = findNextValidBreakpointLine(line);
        if (line <= 0) {
            return;
        }
    }
    if (indexOfLine(line) >= 0) {
        return;
    }
    EditorBreakpoint item;
    item.block = blockForLine(line);
    item.lastKnownLine = 0;
    if (!item.block.isValid()) {
        return;
    }
    breakpoints.push_back(item);
    update();
    emit breakpointToggled(line, true);
    emit breakpointsChanged();
}
//=============================================================================
void
QtBreakpointArea::removeBreakpointAt(int index)
{
    if (index < 0 || index >= breakpoints.size()) {
        return;
    }
    EditorBreakpoint item = breakpoints[index];
    int line = item.currentLine();
    breakpoints.removeAt(index);
    if (item.lastKnownLine != 0 && nlsEvaluator != nullptr) {
        if (documentModified) {
            pendingRemovals.push_back(item.lastKnownLine);
        } else {
            nlsEvaluator->removeBreakpoint(
                Nelson::QStringTowstring(currentFilename), static_cast<size_t>(item.lastKnownLine));
        }
    }
    update();
    emit breakpointToggled(line, false);
    emit breakpointsChanged();
}
//=============================================================================
void
QtBreakpointArea::toggleBreakpoint(int line)
{
    if (line <= 0 || currentFilename.isEmpty()) {
        return;
    }
    int index = indexOfLine(line);
    if (index >= 0) {
        removeBreakpointAt(index);
        return;
    }
    if (isCommentOnlyLine(line) && findNextValidBreakpointLine(line) <= 0) {
        return;
    }
    addBreakpointAt(line);
}
//=============================================================================
bool
QtBreakpointArea::hasBreakpoint(int line) const
{
    return indexOfLine(line) >= 0;
}
//=============================================================================
QSet<int>
QtBreakpointArea::getBreakpointLines() const
{
    QSet<int> lines;
    for (const EditorBreakpoint& item : breakpoints) {
        int line = item.currentLine();
        if (line > 0) {
            lines.insert(line);
        }
    }
    return lines;
}
//=============================================================================
const EditorBreakpoint*
QtBreakpointArea::breakpointAt(int line) const
{
    int index = indexOfLine(line);
    if (index < 0) {
        return nullptr;
    }
    return &breakpoints[index];
}
//=============================================================================
bool
QtBreakpointArea::setBreakpointEnabled(int line, bool enabled)
{
    int index = indexOfLine(line);
    if (index < 0) {
        return false;
    }
    breakpoints[index].enabled = enabled;
    if (breakpoints[index].lastKnownLine != 0 && nlsEvaluator != nullptr) {
        nlsEvaluator->setBreakpointEnabled(Nelson::QStringTowstring(currentFilename),
            static_cast<size_t>(breakpoints[index].lastKnownLine), enabled);
    }
    update();
    emit breakpointsChanged();
    return true;
}
//=============================================================================
bool
QtBreakpointArea::setBreakpointCondition(int line, const QString& condition, QString& errorMessage)
{
    errorMessage.clear();
    int index = indexOfLine(line);
    if (index < 0) {
        return false;
    }
    if (nlsEvaluator != nullptr) {
        std::wstring message;
        if (!nlsEvaluator->validateBreakpointCondition(
                Nelson::QStringTowstring(condition), message)) {
            errorMessage = Nelson::wstringToQString(message);
            return false;
        }
        if (breakpoints[index].lastKnownLine != 0) {
            nlsEvaluator->setBreakpointCondition(Nelson::QStringTowstring(currentFilename),
                static_cast<size_t>(breakpoints[index].lastKnownLine),
                Nelson::QStringTowstring(condition), message);
        }
    }
    breakpoints[index].condition = condition.trimmed();
    update();
    emit breakpointsChanged();
    return true;
}
//=============================================================================
void
QtBreakpointArea::setAllBreakpointsEnabled(bool enabled)
{
    for (EditorBreakpoint& item : breakpoints) {
        item.enabled = enabled;
    }
    if (nlsEvaluator != nullptr) {
        nlsEvaluator->setAllBreakpointsEnabled(enabled, Nelson::QStringTowstring(currentFilename));
    }
    update();
    emit breakpointsChanged();
}
//=============================================================================
void
QtBreakpointArea::clearAllBreakpoints()
{
    breakpoints.clear();
    pendingRemovals.clear();
    if (nlsEvaluator != nullptr) {
        nlsEvaluator->removeAllBreakpoints(Nelson::QStringTowstring(currentFilename));
    }
    update();
    emit breakpointsChanged();
}
//=============================================================================
void
QtBreakpointArea::setDocumentModified(bool modified)
{
    if (documentModified == modified) {
        return;
    }
    documentModified = modified;
    update();
}
//=============================================================================
bool
QtBreakpointArea::isDocumentModified() const
{
    return documentModified;
}
//=============================================================================
void
QtBreakpointArea::syncAfterSave()
{
    documentModified = false;
    if (nlsEvaluator == nullptr || currentFilename.isEmpty()) {
        update();
        return;
    }
    std::wstring wfilename = Nelson::QStringTowstring(currentFilename);
    for (int line : pendingRemovals) {
        nlsEvaluator->removeBreakpoint(wfilename, static_cast<size_t>(line));
    }
    pendingRemovals.clear();
    // markers whose line was deleted go away, the others follow their block
    QList<EditorBreakpoint> survivors;
    for (const EditorBreakpoint& item : breakpoints) {
        int line = item.currentLine();
        if (line <= 0) {
            if (item.lastKnownLine != 0) {
                nlsEvaluator->removeBreakpoint(wfilename, static_cast<size_t>(item.lastKnownLine));
            }
            continue;
        }
        survivors.push_back(item);
    }
    for (const EditorBreakpoint& item : survivors) {
        int line = item.currentLine();
        if (item.lastKnownLine == 0) {
            std::wstring errorMessage;
            size_t usedLine = addEditorBreakpoint(
                nlsEvaluator, wfilename, static_cast<size_t>(line), errorMessage);
            if (usedLine != 0) {
                std::wstring conditionError;
                nlsEvaluator->setBreakpointCondition(
                    wfilename, usedLine, Nelson::QStringTowstring(item.condition), conditionError);
                nlsEvaluator->setBreakpointEnabled(wfilename, usedLine, item.enabled);
            }
            continue;
        }
        if (line != item.lastKnownLine) {
            nlsEvaluator->moveBreakpoint(
                wfilename, static_cast<size_t>(item.lastKnownLine), static_cast<size_t>(line));
        }
        // the saved text may have changed what is executable on that line
        size_t adjustedLine = static_cast<size_t>(line);
        std::wstring errorMessage;
        if (nlsEvaluator->adjustBreakpointLine(
                wfilename, static_cast<size_t>(line), adjustedLine, errorMessage)) {
            if (adjustedLine != static_cast<size_t>(line)) {
                nlsEvaluator->moveBreakpoint(wfilename, static_cast<size_t>(line), adjustedLine);
            }
        } else {
            nlsEvaluator->removeBreakpoint(wfilename, static_cast<size_t>(line));
        }
    }
    refreshBreakpoints();
    emit breakpointsChanged();
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
    executionIsError = false;
    executionMessage.clear();
    update();
}
//=============================================================================
int
QtBreakpointArea::getExecutionLine() const
{
    return currentExecutionLine;
}
//=============================================================================
void
QtBreakpointArea::setErrorStop(bool errorStop, const QString& message)
{
    executionIsError = errorStop;
    executionMessage = message;
    update();
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
bool
QtBreakpointArea::isCommentOnlyLine(int lineNumber) const
{
    QTextDocument* doc = tEditor->document();
    if (!doc) {
        return false;
    }
    bool inBlockComment = false;
    int currentLine = 1;
    for (QTextBlock block = doc->begin(); block.isValid(); block = block.next(), ++currentLine) {
        const QString text = block.text().trimmed();
        const bool lineIsComment = inBlockComment || text.startsWith('%');
        if (currentLine == lineNumber) {
            return lineIsComment;
        }
        if (text.startsWith("%{")) {
            inBlockComment = true;
        }
        if (text.startsWith("%}")) {
            inBlockComment = false;
        }
    }
    return false;
}
//=============================================================================
QColor
QtBreakpointArea::markerColor(const EditorBreakpoint& bp) const
{
    if (documentModified || bp.lastKnownLine == 0) {
        return invalidColor();
    }
    if (!bp.enabled) {
        return disabledColor();
    }
    if (!bp.condition.isEmpty()) {
        return conditionalColor();
    }
    return activeColor();
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
        int index = indexOfLine(lineNumber);
        if (index >= 0) {
            p.setRenderHint(QPainter::Antialiasing, true);
            QColor color = markerColor(breakpoints[index]);
            p.setBrush(QBrush(color));
            p.setPen(color.darker(130));
            p.drawEllipse(
                QPoint(centerX, centerY), BREAKPOINT_MARKER_RADIUS, BREAKPOINT_MARKER_RADIUS);
        }
        if (lineNumber == currentExecutionLine) {
            // a green arrow marks the line about to run
            p.setRenderHint(QPainter::Antialiasing, true);
            QPolygon arrow;
            arrow << QPoint(2, centerY - 5) << QPoint(width() - 2, centerY)
                  << QPoint(2, centerY + 5);
            QColor color = executionColor();
            p.setBrush(QBrush(color));
            p.setPen(color.darker(130));
            p.drawPolygon(arrow);
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
bool
QtBreakpointArea::editConditionAt(int line)
{
    const EditorBreakpoint* bp = breakpointAt(line);
    if (bp == nullptr) {
        addBreakpointAt(line);
        bp = breakpointAt(line);
        if (bp == nullptr) {
            // the marker moved to the next executable line
            for (const EditorBreakpoint& item : breakpoints) {
                if (item.currentLine() > line) {
                    line = item.currentLine();
                    bp = &item;
                    break;
                }
            }
        }
        if (bp == nullptr) {
            return false;
        }
    }
    QString condition = bp->condition;
    while (true) {
        bool ok = false;
        condition = QInputDialog::getText(this, TR("Set/Modify Condition"),
            TR("Pause on line %1 only when this expression is true:").arg(line), QLineEdit::Normal,
            condition, &ok);
        if (!ok) {
            return false;
        }
        QString errorMessage;
        if (setBreakpointCondition(line, condition, errorMessage)) {
            return true;
        }
        QMessageBox::warning(this, TR("Set/Modify Condition"), errorMessage);
    }
}
//=============================================================================
void
QtBreakpointArea::contextMenuEvent(QContextMenuEvent* event)
{
    int line = lineNumberAtPosition(event->pos().y());
    if (line <= 0 || currentFilename.isEmpty()) {
        return;
    }
    const EditorBreakpoint* bp = breakpointAt(line);
    QMenu menu(this);
    QAction* toggleAction = menu.addAction(bp ? TR("Clear Breakpoint") : TR("Set Breakpoint"));
    QAction* conditionAction
        = menu.addAction(bp ? TR("Set/Modify Condition...") : TR("Set Conditional Breakpoint..."));
    QAction* enableAction = nullptr;
    if (bp != nullptr) {
        enableAction
            = menu.addAction(bp->enabled ? TR("Disable Breakpoint") : TR("Enable Breakpoint"));
    }
    menu.addSeparator();
    QAction* enableAllAction = menu.addAction(TR("Enable All Breakpoints in File"));
    QAction* disableAllAction = menu.addAction(TR("Disable All Breakpoints in File"));
    QAction* clearAllInFileAction = menu.addAction(TR("Clear All Breakpoints in File"));
    QAction* clearAllAction = menu.addAction(TR("Clear All Breakpoints"));
    bool hasAny = !breakpoints.isEmpty();
    enableAllAction->setEnabled(hasAny);
    disableAllAction->setEnabled(hasAny);
    clearAllInFileAction->setEnabled(hasAny);
    QAction* chosen = menu.exec(event->globalPos());
    if (chosen == nullptr) {
        return;
    }
    if (chosen == toggleAction) {
        toggleBreakpoint(line);
    } else if (chosen == conditionAction) {
        editConditionAt(line);
    } else if (chosen == enableAction && bp != nullptr) {
        setBreakpointEnabled(line, !bp->enabled);
    } else if (chosen == enableAllAction) {
        setAllBreakpointsEnabled(true);
    } else if (chosen == disableAllAction) {
        setAllBreakpointsEnabled(false);
    } else if (chosen == clearAllInFileAction) {
        clearAllBreakpoints();
    } else if (chosen == clearAllAction) {
        if (nlsEvaluator != nullptr) {
            nlsEvaluator->clearBreakpoints();
        }
        breakpoints.clear();
        pendingRemovals.clear();
        update();
        emit breakpointsChanged();
    }
}
//=============================================================================
QString
QtBreakpointArea::tooltipForLine(int line) const
{
    QStringList parts;
    if (line == currentExecutionLine) {
        if (executionIsError) {
            parts << TR("Paused on error at line %1").arg(line);
            if (!executionMessage.isEmpty()) {
                parts << executionMessage;
            }
        } else {
            parts << TR("Paused at line %1").arg(line);
        }
    }
    const EditorBreakpoint* bp = breakpointAt(line);
    if (bp != nullptr) {
        QString text = TR("Breakpoint on line %1").arg(line);
        if (!bp->enabled) {
            text += TR(" (disabled)");
        }
        parts << text;
        if (!bp->condition.isEmpty()) {
            parts << TR("Condition: %1").arg(bp->condition);
        }
        if (documentModified || bp->lastKnownLine == 0) {
            parts << TR("Invalid until the file is saved.");
        }
    }
    if (parts.isEmpty()) {
        return TR("Click to set or clear a breakpoint");
    }
    return parts.join("\n");
}
//=============================================================================
bool
QtBreakpointArea::event(QEvent* event)
{
    if (event->type() == QEvent::ToolTip) {
        auto* helpEvent = static_cast<QHelpEvent*>(event);
        int line = lineNumberAtPosition(helpEvent->pos().y());
        if (line > 0) {
            QToolTip::showText(helpEvent->globalPos(), tooltipForLine(line), this);
        } else {
            QToolTip::hideText();
        }
        return true;
    }
    return QWidget::event(event);
}
//=============================================================================
