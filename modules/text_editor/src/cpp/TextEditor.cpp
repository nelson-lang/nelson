//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QCoreApplication>
#include <QtCore/QPointer>
#include <QtCore/QTimer>
#include <algorithm>
#include <deque>
#include "TextEditor.hpp"
#include "QStringConverter.hpp"
#include "QtTextEditor.h"
#include "QtBreakpointArea.h"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "ForceWindowsTitleBarToDark.hpp"
#include "HistoryBrowser.hpp"
#include "DesktopDocking.hpp"
#include "MainThreadTaskQueue.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// QPointer: while docked, the editor is a child of the desktop and could be
// destroyed with it at teardown; the guard avoids a double delete.
static QPointer<QtTextEditor> edit;
//=============================================================================
// commands queued by the test hook run one by one while a K>> prompt waits for input
static std::deque<std::wstring> promptCommands;
static QPointer<QTimer> promptFeeder;
//=============================================================================
static bool feedingCommand = false;
//=============================================================================
static void
feedPromptCommand(Evaluator* eval)
{
    if (feedingCommand) {
        return;
    }
    if (promptCommands.empty()) {
        if (!promptFeeder.isNull()) {
            promptFeeder->stop();
        }
        return;
    }
    // the prompt pumps events: the command runs here, as a toolbar click would
    // a kept prompt still reads as ready while a step runs: wait for the idle prompt loop
    if (eval == nullptr || !eval->isBreakpointActive() || !eval->isAtPrompt()
        || !eval->commandQueue.isEmpty()
        || !MainThreadTaskQueue::getInstance()->isInterpreterIdle()) {
        return;
    }
    std::wstring command = promptCommands.front();
    promptCommands.pop_front();
    // the editor repaints the paused line on its own timer: show the stop before the command
    if (!edit.isNull()) {
        edit->refreshDebugState();
    }
    feedingCommand = true;
    try {
        eval->evaluateString(command, true);
    } catch (...) {
    }
    feedingCommand = false;
}
//=============================================================================
static void
queuePromptCommand(Evaluator* eval, const std::wstring& command)
{
    promptCommands.push_back(command);
    if (promptFeeder.isNull()) {
        promptFeeder = new QTimer(QCoreApplication::instance());
        promptFeeder->setInterval(50);
        QObject::connect(
            promptFeeder.data(), &QTimer::timeout, [eval]() { feedPromptCommand(eval); });
    }
    if (!promptFeeder->isActive()) {
        promptFeeder->start();
    }
}
//=============================================================================
static void
clearPromptCommands()
{
    promptCommands.clear();
    if (!promptFeeder.isNull()) {
        promptFeeder->stop();
    }
}
//=============================================================================
static QtTextEditor*
ensureTextEditor(Evaluator* eval)
{
    if (edit.isNull()) {
        edit = new QtTextEditor(eval);
        // re-dock the editor if it was docked when the session ended
        edit->restoreDockState();
    }
    return edit.data();
}
//=============================================================================
static void
showTextEditorWindow()
{
    if (edit.isNull()) {
        return;
    }
    if (edit->isDockedInDesktop()) {
        DesktopShowDockedWindow(edit.data());
        return;
    }
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(edit->winId());
#endif
    edit->showNormal();
    edit->activateWindow();
    edit->raise();
}
//=============================================================================
bool
textEditor(Evaluator* eval)
{
    ensureTextEditor(eval);
    showTextEditorWindow();
    return false;
}
//=============================================================================
bool
textEditor(Evaluator* eval, bool importSharedText)
{
    ensureTextEditor(eval);
    showTextEditorWindow();
    if (importSharedText) {
        edit->createTabUntitledWithText(
            wstringToQString(HistoryBrowser::getTextForTextEditor(true)));
    }
    return true;
}
//=============================================================================
bool
textEditor(Evaluator* eval, const std::wstring& filename)
{
    ensureTextEditor(eval);
    edit->loadOrCreateFile(wstringToQString(filename));
    showTextEditorWindow();
    return false;
}
//=============================================================================
bool
textEditor(Evaluator* eval, const std::wstring& filename, int lineNumber)
{
    return textEditor(eval, filename, lineNumber, 0);
}
//=============================================================================
bool
textEditor(Evaluator* eval, const std::wstring& filename, int lineNumber, int column)
{
    ensureTextEditor(eval);
    edit->loadOrCreateFile(wstringToQString(filename));
    if (lineNumber > 0) {
        edit->gotoLineNumber(lineNumber, column);
    }
    showTextEditorWindow();
    return false;
}
//=============================================================================
bool
textEditor(Evaluator* eval, const wstringVector& filenames)
{
    bool res = true;
    for (const auto& filename : filenames) {
        res = res && textEditor(eval, filename);
    }
    return res;
}
//=============================================================================
bool
closeTextEditor()
{
    if (!edit.isNull()) {
        // leave the desktop dock (destroyed with it) before deleting
        DesktopUndockWindow(edit.data(), false);
        delete edit.data();
        edit = nullptr;
        return true;
    }
    return false;
}
//=============================================================================
bool
isTextEditorVisible()
{
    if (!edit.isNull()) {
        return edit->isVisible();
    }
    return false;
}
//=============================================================================
void
showTextEditor()
{
    if (!edit.isNull()) {
        if (edit->isDockedInDesktop()) {
            DesktopShowDockedWindow(edit.data());
            return;
        }
        edit->show();
        edit->activateWindow();
        edit->setWindowState(Qt::WindowActive);
    }
}
//=============================================================================
static std::wstring
hookTextArgument(const ArrayOfVector& args, size_t index, const wchar_t* what)
{
    if (index >= args.size()
        || !(args[index].isRowVectorCharacterArray() || args[index].isScalarStringArray())) {
        Error(L"Nelson:editor:invalidArgument:wrongTypeInputArgument",
            L"Wrong type for input argument #%d.", static_cast<int>(index + 2));
    }
    (void)what;
    return args[index].asWideString();
}
//=============================================================================
static int
hookIntegerArgument(const ArrayOfVector& args, size_t index)
{
    if (index >= args.size() || !args[index].isNumeric() || !args[index].isScalar()) {
        Error(L"Nelson:editor:invalidArgument:wrongTypeInputArgument",
            L"Wrong type for input argument #%d.", static_cast<int>(index + 2));
    }
    return static_cast<int>(args[index].asDoubleScalar());
}
//=============================================================================
static QtBreakpointArea*
hookBreakpointArea()
{
    if (edit.isNull()) {
        return nullptr;
    }
    QtEditPane* pane = edit->currentEditPane();
    return pane != nullptr ? pane->getBreakpointArea() : nullptr;
}
//=============================================================================
ArrayOfVector
textEditorHook(Evaluator* eval, const std::wstring& action, const ArrayOfVector& args)
{
    ArrayOfVector retval;
    if (action == L"open") {
        ensureTextEditor(eval);
        edit->loadOrCreateFile(wstringToQString(hookTextArgument(args, 0, L"file")));
        edit->refreshDebugState();
        return retval;
    }
    if (action == L"close") {
        clearPromptCommands();
        closeTextEditor();
        return retval;
    }
    if (action == L"queueCommand") {
        queuePromptCommand(eval, hookTextArgument(args, 0, L"command"));
        return retval;
    }
    if (edit.isNull()) {
        Error(L"Nelson:editor:notOpen", L"The text editor is not open.");
    }
    if (action == L"cursor") {
        QtEditPane* pane = edit->currentEditPane();
        double line = 0;
        double column = 0;
        if (pane != nullptr && pane->getEditor() != nullptr) {
            QTextCursor cursor = pane->getEditor()->textCursor();
            line = cursor.blockNumber() + 1;
            column = cursor.positionInBlock() + 1;
        }
        double* position
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 2, stringVector(), false));
        position[0] = line;
        position[1] = column;
        retval << ArrayOf(NLS_DOUBLE, Dimensions(1, 2), position);
        return retval;
    }
    if (action == L"currentFile") {
        QtEditPane* pane = edit->currentEditPane();
        retval << ArrayOf::characterArray(
            pane != nullptr ? QStringTowstring(pane->getFileName()) : L"");
        return retval;
    }
    if (action == L"toggleBreakpoint") {
        QtBreakpointArea* area = hookBreakpointArea();
        if (area != nullptr) {
            area->toggleBreakpoint(hookIntegerArgument(args, 0));
        }
        edit->refreshDebugState();
        return retval;
    }
    if (action == L"setCondition") {
        QtBreakpointArea* area = hookBreakpointArea();
        QString errorMessage;
        bool ok = area != nullptr
            && area->setBreakpointCondition(hookIntegerArgument(args, 0),
                wstringToQString(hookTextArgument(args, 1, L"condition")), errorMessage);
        retval << ArrayOf::logicalScalar(ok);
        if (!ok && !errorMessage.isEmpty()) {
            retval << ArrayOf::characterArray(QStringTowstring(errorMessage));
        }
        return retval;
    }
    if (action == L"setEnabled") {
        QtBreakpointArea* area = hookBreakpointArea();
        // the flag may be logical or numeric
        bool enabled = args.size() > 1 && args[1].isLogical() ? args[1].asLogicalScalar()
                                                              : hookIntegerArgument(args, 1) != 0;
        bool ok
            = area != nullptr && area->setBreakpointEnabled(hookIntegerArgument(args, 0), enabled);
        retval << ArrayOf::logicalScalar(ok);
        return retval;
    }
    if (action == L"breakpoints") {
        QtBreakpointArea* area = hookBreakpointArea();
        stringVector fieldNames = { "line", "enabled", "condition", "invalid" };
        std::vector<int> lines;
        if (area != nullptr) {
            QList<int> sorted = area->getBreakpointLines().values();
            std::sort(sorted.begin(), sorted.end());
            for (int line : sorted) {
                lines.push_back(line);
            }
        }
        Dimensions dims(lines.size(), 1);
        if (lines.empty()) {
            retval << ArrayOf::emptyStruct(fieldNames, dims);
            return retval;
        }
        auto* rawStruct = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), fieldNames, false));
        ArrayOf structArray(NLS_STRUCT_ARRAY, dims, rawStruct, false, fieldNames);
        ArrayOfVector lineValues, enabledValues, conditionValues, invalidValues;
        for (int line : lines) {
            const EditorBreakpoint* bp = area->breakpointAt(line);
            lineValues << ArrayOf::doubleScalar(line);
            enabledValues << ArrayOf::logicalScalar(bp != nullptr && bp->enabled);
            conditionValues << ArrayOf::characterArray(
                bp != nullptr ? QStringTowstring(bp->condition) : L"");
            invalidValues << ArrayOf::logicalScalar(
                bp == nullptr || bp->lastKnownLine == 0 || area->isDocumentModified());
        }
        structArray.setFieldAsList("line", lineValues);
        structArray.setFieldAsList("enabled", enabledValues);
        structArray.setFieldAsList("condition", conditionValues);
        structArray.setFieldAsList("invalid", invalidValues);
        retval << structArray;
        return retval;
    }
    if (action == L"debugState") {
        edit->refreshDebugState();
        const DebugStopInfo& info = eval->getDebugStopInfo();
        bool paused = eval->isBreakpointActive() && info.reason != DebugStopReason::None;
        stringVector fieldNames
            = { "paused", "file", "line", "reason", "canStep", "errorMessage", "executionLine" };
        Dimensions dims(1, 1);
        auto* rawStruct = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, 1, fieldNames, false));
        ArrayOf structArray(NLS_STRUCT_ARRAY, dims, rawStruct, false, fieldNames);
        // setField takes a reference: give each value a name first
        auto setFieldValue
            = [&](const std::string& name, ArrayOf value) { structArray.setField(name, value); };
        QtEditPane* pane = edit->currentEditPane();
        setFieldValue("paused", ArrayOf::logicalScalar(paused));
        setFieldValue("file", ArrayOf::characterArray(paused ? info.filename : L""));
        setFieldValue("line", ArrayOf::doubleScalar(paused ? static_cast<double>(info.line) : 0.0));
        std::string reason = "none";
        switch (info.reason) {
        case DebugStopReason::Breakpoint:
            reason = "breakpoint";
            break;
        case DebugStopReason::Step:
            reason = "step";
            break;
        case DebugStopReason::Keyboard:
            reason = "keyboard";
            break;
        case DebugStopReason::Pause:
            reason = "pause";
            break;
        case DebugStopReason::Error:
            reason = "error";
            break;
        case DebugStopReason::RunToLine:
            reason = "runToLine";
            break;
        default:
            break;
        }
        setFieldValue("reason", ArrayOf::characterArray(paused ? reason : "none"));
        setFieldValue("canStep", ArrayOf::logicalScalar(paused && info.canStep));
        setFieldValue("errorMessage", ArrayOf::characterArray(info.errorMessage));
        setFieldValue("executionLine",
            ArrayOf::doubleScalar(
                pane != nullptr ? static_cast<double>(pane->getExecutionLine()) : -1.0));
        retval << structArray;
        return retval;
    }
    if (action == L"hover") {
        QtEditPane* pane = edit->currentEditPane();
        std::wstring text;
        if (pane != nullptr && pane->getEditor() != nullptr) {
            int line = hookIntegerArgument(args, 0);
            int column = hookIntegerArgument(args, 1);
            QTextBlock block = pane->getEditor()->document()->findBlockByLineNumber(line - 1);
            if (block.isValid()) {
                text
                    = QStringTowstring(pane->getEditor()->dataTipAt(block.position() + column - 1));
            }
        }
        retval << ArrayOf::characterArray(text);
        return retval;
    }
    if (action == L"command") {
        bool ok = edit->triggerDebugAction(wstringToQString(hookTextArgument(args, 0, L"name")));
        retval << ArrayOf::logicalScalar(ok);
        return retval;
    }
    if (action == L"save") {
        retval << ArrayOf::logicalScalar(edit->triggerDebugAction("save"));
        return retval;
    }
    if (action == L"insertLine") {
        QtEditPane* pane = edit->currentEditPane();
        if (pane != nullptr && pane->getEditor() != nullptr) {
            int line = hookIntegerArgument(args, 0);
            QString text = wstringToQString(hookTextArgument(args, 1, L"text"));
            QTextBlock block = pane->getEditor()->document()->findBlockByLineNumber(line - 1);
            if (block.isValid()) {
                QTextCursor cursor(block);
                cursor.movePosition(QTextCursor::StartOfBlock);
                cursor.insertText(text + "\n");
            }
        }
        return retval;
    }
    if (action == L"statusText") {
        retval << ArrayOf::characterArray(QStringTowstring(edit->debugStatusText()));
        return retval;
    }
    Error(L"Nelson:editor:invalidArgument:wrongValueInputArgument",
        L"Wrong value for input argument #%d.", 1);
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
