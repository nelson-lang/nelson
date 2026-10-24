//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QPointer>
#include "TextEditor.hpp"
#include "QStringConverter.hpp"
#include "QtTextEditor.h"
#include "ForceWindowsTitleBarToDark.hpp"
#include "HistoryBrowser.hpp"
#include "DesktopDocking.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// QPointer: while docked, the editor is a child of the desktop and could be
// destroyed with it at teardown; the guard avoids a double delete.
static QPointer<QtTextEditor> edit;
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
    ensureTextEditor(eval);
    edit->loadOrCreateFile(wstringToQString(filename));
    if (lineNumber > 0) {
        edit->gotoLineNumber(lineNumber);
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
} // namespace Nelson
//=============================================================================
