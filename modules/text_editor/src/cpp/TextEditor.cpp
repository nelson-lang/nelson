//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "TextEditor.hpp"
#include "QStringConverter.hpp"
#include "QtTextEditor.h"
#include "ForceWindowsTitleBarToDark.hpp"
#include "HistoryBrowser.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static QtTextEditor* edit = nullptr;
//=============================================================================
bool
textEditor(Evaluator* eval)
{
    bool res = false;
    if (edit == nullptr) {
        edit = new QtTextEditor(eval);
    }
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(edit->winId());
#endif
    edit->showNormal();
    edit->activateWindow();
    edit->raise();
    return res;
}
//=============================================================================
bool
textEditor(Evaluator* eval, bool importSharedText)
{
    if (edit == nullptr) {
        edit = new QtTextEditor(eval);
    }
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(edit->winId());
#endif

    edit->showNormal();
    edit->activateWindow();
    edit->raise();
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
    bool res = false;
    if (edit == nullptr) {
        edit = new QtTextEditor(eval);
    }
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(edit->winId());
#endif
    edit->loadOrCreateFile(wstringToQString(filename));
    edit->showNormal();
    edit->activateWindow();
    edit->raise();
    return res;
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
    if (edit != nullptr) {
        delete edit;
        edit = nullptr;
        return true;
    }
    return false;
}
//=============================================================================
bool
isTextEditorVisible()
{
    if (edit != nullptr) {
        return edit->isVisible();
    }
    return false;
}
//=============================================================================
void
showTextEditor()
{
    if (edit != nullptr) {
        edit->show();
        edit->activateWindow();
        edit->setWindowState(Qt::WindowActive);
    }
}
//=============================================================================
}
//=============================================================================
