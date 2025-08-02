//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "VariablesEditor.hpp"
#include "QtVariablesEditor.h"
#include "QStringConverter.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson::VariablesEditor {
//=============================================================================
static QtVariablesEditor* qtVariablesEditor = nullptr;
//=============================================================================
void*
getVariablesEditor()
{
    return qtVariablesEditor;
}
//=============================================================================
bool
isVariablesEditorVisible()
{
    if (qtVariablesEditor) {
        return qtVariablesEditor->isVisible();
    }
    return false;
}
//=============================================================================
void
showVariablesEditor()
{
    if (qtVariablesEditor) {
        qtVariablesEditor->show();
        qtVariablesEditor->activateWindow();
        qtVariablesEditor->raise();
    }
}
//=============================================================================
void
hideVariablesEditor()
{
    if (qtVariablesEditor) {
        qtVariablesEditor->hide();
    }
}
//=============================================================================
void
toggleVisibilityVariablesEditor()
{
    if (qtVariablesEditor) {
        if (qtVariablesEditor->isVisible()) {
            qtVariablesEditor->hide();
        } else {
            qtVariablesEditor->show();
        }
    }
}
//=============================================================================
bool
createVariablesEditor(Evaluator* eval)
{
    if (!qtVariablesEditor) {
        qtVariablesEditor = new QtVariablesEditor(nullptr);
        qtVariablesEditor->setEvaluator(eval);
        return true;
    }
    return false;
}
//=============================================================================
bool
destroyVariablesEditor()
{
    if (qtVariablesEditor) {
        qtVariablesEditor->savePosition();
        qtVariablesEditor->deleteLater();
        qtVariablesEditor = nullptr;
        return true;
    }
    return false;
}
//=============================================================================
bool
openVariable(const std::wstring& variableName)
{
    if (qtVariablesEditor) {
        bool res = qtVariablesEditor->openVariable(wstringToQString(variableName));
        if (res) {
            qtVariablesEditor->show();
        }
        return res;
    }
    return false;
}
//=============================================================================
bool
updateVariablesEditor()
{
    if (qtVariablesEditor) {
        qtVariablesEditor->updateVariables();
        return true;
    }
    return false;
}
//=============================================================================
}
//=============================================================================
