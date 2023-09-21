//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "TextEditor.hpp"
#include "QStringConverter.hpp"
#include "QtTextEditor.h"
#include "ForceWindowsTitleBarToDark.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static QtTextEditor* edit = nullptr;
//=============================================================================
bool
editor(Evaluator* eval)
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
editor(Evaluator* eval, const std::wstring& filename)
{
    bool res = false;
    if (edit == nullptr) {
        edit = new QtTextEditor(eval);
    }
    edit->loadOrCreateFile(wstringToQString(filename));
    edit->showNormal();
    edit->activateWindow();
    edit->raise();
    return res;
}
//=============================================================================
bool
editor(Evaluator* eval, const wstringVector& filenames)
{
    bool res = true;
    for (const auto& filename : filenames) {
        res = res && editor(eval, filename);
    }
    return res;
}
//=============================================================================
bool
closeEditor()
{
    if (edit != nullptr) {
        delete edit;
        edit = nullptr;
        return true;
    }
    return false;
}
//=============================================================================
}
//=============================================================================
