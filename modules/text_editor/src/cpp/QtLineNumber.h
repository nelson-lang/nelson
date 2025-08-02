//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <QtWidgets/QWidget>
#include "QtTextEdit.h"
//=============================================================================
class QtLineNumber : public QWidget
{
    Q_OBJECT
public:
    QtLineNumber(QtTextEdit* textEditor);

protected:
    virtual void
    paintEvent(QPaintEvent* paintEvent);

private:
    QtTextEdit* tEditor;
};
//=============================================================================
