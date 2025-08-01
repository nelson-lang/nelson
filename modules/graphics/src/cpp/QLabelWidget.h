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
#include <QtWidgets/QLabel>
#include <QtGui/QKeyEvent>
#include "GOUIControl.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class GOUIControl;
//=============================================================================
class QLabelWidget : public QLabel
{
    //=============================================================================
    Q_OBJECT
    //=============================================================================
public:
    //=============================================================================
    QLabelWidget(GOUIControl* goParent, QWidget* parent = nullptr);
    //=============================================================================
protected:
    //=============================================================================
    void
    keyPressEvent(QKeyEvent* event) override;
    //=============================================================================
    void
    keyReleaseEvent(QKeyEvent* event) override;
    //=============================================================================
    void
    mousePressEvent(QMouseEvent* event) override;
    //=============================================================================
    void
    mouseReleaseEvent(QMouseEvent* event) override;
    //=============================================================================
private:
    GOUIControl* goParent = nullptr;
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
