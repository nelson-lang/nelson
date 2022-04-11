//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtBaseWindow.hpp"
#include "QtRenderer.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
QtBaseWindow::resizeEvent(QResizeEvent* e)
{
    QWidget::resizeEvent(e);
    _goFig->resizeGL(width(), height());
}
//=============================================================================
void
QtBaseWindow::paintEvent(QPaintEvent* e)
{
    QWidget::paintEvent(e);
    QPainter pnt(this);
    QtRenderer gc(&pnt, 0, 0, width(), height());
    _goFig->paintMe(gc);
}
//=============================================================================
QtBaseWindow::QtBaseWindow(QWidget* parent, GOFigure* fig) : QWidget(parent)
{
    _goFig = fig;
    if (_goFig != nullptr) {
        _goFig->resizeGL(width(), height());
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
