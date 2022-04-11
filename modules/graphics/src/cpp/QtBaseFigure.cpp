//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtGui/QPainter>
#include "QtBaseFigure.hpp"
#include "QtRenderer.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
QtBaseFigure::resizeEvent(QResizeEvent* e)
{
    QWidget::resizeEvent(e);
    m_goFig->resizeGL(width(), height());
}
//=============================================================================
void
QtBaseFigure::paintEvent(QPaintEvent* e)
{
    QWidget::paintEvent(e);
    QPainter painter(this);
    QtRenderer gc(&painter, 0, 0, width(), height());
    m_goFig->paintMe(gc);
}
//=============================================================================
QtBaseFigure::QtBaseFigure(QWidget* parent, GOFigure* fig) : QWidget(parent)
{
    m_goFig = fig;
    if (fig != nullptr) {
        m_goFig->resizeGL(width(), height());
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
