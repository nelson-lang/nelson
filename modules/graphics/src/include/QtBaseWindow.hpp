//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <QtWidgets/QWidget>
#include "GOFigure.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class QtBaseWindow : public QWidget
{
    GOFigure* _goFig;

public:
    QtBaseWindow(QWidget* parent, GOFigure* fig);
    void
    paintEvent(QPaintEvent* e) override;
    void
    resizeEvent(QResizeEvent* e) override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
