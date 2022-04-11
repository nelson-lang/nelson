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
#include <QtWidgets/QStackedWidget>
#include <QtCore/QEventLoop>
#include <QtGui/QMouseEvent>
#include <QtGui/QCloseEvent>
#include "GraphicObject.hpp"
#include "GOFigure.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class GOFigure;
//=============================================================================
class GOWindow : public QWidget
{
protected:
    QWidget* m_qtChild;
    QStackedWidget* m_layout;
    QEventLoop m_loop;
    bool m_initialized;
    unsigned m_id;
    GOFigure* m_goFig;
    int m_clickX, m_clickY;
    std::wstring graphicsRootPath;

public:
    GOWindow(unsigned id);
    ~GOWindow() override;
    unsigned
    ID();
    GOFigure*
    getGOFigure();
    void
    refreshProperties();
    void
    getClick(int& x, int& y);
    void
    closeEvent(QCloseEvent* e) override;
    void
    mousePressEvent(QMouseEvent* e) override;
    QWidget*
    getQWidget();
    std::vector<double>
    getCurrentScreenGeometry();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
