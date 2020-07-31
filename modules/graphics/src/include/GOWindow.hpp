//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
