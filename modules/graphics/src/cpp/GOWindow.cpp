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
#define _CRT_SECURE_NO_WARNINGS
//=============================================================================
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QApplication>
#include "GOWindow.hpp"
#include "GOFigure.hpp"
#include "QtBaseFigure.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOWindow::GOWindow(unsigned id)
{
    m_initialized = false;
    // setWindowIcon(QPixmap(":/images/freemat_small_mod_64.png"));
    m_id = id;
    m_goFig = new GOFigure(this);
    std::string title = "Figure " + std::to_string(id);
    setWindowTitle(title.c_str());
    m_qtChild = new QtBaseFigure(NULL, m_goFig);
    m_layout = new QStackedWidget(this);
    QHBoxLayout* box = new QHBoxLayout(this);
    box->setMargin(0);
    setLayout(box);
    m_layout->addWidget(m_qtChild);
    m_layout->show();
    box->addWidget(m_layout);
    resize(600, 400);
    m_initialized = true;
}
//=============================================================================
GOWindow::~GOWindow() = default;
//=============================================================================
unsigned
GOWindow::ID()
{
    return m_id;
}
//=============================================================================
GOFigure*
GOWindow::getGOFigure()
{
    return m_goFig;
}
//=============================================================================
void
GOWindow::refreshProperties()
{
    if (!m_initialized) {
        return;
    }
    if (m_layout->currentWidget() != m_qtChild) {
        m_layout->setCurrentWidget(m_qtChild);
    }
    update();
}
//=============================================================================
void
GOWindow::getClick(int& x, int& y)
{
    QApplication::setOverrideCursor(Qt::CrossCursor);
    m_loop.exec();
    x = m_clickX;
    y = m_clickX;
    QApplication::restoreOverrideCursor();
}
//=============================================================================
void
GOWindow::closeEvent(QCloseEvent* e)
{}
//=============================================================================
void
GOWindow::mousePressEvent(QMouseEvent* e)
{
    m_clickX = e->x();
    m_clickY = e->y();
    m_loop.exit();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
