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
#if QT_VERSION < QT_VERSION_CHECK(5, 13, 0)
#include <QtWidgets/QDesktopWidget>
#endif
#include <QtGui/QScreen>
#include <QtGui/QGuiApplication>
#include <QtGui/QIcon>
#include "GOWindow.hpp"
#include "GOFigure.hpp"
#include "QtBaseFigure.hpp"
#include "DefaultProperties.hpp"
#include "ModulesManager.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOWindow::GOWindow(unsigned id)
{
    m_initialized = false;
    graphicsRootPath = Nelson::GetModulePath(L"graphics");
    QString fileNameIcon = Nelson::wstringToQString(
        graphicsRootPath + std::wstring(L"/resources/icon-mathematical-plot.svg"));
    setWindowIcon(QPixmap(fileNameIcon));
    m_id = id;
    m_goFig = new GOFigure(this);
    std::string title = "Figure " + std::to_string(id);
    setWindowTitle(title.c_str());
    m_qtChild = new QtBaseFigure(NULL, m_goFig);
    m_layout = new QStackedWidget(this);
    QHBoxLayout* box = new QHBoxLayout(this);
    box->setContentsMargins(0, 0, 0, 0);
    setLayout(box);
    m_layout->addWidget(m_qtChild);
    m_layout->show();
    box->addWidget(m_layout);
    resize(DEFAULT_FIGURE_WIDTH, DEFAULT_FIGURE_HEIGHT);
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
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    m_clickX = e->position().x();
    m_clickY = e->position().y();
#else
    m_clickX = e->x();
    m_clickY = e->y();
#endif
    m_loop.exit();
}
//=============================================================================
QWidget*
GOWindow::getQWidget()
{
    return m_qtChild;
}
//=============================================================================
std::vector<double>
GOWindow::getCurrentScreenGeometry()
{
    QPoint qPoint = this->pos();
#if QT_VERSION >= QT_VERSION_CHECK(5, 13, 0)
    QScreen* currentQScreen = QGuiApplication::screenAt(qPoint);
    QRect currentScreenGeometry;
    if (currentQScreen != nullptr) {
        currentScreenGeometry = currentQScreen->geometry();
    }
#else
    QRect currentScreenGeometry = QApplication::desktop()->screenGeometry(qPoint);
#endif
    std::vector<double> res;
    res.reserve(4);
    res.push_back(currentScreenGeometry.x());
    res.push_back(currentScreenGeometry.y());
    res.push_back(currentScreenGeometry.width());
    res.push_back(currentScreenGeometry.height());
    return res;
}
} // namespace Nelson
//=============================================================================
