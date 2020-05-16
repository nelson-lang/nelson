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
#include <QtGui/QScreen>
#include <QtGui/QCursor>
#include <QtGui/QGuiApplication>
#include <QtWidgets/QApplication>
#include <QtWidgets/QDesktopWidget>
#include "GORoot.hpp"
#include "GOStringProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOOnOffSwitchProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOVector2DProperty.hpp"
#include "GOVector4DProperty.hpp"
#include "MainGuiObject.hpp"
#include "GOWindowManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GORoot::GORoot(void* qWindowPtr)
{
    m_qWindowPtr = qWindowPtr;
    registerProperties();
    initializeProperties();
}
//=============================================================================
void
GORoot::registerProperties()
{
    setType(ROOT_TYPE_STR);
    addProperty(TYPE_PROPERTY_STR, new GOStringProperty());
    addProperty(PARENT_PROPERTY_STR, new GOArrayOfProperty());
    addProperty(TAG_PROPERTY_STR, new GOStringProperty());
    addProperty(USERDATA_PROPERTY_STR, new GOArrayOfProperty());
    addProperty(SCREENDEPTH_PROPERTY_STR, new GOScalarDoubleProperty());
    addProperty(POINTERLOCATION_PROPERTY_STR, new GOVector2DProperty());
    addProperty(SCREENSIZE_PROPERTY_STR, new GOVector4DProperty());
    addProperty(CURRENTFIGURE_PROPERTY_STR, new GOArrayOfProperty());
    addProperty(CHILDREN_PROPERTY_STR, new GOArrayOfProperty());
}
//=============================================================================
void
GORoot::initializeProperties()
{
    GOProperty* property = nullptr;
    setPropertyAsStringValue(TYPE_PROPERTY_STR, ROOT_TYPE_STR);
    property = this->searchProperty(TYPE_PROPERTY_STR);
    property->forceWriteProtected();

    ArrayOf parent = ArrayOf::emptyConstructor();
    parent.promoteType(NLS_GO_HANDLE);
    setPropertyAsArrayOfValue(PARENT_PROPERTY_STR, parent);
    property = this->searchProperty(PARENT_PROPERTY_STR);
    property->forceWriteProtected();

    setPropertyAsStringValue(TAG_PROPERTY_STR, "");

    QScreen* mainScreen = QGuiApplication::primaryScreen();
    int screenDepth = mainScreen->depth();
    setPropertyAsScalarDoubleValue(SCREENDEPTH_PROPERTY_STR, screenDepth);
    property = this->searchProperty(SCREENDEPTH_PROPERTY_STR);
    property->forceWriteProtected();

    setPropertyAsArrayOfValue(USERDATA_PROPERTY_STR, ArrayOf::emptyConstructor());

    QPoint qPoint = QCursor::pos();
#if QT_VERSION >= QT_VERSION_CHECK(5, 13, 0)
    const QRect currentScreenGeometry = QGuiApplication::screenAt(qPoint)->availableGeometry();
#else
    const QRect currentScreenGeometry = QApplication::desktop()->screenGeometry(qPoint);
#endif
    setPropertyAsVector2DValue(
        POINTERLOCATION_PROPERTY_STR, qPoint.x(), currentScreenGeometry.height() - qPoint.y());

    QSize mainSize = mainScreen->availableSize();
    setPropertyAsVector4DValue(
        SCREENSIZE_PROPERTY_STR, 1.0, 1.0, mainSize.width(), mainSize.height());
    property = this->searchProperty(SCREENSIZE_PROPERTY_STR);
    property->forceWriteProtected();

    uint64 currentFigureID = getCurrentGOWindowID();
    ArrayOf gcf;
    if (currentFigureID == 0) {
        gcf = ArrayOf::emptyConstructor();
        gcf.promoteType(NLS_GO_HANDLE);
    } else {
        GOWindow* goWin = getGOWindow(currentFigureID);
        if (goWin) {
            gcf = ArrayOf::graphicObjectConstructor(goWin->getGOFigure());
        }
    }
    setPropertyAsArrayOfValue(CURRENTFIGURE_PROPERTY_STR, gcf);
    property = this->searchProperty(CURRENTFIGURE_PROPERTY_STR);
    property->forceWriteProtected();

    std::vector<GOWindow*> goWindows = getGOWindows();
    ArrayOf children;
    if (goWindows.empty()) {
        children = ArrayOf::emptyConstructor();
        children.promoteType(NLS_GO_HANDLE);
    } else {
        Dimensions dims(goWindows.size(), 1);
        nelson_handle* ptr = static_cast<nelson_handle*>(
            ArrayOf::allocateArrayOf(NLS_GO_HANDLE, dims.getElementCount(), stringVector(), false));

        for (size_t idx = 0; idx < goWindows.size(); ++idx) {
            GOFigure* fig = goWindows[idx]->getGOFigure();
            ptr[idx] = PTR_TO_NELSON_HANDLE(fig);
        }
        children = ArrayOf(NLS_GO_HANDLE, dims, (void*)ptr);
    }
    setPropertyAsArrayOfValue(CHILDREN_PROPERTY_STR, children);
    property = this->searchProperty(CHILDREN_PROPERTY_STR);
    property->forceWriteProtected();
}
//=============================================================================
bool
GORoot::resized()
{
    return m_resized;
}
//=============================================================================
int
GORoot::getWidth()
{
    return m_width;
}
//=============================================================================
int
GORoot::getHeight()
{
    return m_height;
}
//=============================================================================
void
GORoot::refreshProperties()
{
    QList<QScreen*> screens = QGuiApplication::screens();
    for (auto s : screens) {
        QRect screenGeometry = s->geometry();
        int height = screenGeometry.height();
        int width = screenGeometry.width();
    }
    QScreen* mainScreen = QGuiApplication::primaryScreen();
    int screenDepth = mainScreen->depth();
    GOScalarDoubleProperty* screenDepthProperty
        = (GOScalarDoubleProperty*)this->searchProperty(SCREENDEPTH_PROPERTY_STR);
    if (screenDepthProperty != nullptr) {
        screenDepthProperty->value(screenDepth);
    }
    GOVector2DProperty* pointerLocation
        = (GOVector2DProperty*)this->searchProperty(POINTERLOCATION_PROPERTY_STR);
    if (pointerLocation != nullptr) {
        QPoint qPoint = QCursor::pos();
#if QT_VERSION >= QT_VERSION_CHECK(5, 13, 0)
        const QRect currentScreenGeometry = QGuiApplication::screenAt(qPoint)->availableGeometry();
#else
        const QRect currentScreenGeometry = QApplication::desktop()->screenGeometry(qPoint);
#endif
        pointerLocation->value(qPoint.x(), currentScreenGeometry.height() - qPoint.y());
    }

    GOArrayOfProperty* goCurrentFigure
        = (GOArrayOfProperty*)this->searchProperty(CURRENTFIGURE_PROPERTY_STR);
    if (goCurrentFigure != nullptr) {
        ArrayOf gcf;
        uint64 currentFigureID = getCurrentGOWindowID();
        if (currentFigureID == 0) {
            gcf = ArrayOf::emptyConstructor();
            gcf.promoteType(NLS_GO_HANDLE);
        } else {
            GOWindow* goWin = getGOWindow(currentFigureID);
            if (goWin) {
                gcf = ArrayOf::graphicObjectConstructor(goWin->getGOFigure());
            }
        }
        goCurrentFigure->value(gcf);
    }

    std::vector<GOWindow*> goWindows = getGOWindows();
    ArrayOf children;
    if (goWindows.empty()) {
        children = ArrayOf::emptyConstructor();
        children.promoteType(NLS_GO_HANDLE);
    } else {
        Dimensions dims(goWindows.size(), 1);
        nelson_handle* ptr = static_cast<nelson_handle*>(
            ArrayOf::allocateArrayOf(NLS_GO_HANDLE, dims.getElementCount(), stringVector(), false));

        for (size_t idx = 0; idx < goWindows.size(); ++idx) {
            GOFigure* fig = goWindows[idx]->getGOFigure();
            ptr[idx] = PTR_TO_NELSON_HANDLE(fig);
        }
        children = ArrayOf(NLS_GO_HANDLE, dims, (void*)ptr);
    }
    GOArrayOfProperty* goChildren = (GOArrayOfProperty*)this->searchProperty(CHILDREN_PROPERTY_STR);
    goChildren->value(children);
}
//=============================================================================
void
GORoot::paintMe(GraphicRenderer& gc)
{
    m_resized = false;
}
//=============================================================================
void
GORoot::resizeGL(int width, int height)
{
    m_width = width;
    m_height = height;
    m_resized = true;
    refreshProperties();
}
//=============================================================================
void
GORoot::repaint()
{
    GOVector2DProperty* pointerLocation
        = (GOVector2DProperty*)this->searchProperty(POINTERLOCATION_PROPERTY_STR);
    std::vector<double> location = pointerLocation->data();

    QPoint qPoint = QCursor::pos();
#if QT_VERSION >= QT_VERSION_CHECK(5, 13, 0)
    const QRect currentScreenGeometry = QGuiApplication::screenAt(qPoint)->availableGeometry();
#else
    const QRect currentScreenGeometry = QApplication::desktop()->screenGeometry(qPoint);
#endif

    QPoint pos(location[0], currentScreenGeometry.height() - location[1]);
    QCursor::setPos(pos);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
