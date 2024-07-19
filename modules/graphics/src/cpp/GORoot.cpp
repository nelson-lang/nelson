//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtGui/QCursor>
#include <QtGui/QScreen>
#include <QtGui/QGuiApplication>
#include <QtWidgets/QApplication>
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GORoot.hpp"
#include "GOList.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOHelpers.hpp"
#include "GOFiguresManager.hpp"
#include "BaseFigureQt.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static GORoot* _graphicsRootObject = nullptr;
//=============================================================================
GORoot::GORoot()
{
    constructProperties();
    setupDefaults();
};
//=============================================================================
std::wstring
GORoot::getType()
{
    return GO_PROPERTY_VALUE_ROOT_STR;
}
//=============================================================================
void
GORoot::constructProperties()
{
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_POINTER_LOCATION_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CURRENT_FIGURE_PROPERTY_NAME_STR, false);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR, false);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR, false);
    registerProperty(new GOStringProperty, GO_UNITS_PROPERTY_NAME_STR, false);
    registerProperty(new GOScalarProperty, GO_SCREEN_DEPTH_PROPERTY_NAME_STR, false);
    registerProperty(new GOScalarProperty, GO_SCREEN_PIXELS_PER_INCH_PROPERTY_NAME_STR, false);
    registerProperty(new GOFourVectorProperty, GO_SCREEN_SIZE_PROPERTY_NAME_STR, false);
    sortProperties();
};
//=============================================================================
void
GORoot::updateState()
{
    QPoint qPoint = QCursor::pos();
    if (hasChanged(GO_POINTER_LOCATION_PROPERTY_NAME_STR)) {
        GOTwoVectorProperty* pos
            = (GOTwoVectorProperty*)findProperty(GO_POINTER_LOCATION_PROPERTY_NAME_STR, false);
        int X = (int)(pos->data()[0]);
        int Y = (int)(pos->data()[1]);
        qPoint.setX(X);
        qPoint.setY(Y);
        QCursor::setPos(qPoint);
        clearChanged(GO_POINTER_LOCATION_PROPERTY_NAME_STR);
    } else {
        setTwoVectorDefault(GO_POINTER_LOCATION_PROPERTY_NAME_STR, qPoint.x(), qPoint.y());
    }

    refreshScreenSizeProperty();
    refreshScreenDepthProperty();
    refreshCurrentFigureProperty();
    refreshChildrenProperty();
    refreshScreenPixelsPerInchProperty();
};
//=============================================================================
void
GORoot::setupDefaults()
{
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setTwoVectorDefault(GO_POINTER_LOCATION_PROPERTY_NAME_STR, 0, 0);
    setStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PIXELS_STR);
    refreshCurrentFigureProperty();
    refreshScreenSizeProperty();
    refreshScreenDepthProperty();
    refreshScreenPixelsPerInchProperty();
};
//=============================================================================
void
GORoot::paintMe(RenderInterface& gc) {};
//=============================================================================
void
GORoot::refreshScreenPixelsPerInchProperty()
{
    QScreen* mainScreen = QGuiApplication::primaryScreen();
    setScalarDoubleDefault(
        GO_SCREEN_PIXELS_PER_INCH_PROPERTY_NAME_STR, mainScreen->logicalDotsPerInch());
}
//=============================================================================
void
GORoot::refreshScreenSizeProperty()
{
    setFourVectorDefault(GO_SCREEN_SIZE_PROPERTY_NAME_STR, 1.0, 1.0,
        Nelson::BaseFigureQt::getCurrentScreenWidth(),
        Nelson::BaseFigureQt::getCurrentScreenHeight());
}
//=============================================================================
void
GORoot::refreshScreenDepthProperty()
{
    QScreen* mainScreen = QGuiApplication::primaryScreen();
    int screenDepth = mainScreen->depth();
    setScalarDoubleDefault(GO_SCREEN_DEPTH_PROPERTY_NAME_STR, screenDepth);
}
//=============================================================================
void
GORoot::refreshCurrentFigureProperty()
{
    int64 currentFigure = getCurrentFigure();
    if (currentFigure != NO_FIGURE) {
        setGoProperty(GO_CURRENT_FIGURE_PROPERTY_NAME_STR, getCurrentFigure());
    } else {
        GOGObjectsProperty* hp
            = (GOGObjectsProperty*)findProperty(GO_CURRENT_FIGURE_PROPERTY_NAME_STR);
        if (hp) {
            std::vector<int64> newval;
            hp->data(newval);
        }
    }
}
//=============================================================================
void
GORoot::refreshChildrenProperty()
{
    std::vector<int64> children = getFigureGraphicsObjects();
    GOGObjectsProperty* cp = (GOGObjectsProperty*)findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
    cp->data(children);
}
//=============================================================================
int64
graphicsRootObject()
{
    if (_graphicsRootObject == nullptr) {
        _graphicsRootObject = new GORoot();
    }
    _graphicsRootObject->updateState();
    return (int64)HANDLE_ROOT_OBJECT;
}
//=============================================================================
GORoot*
getGraphicsRootObject()
{
    if (_graphicsRootObject == nullptr) {
        _graphicsRootObject = new GORoot();
    }
    _graphicsRootObject->updateState();
    return _graphicsRootObject;
}
//=============================================================================
}
//=============================================================================
