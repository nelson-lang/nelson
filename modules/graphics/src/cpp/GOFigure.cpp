//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DefaultProperties.hpp"
#include "GOFigure.hpp"
#include "GOStringProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOOnOffSwitchProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOVector4DProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOFigure::GOFigure(GOWindow* win)
{
    m_win = win;
    registerProperties();
    initializeProperties();
}
//=============================================================================
void
GOFigure::registerProperties()
{
    setType(FIGURE_TYPE_STR);
    addProperty(TYPE_PROPERTY_STR, new GOStringProperty());
    addProperty(TAG_PROPERTY_STR, new GOStringProperty());
    addProperty(NUMBER_PROPERTY_STR, new GOScalarDoubleProperty());
    addProperty(NAME_PROPERTY_STR, new GOStringProperty());
    addProperty(VISIBLE_PROPERTY_STR, new GOOnOffSwitchProperty());
    addProperty(USERDATA_PROPERTY_STR, new GOArrayOfProperty());
    addProperty(COLOR_PROPERTY_STR, new GOColorProperty());
    addProperty(OUTERPOSITION_PROPERTY_STR, new GOVector4DProperty());
    addProperty(INNERPOSITION_PROPERTY_STR, new GOVector4DProperty());
    addProperty(POSITION_PROPERTY_STR, new GOVector4DProperty());
}
//=============================================================================
void
GOFigure::initializeProperties()
{
    GOProperty* property = nullptr;
    setPropertyAsStringValue(TYPE_PROPERTY_STR, FIGURE_TYPE_STR);
    property = this->searchProperty(TYPE_PROPERTY_STR);
    property->forceWriteProtected();

    property = this->searchProperty(NUMBER_PROPERTY_STR);
    GOWindow* goWinPtr = this->getParentWindow();
    if (goWinPtr != nullptr) {
        setPropertyAsScalarDoubleValue(NUMBER_PROPERTY_STR, goWinPtr->ID());
    }
    property->forceWriteProtected();

    setPropertyAsStringValue(TAG_PROPERTY_STR, "");
    setPropertyAsStringValue(NAME_PROPERTY_STR, "");

    setPropertyAsOnOffSwitchValue(VISIBLE_PROPERTY_STR, DEFAULT_FIGURE_VISIBLE);
    setPropertyAsArrayOfValue(USERDATA_PROPERTY_STR, ArrayOf::emptyConstructor());
    setPropertyAsColorValue(COLOR_PROPERTY_STR, DEFAULT_FIGURE_RED_COLOR,
        DEFAULT_FIGURE_GREEN_COLOR, DEFAULT_FIGURE_BLUE_COLOR);
    refreshProperties();
}
//=============================================================================
bool
GOFigure::resized()
{
    return m_resized;
}
//=============================================================================
int
GOFigure::computeYOuterInnerOffset()
{
    QRect qGeometry = m_win->geometry();
    QRect qFrameGeometry = m_win->frameGeometry();
    QSize qOffset = qFrameGeometry.size() - qGeometry.size();
    return qOffset.height();
}
//=============================================================================
int
GOFigure::computeXOuterInnerOffset()
{
    QRect qGeometry = m_win->geometry();
    QRect qFrameGeometry = m_win->frameGeometry();
    QSize qOffset = qFrameGeometry.size() - qGeometry.size();
    return qOffset.width();
}
//=============================================================================
void
GOFigure::refreshColorProperty()
{
    QWidget* qWidget = m_win->getQWidget();
    if (qWidget != nullptr) {
        QPalette qPalette = qWidget->palette();
        QBrush qBrush = qPalette.base();
        QColor qColor = qBrush.color();
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
        float r;
        float g;
        float b;
#else
        qreal r;
        qreal g;
        qreal b;
#endif
        qColor.getRgbF(&r, &g, &b);
        GOProperty* property = this->searchProperty(COLOR_PROPERTY_STR);
        if (property != nullptr) {
            auto* colorProperty = (GOColorProperty*)property;
            colorProperty->value(r, g, b);
        }
    }
}
//=============================================================================
void
GOFigure::refreshInnerPositionProperty()
{
    QRect qGeometry = m_win->geometry();
    QSize qSize = qGeometry.size();
    QPoint qPoint = qGeometry.topLeft();
    GOProperty* property = this->searchProperty(INNERPOSITION_PROPERTY_STR);
    if (property != nullptr) {
        auto* innerPositionProperty = (GOVector4DProperty*)property;
        innerPositionProperty->value(qPoint.x(), qPoint.y(), qSize.width(), qSize.height());
    }
}
//=============================================================================
void
GOFigure::refreshPositionProperty()
{
    GOProperty* property = this->searchProperty(POSITION_PROPERTY_STR);
    if (property != nullptr) {
        QRect qFrameGeometry = m_win->frameGeometry();
        QSize qSize = qFrameGeometry.size();
        QPoint qPoint = qFrameGeometry.topLeft();

        auto* positionProperty = (GOVector4DProperty*)property;
        positionProperty->value(qPoint.x(), qPoint.y(), qSize.width(), qSize.height());
    }
}
//=============================================================================
void
GOFigure::refreshOuterPositionProperty()
{
    QRect qframeGeometry = m_win->frameGeometry();
    QSize qSize = qframeGeometry.size();
    QPoint qPoint = qframeGeometry.topLeft();
    GOProperty* property = this->searchProperty(OUTERPOSITION_PROPERTY_STR);
    if (property != nullptr) {
        auto* outerPositionProperty = (GOVector4DProperty*)property;
        outerPositionProperty->value(qPoint.x(), qPoint.y(), qSize.width(), qSize.height());
    }
}
//=============================================================================
void
GOFigure::refreshProperties()
{
    refreshInnerPositionProperty();
    refreshPositionProperty();
    refreshOuterPositionProperty();
    m_win->refreshProperties();
}
//=============================================================================
void
GOFigure::paintMe(GraphicRenderer& gc)
{
    m_resized = false;
    GOProperty* property = this->searchProperty(COLOR_PROPERTY_STR);

    if (property != nullptr) {
        auto* colorProperty = (GOColorProperty*)property;
        if (colorProperty != nullptr) {
            gc.clear(colorProperty->data());
        }
    }
}
//=============================================================================
void
GOFigure::resizeGL(int width, int height)
{
    m_resized = true;
    refreshProperties();
}
//=============================================================================
void
GOFigure::repaint()
{
    applyVisibleProperty();
    applyBackgroundProperty();
    applyNameProperty();
    applyPositionProperty();
    applyOuterPositionProperty();
    applyInnerPositionProperty();
    refreshProperties();
    m_win->update();
}
//=============================================================================
void
GOFigure::applyBackgroundProperty()
{
    GOProperty* property = this->searchProperty(COLOR_PROPERTY_STR);
    if (property != nullptr) {
        auto* colorProperty = (GOColorProperty*)property;
        if (colorProperty->isModified()) {

            QColor qColor;
            std::vector<double> color = colorProperty->data();
            qColor.setRedF(color[0]);
            qColor.setGreenF(color[1]);
            qColor.setBlueF(color[2]);
            QWidget* qWidget = m_win->getQWidget();
            QPalette px = qWidget->palette();
            px.setColor(QPalette::Window, qColor);
            qWidget->setPalette(px);
            qWidget->update();
            colorProperty->clearModified();
        }
    }
}
//=============================================================================
void
GOFigure::applyVisibleProperty()
{
    GOProperty* property = this->searchProperty(VISIBLE_PROPERTY_STR);
    if (property != nullptr) {
        auto* visibleProperty = (GOOnOffSwitchProperty*)property;
        if (visibleProperty->isModified()) {
            m_win->setVisible(visibleProperty->asLogical());
            visibleProperty->clearModified();
        }
    }
}
//=============================================================================
void
GOFigure::applyNameProperty()
{
    GOProperty* property = this->searchProperty(NAME_PROPERTY_STR);
    if (property != nullptr) {
        auto* nameProperty = (GOStringProperty*)property;
        if (nameProperty != nullptr) {
            if (nameProperty->isModified()) {
                std::string name = nameProperty->data();
                std::string title = "Figure " + std::to_string(m_win->ID());
                if (!name.empty()) {
                    title = title + ": " + name;
                }
                m_win->setWindowTitle(title.c_str());
                nameProperty->clearModified();
            }
        }
    }
}
//=============================================================================
void
GOFigure::applyOuterPositionProperty()
{
    GOProperty* property = this->searchProperty(OUTERPOSITION_PROPERTY_STR);
    if (property != nullptr) {
        auto* outerPositionProperty = (GOVector4DProperty*)property;
        if (outerPositionProperty != nullptr) {
            if (outerPositionProperty->isModified()) {
                std::vector<double> positions = outerPositionProperty->data();
                int xOffset = computeXOuterInnerOffset();
                int yOffset = computeYOuterInnerOffset();
                QRect qRect(
                    positions[0], positions[1], positions[2] - xOffset, positions[3] - yOffset);
                m_win->resize(qRect.size());
                m_win->move(qRect.topLeft());
                outerPositionProperty->clearModified();
            }
        }
    }
}
//=============================================================================
void
GOFigure::applyInnerPositionProperty()
{
    GOProperty* property = this->searchProperty(INNERPOSITION_PROPERTY_STR);
    if (property != nullptr) {
        auto* innerPositionProperty = (GOVector4DProperty*)property;
        if (innerPositionProperty != nullptr) {
            if (innerPositionProperty->isModified()) {
                std::vector<double> positions = innerPositionProperty->data();
                int xOffset = computeXOuterInnerOffset();
                int yOffset = computeYOuterInnerOffset();
                QRect qRect(positions[0] - xOffset + 1, positions[1] - yOffset + 1, positions[2],
                    positions[3]);
                m_win->resize(qRect.size());
                m_win->move(qRect.topLeft());
                innerPositionProperty->clearModified();
            }
        }
    }
}
//=============================================================================
void
GOFigure::applyPositionProperty()
{
    GOProperty* property = this->searchProperty(POSITION_PROPERTY_STR);
    if (property != nullptr) {
        auto* positionProperty = (GOVector4DProperty*)property;
        if (positionProperty != nullptr) {
            if (positionProperty->isModified()) {
                std::vector<double> positions = positionProperty->data();
                int xOffset = computeXOuterInnerOffset();
                int yOffset = computeYOuterInnerOffset();
                QRect qRect(
                    positions[0], positions[1], positions[2] - xOffset, positions[3] - yOffset);
                m_win->resize(qRect.size());
                m_win->move(qRect.topLeft());
                positionProperty->clearModified();
            }
        }
    }
}
//=============================================================================
GOWindow*
GOFigure::getParentWindow()
{
    return m_win;
}
//=============================================================================
uint64
GOFigure::id()
{
    return m_win->ID();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
