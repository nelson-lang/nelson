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
#include "GOFigure.hpp"
#include "GOStringProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOOnOffSwitchProperty.hpp"
#include "GOArrayOfProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOFigure::GOFigure(GOWindow* win)
{
    m_width = 800;
    m_height = 600;
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
}
//=============================================================================
void
GOFigure::initializeProperties()
{
    GOProperty* property = nullptr;
    setPropertyAsStringValue(TYPE_PROPERTY_STR, FIGURE_TYPE_STR);
    property = this->searchProperty(TYPE_PROPERTY_STR);
    property->forceWriteProtected();

    setPropertyAsStringValue(TAG_PROPERTY_STR, "");

    property = this->searchProperty(NUMBER_PROPERTY_STR);
    GOWindow* goWinPtr = this->getParentWindow();
    if (goWinPtr != nullptr) {
        setPropertyAsScalarDoubleValue(NUMBER_PROPERTY_STR, goWinPtr->ID());
    }
    property->forceWriteProtected();

    setPropertyAsStringValue(TAG_PROPERTY_STR, "");
    setPropertyAsStringValue(NAME_PROPERTY_STR, "");

    setPropertyAsOnOffSwitchValue(VISIBLE_PROPERTY_STR, "on");
    setPropertyAsArrayOfValue(USERDATA_PROPERTY_STR, ArrayOf::emptyConstructor());
}
//=============================================================================
bool
GOFigure::resized()
{
    return m_resized;
}
//=============================================================================
int
GOFigure::getWidth()
{
    return m_width;
}
//=============================================================================
int
GOFigure::getHeight()
{
    return m_height;
}
//=============================================================================
void
GOFigure::refreshProperties()
{
    m_win->refreshProperties();
}
//=============================================================================
void
GOFigure::paintMe(GraphicRenderer& gc)
{
    m_resized = false;
}
//=============================================================================
void
GOFigure::resizeGL(int width, int height)
{
    m_width = width;
    m_height = height;
    m_resized = true;
    refreshProperties();
}
//=============================================================================
void
GOFigure::repaint()
{
    GOProperty* property = this->searchProperty(VISIBLE_PROPERTY_STR);
    if (property != nullptr) {
        auto* visibleProperty = (GOOnOffSwitchProperty*)property;
        m_win->setVisible(visibleProperty->asLogical());
    }

    property = this->searchProperty(NAME_PROPERTY_STR);
    if (property != nullptr) {
        auto* nameProperty = (GOStringProperty*)property;
        if (nameProperty != nullptr) {
            std::string name = nameProperty->data();
            std::string title = "Figure " + std::to_string(m_win->ID());
            if (!name.empty()) {
                title = title + ": " + name;

            }
            m_win->setWindowTitle(title.c_str());
        }
    }

    m_win->update();
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
