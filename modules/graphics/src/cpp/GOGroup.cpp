//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOGroup.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOGroup::~GOGroup() = default;
//=============================================================================
void
GOGroup::constructProperties()
{
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOGroup::setupDefaults()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setStringDefault(GO_DISPLAY_NAME_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_HGGROUP_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
}
//=============================================================================
GOGroup::GOGroup()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOGroup::getType()
{
    return GO_PROPERTY_VALUE_HGGROUP_STR;
}
//=============================================================================
void
GOGroup::updateState()
{
    if (hasChanged(GO_VISIBLE_PROPERTY_NAME_STR)) {
        GOGObjectsProperty* cp
            = static_cast<GOGObjectsProperty*>(this->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
        std::vector<int64> children(cp->data());
        bool turnOff = stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
        for (auto child : children) {
            GraphicsObject* cp = findGraphicsObject(child, false);
            if (cp) {
                auto property = cp->findProperty(GO_VISIBLE_PROPERTY_NAME_STR, false);
                if (property) {
                    property->set(ArrayOf::characterArrayConstructor(
                        turnOff ? GO_PROPERTY_VALUE_OFF_STR : GO_PROPERTY_VALUE_ON_STR));
                }
            }
        }
        clearChanged(GO_VISIBLE_PROPERTY_NAME_STR);
    }
}
//=============================================================================
void
GOGroup::paintMe(RenderInterface& gc)
{
    updateState();
    bool isVisible = stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    if (isVisible) {
        GOGObjectsProperty* cp
            = static_cast<GOGObjectsProperty*>(this->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
        std::vector<int64> children(cp->data());
        for (auto child : children) {
            GraphicsObject* cp = findGraphicsObject(child, false);
            if (cp) {
                cp->paintMe(gc);
            }
        }
    }
}
//=============================================================================
}
//=============================================================================
