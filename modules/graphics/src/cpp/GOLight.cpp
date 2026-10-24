//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOLight.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOVectorThreeDoubleProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOCommonProperties.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOLight::GOLight()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
GOLight::~GOLight() = default;
//=============================================================================
std::wstring
GOLight::getType()
{
    return GO_PROPERTY_VALUE_LIGHT_STR;
}
//=============================================================================
void
GOLight::constructProperties()
{
    static const std::vector<std::wstring> lightStyles
        = { GO_PROPERTY_VALUE_INFINITE_STR, GO_PROPERTY_VALUE_LOCAL_STR };

    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR, false);
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOThreeVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty(lightStyles), GO_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerCommonProperties(this);
    sortProperties();
}
//=============================================================================
void
GOLight::setupDefaults()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 1, 1, 1);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setThreeVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 1, 0, 1);
    setRestrictedStringDefault(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_INFINITE_STR);
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    initializeCommonProperties(this);
}
//=============================================================================
void
GOLight::paintMe(RenderInterface&)
{
}
//=============================================================================
} // namespace Nelson
//=============================================================================
