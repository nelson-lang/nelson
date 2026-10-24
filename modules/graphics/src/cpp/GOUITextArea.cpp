//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUITextArea.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOAlignHorizProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOStringVectorProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOUITextArea::GOUITextArea()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUITextArea::getType()
{
    return GO_PROPERTY_VALUE_UITEXTAREA_STR;
}
//=============================================================================
std::string
GOUITextArea::getClassName()
{
    return GO_CLASS_NAME_UI_TEXTAREA_STR;
}
//=============================================================================
void
GOUITextArea::constructProperties()
{
    registerUIComponentCommonProperties();
    registerProperty(new GOUITextVectorProperty(true), GO_VALUE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_EDITABLE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_WORD_WRAP_PROPERTY_NAME_STR);
    registerProperty(new GOAlignHorizProperty, GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_PLACEHOLDER_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_VALUE_CHANGING_FCN_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOUITextArea::setupDefaults()
{
    setupUIComponentCommonDefaults(100, 100, 150, 60);
    setThreeVectorDefault(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, 1, 1, 1);
    GOStringVectorProperty* value
        = static_cast<GOStringVectorProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR));
    value->data(L"");
    setRestrictedStringDefault(GO_EDITABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_WORD_WRAP_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(
        GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LEFT_STR);
    setStringDefault(GO_PLACEHOLDER_PROPERTY_NAME_STR, L"");
}
//=============================================================================
} // namespace Nelson
//=============================================================================
