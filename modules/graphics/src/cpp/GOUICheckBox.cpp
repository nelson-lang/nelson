//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUICheckBox.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOCallbackProperty.hpp"
#include "GOScalarLogicalProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringVectorProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOUICheckBox::GOUICheckBox()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUICheckBox::getType()
{
    return GO_PROPERTY_VALUE_UICHECKBOX_STR;
}
//=============================================================================
std::string
GOUICheckBox::getClassName()
{
    return GO_CLASS_NAME_UI_CHECKBOX_STR;
}
//=============================================================================
void
GOUICheckBox::constructProperties()
{
    registerUIComponentCommonProperties(true, false);
    registerProperty(new GOScalarLogicalProperty, GO_VALUE_PROPERTY_NAME_STR);
    registerProperty(new GOUITextVectorProperty, GO_TEXT_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_WORD_WRAP_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOUICheckBox::setupDefaults()
{
    setupUIComponentCommonDefaults(100, 100, 84, 22, true, false);
    GOStringVectorProperty* text
        = static_cast<GOStringVectorProperty*>(findProperty(GO_TEXT_PROPERTY_NAME_STR));
    text->data(L"Check Box");
    setRestrictedStringDefault(GO_WORD_WRAP_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
