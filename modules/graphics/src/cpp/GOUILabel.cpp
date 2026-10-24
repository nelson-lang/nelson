//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUILabel.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOAlignHorizProperty.hpp"
#include "GOColorProperty.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringVectorProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOUILabel::GOUILabel()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUILabel::getType()
{
    return GO_PROPERTY_VALUE_UILABEL_STR;
}
//=============================================================================
std::string
GOUILabel::getClassName()
{
    return GO_CLASS_NAME_UI_LABEL_STR;
}
//=============================================================================
void
GOUILabel::constructProperties()
{
    registerUIComponentCommonProperties();
    registerProperty(new GOUITextVectorProperty, GO_TEXT_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty({ GO_PROPERTY_VALUE_NONE_STR,
                         GO_PROPERTY_VALUE_TEX_STR, L"latex", L"html" }),
        GO_INTERPRETER_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_WORD_WRAP_PROPERTY_NAME_STR);
    registerProperty(new GOAlignHorizProperty, GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty({ GO_PROPERTY_VALUE_CENTER_STR,
                         GO_PROPERTY_VALUE_TOP_STR, GO_PROPERTY_VALUE_BOTTOM_STR }),
        GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOUILabel::setupDefaults()
{
    setupUIComponentCommonDefaults(100, 100, 31, 22);
    GOStringVectorProperty* text
        = static_cast<GOStringVectorProperty*>(findProperty(GO_TEXT_PROPERTY_NAME_STR));
    text->data(L"Label");
    setRestrictedStringDefault(GO_INTERPRETER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setRestrictedStringDefault(GO_WORD_WRAP_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(
        GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LEFT_STR);
    setRestrictedStringDefault(
        GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CENTER_STR);
    // Label default background is 'none'
    GOColorProperty* backgroundColor
        = static_cast<GOColorProperty*>(findProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, false));
    if (backgroundColor) {
        backgroundColor->set(ArrayOf::characterArray(GO_PROPERTY_VALUE_NONE_STR));
        backgroundColor->clearModified();
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
