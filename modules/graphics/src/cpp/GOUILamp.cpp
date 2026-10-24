//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUILamp.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOColorProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOUILamp::GOUILamp()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUILamp::getType()
{
    return GO_PROPERTY_VALUE_UILAMP_STR;
}
//=============================================================================
std::string
GOUILamp::getClassName()
{
    return GO_CLASS_NAME_UI_LAMP_STR;
}
//=============================================================================
void
GOUILamp::constructProperties()
{
    registerUIComponentCommonProperties(false, false);
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOUILamp::setupDefaults()
{
    setupUIComponentCommonDefaults(100, 100, 20, 20, false, false);
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 0, 1, 0);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
