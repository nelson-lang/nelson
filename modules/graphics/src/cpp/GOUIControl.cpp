//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "GOUIControl.h"
#include "GOPropertyValues.hpp"
#include "GOPropertyNames.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOPositionHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static UIControlBindingFactory uiControlBindingFactory = nullptr;
//=============================================================================
void
setUIControlBindingFactory(UIControlBindingFactory factory)
{
    uiControlBindingFactory = factory;
}
//=============================================================================
UIControlBindingFactory
getUIControlBindingFactory()
{
    return uiControlBindingFactory;
}
//=============================================================================
static UIControlKeyInjector uiControlKeyInjector = nullptr;
//=============================================================================
void
setUIControlKeyInjector(UIControlKeyInjector injector)
{
    uiControlKeyInjector = injector;
}
//=============================================================================
UIControlKeyInjector
getUIControlKeyInjector()
{
    return uiControlKeyInjector;
}
//=============================================================================
GOUIControl::GOUIControl()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
GOUIControl::~GOUIControl()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    deleteWidget();
}
//=============================================================================
std::wstring
GOUIControl::getType()
{
    return GO_PROPERTY_VALUE_UICONTROL_STR;
}
//=============================================================================
void
GOUIControl::updateState()
{
    syncButtonGroupSelection();
    const std::wstring style = findStringProperty(GO_STYLE_PROPERTY_NAME_STR);
    if ((style == GO_PROPERTY_VALUE_LISTBOX_STR || style == GO_PROPERTY_VALUE_POPUPMENU_STR)
        && findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR) == 0) {
        setScalarDoubleDefault(GO_VALUE_PROPERTY_NAME_STR, 1);
    }
    // Text-entry and list widgets default to a white background, unlike the light gray of
    // buttons/text/slider. The base default is gray for every style (the Style is not known when
    // defaults are set), so when the Style is established as one of these and the background is
    // still the untouched gray, it is switched to white.
    if ((style == GO_PROPERTY_VALUE_EDIT_STR || style == GO_PROPERTY_VALUE_LISTBOX_STR
            || style == GO_PROPERTY_VALUE_POPUPMENU_STR)
        && hasChanged(GO_STYLE_PROPERTY_NAME_STR)) {
        std::vector<double> bg = findVectorDoubleProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR);
        const double gray = 245.0 / 255.0;
        if (bg.size() == 3 && std::abs(bg[0] - gray) < 1e-6 && std::abs(bg[1] - gray) < 1e-6
            && std::abs(bg[2] - gray) < 1e-6) {
            setThreeVectorDefault(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, 1.0, 1.0, 1.0);
        }
    }
    bool positionChanged = hasChanged(GO_POSITION_PROPERTY_NAME_STR)
        || hasChanged(GO_INNER_POSITION_PROPERTY_NAME_STR)
        || hasChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR);
    syncSimplePositionTriplet(this);
    if (positionChanged) {
        findProperty(GO_POSITION_PROPERTY_NAME_STR)->setModified(true);
    }
    // Headless figure host (WEB_GUI): no widget backend is registered; the
    // web path reads/writes properties only (describeForWeb / applyWebAction).
    if (!binding && parentGoWindow && !parentGoWindow->isHeadlessHost()
        && uiControlBindingFactory) {
        binding = uiControlBindingFactory(this);
    }
    if (!binding) {
        clearAllChanged();
        return;
    }
    // A Style change destroys the current widget and creates the one matching the new Style.
    bool isNewWidget = false;
    if (hasChanged(GO_STYLE_PROPERTY_NAME_STR)) {
        binding->destroy();
        if (binding->create()) {
            isNewWidget = true;
            clearChanged(GO_STYLE_PROPERTY_NAME_STR);
        }
    }
    if (!binding->exists()) {
        clearAllChanged();
        return;
    }
    binding->applyState(isNewWidget);
    binding->setFocus();
}
//=============================================================================
void
GOUIControl::buildWidget(GOFigureHost* f)
{
    parentGoWindow = f;
}
//=============================================================================
void
GOUIControl::deleteWidget()
{
    if (binding) {
        binding->hide();
        delete binding;
        binding = nullptr;
    }
}
//=============================================================================
void
GOUIControl::show()
{
    if (binding) {
        binding->show();
    }
}
//=============================================================================
void
GOUIControl::hide()
{
    if (binding) {
        binding->hide();
    }
}
//=============================================================================
void
GOUIControl::setFocus()
{
    if (binding) {
        binding->setFocus();
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
