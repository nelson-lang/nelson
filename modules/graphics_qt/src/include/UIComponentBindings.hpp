//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "GOUIWidgetBinding.hpp"
//=============================================================================
// One creator per App Designer component type, each defined in its
// UIBinding<Name>.cpp. createUIComponentBinding dispatches on the concrete model class
// and is the factory registered with the graphics module at load time.
//=============================================================================
namespace Nelson {
//=============================================================================
class GOUIComponent;
//=============================================================================
UIComponentBinding*
createUIButtonBinding(GOUIComponent* component);
UIComponentBinding*
createUICheckBoxBinding(GOUIComponent* component);
UIComponentBinding*
createUIDatePickerBinding(GOUIComponent* component);
UIComponentBinding*
createUIDropDownBinding(GOUIComponent* component);
UIComponentBinding*
createUIEditFieldBinding(GOUIComponent* component);
UIComponentBinding*
createUIGaugeBinding(GOUIComponent* component);
UIComponentBinding*
createUIGridLayoutBinding(GOUIComponent* component);
UIComponentBinding*
createUIKnobBinding(GOUIComponent* component);
UIComponentBinding*
createUILabelBinding(GOUIComponent* component);
UIComponentBinding*
createUILampBinding(GOUIComponent* component);
UIComponentBinding*
createUIListBoxBinding(GOUIComponent* component);
UIComponentBinding*
createUIPanelBinding(GOUIComponent* component);
UIComponentBinding*
createUIRadioButtonBinding(GOUIComponent* component);
UIComponentBinding*
createUISliderBinding(GOUIComponent* component);
UIComponentBinding*
createUISpinnerBinding(GOUIComponent* component);
UIComponentBinding*
createUISwitchBinding(GOUIComponent* component);
UIComponentBinding*
createUITabBinding(GOUIComponent* component);
UIComponentBinding*
createUITabGroupBinding(GOUIComponent* component);
UIComponentBinding*
createUITableBinding(GOUIComponent* component);
UIComponentBinding*
createUITextAreaBinding(GOUIComponent* component);
UIComponentBinding*
createUITreeBinding(GOUIComponent* component);
//=============================================================================
// Factory registered via setUIComponentBindingFactory (nullptr when the
// component type has no widget backend, e.g. uitreenode).
UIComponentBinding*
createUIComponentBinding(GOUIComponent* component);
//=============================================================================
} // namespace Nelson
//=============================================================================
