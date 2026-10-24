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
#include "GORestrictedStringProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Three-state Enable property: 'on', 'off' or 'inactive'.
// 'inactive' shows the control looking normal (not greyed) but not
// editable/clickable, while ButtonDownFcn still fires.
class GOEnableProperty : public GORestrictedStringProperty
{
public:
    GOEnableProperty()
        : GORestrictedStringProperty(std::vector<std::wstring> {
              GO_PROPERTY_VALUE_ON_STR, GO_PROPERTY_VALUE_OFF_STR, GO_PROPERTY_VALUE_INACTIVE_STR })
    {
    }
    ~GOEnableProperty() override = default;
    bool
    isInactive()
    {
        return isEqual(GO_PROPERTY_VALUE_INACTIVE_STR);
    }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
