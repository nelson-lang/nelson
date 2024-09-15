//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
class GOUnitsProperty : public GORestrictedStringProperty
{
private:
    std::wstring previousUnits = GO_PROPERTY_VALUE_NORMALIZED_STR;

public:
    GOUnitsProperty();
    ~GOUnitsProperty() override = default;
    void
    set(ArrayOf arg) override;
    std::wstring
    getPreviousUnits();
};
//=============================================================================
};
//=============================================================================
