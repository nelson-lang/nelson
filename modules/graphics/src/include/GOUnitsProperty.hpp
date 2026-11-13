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
#include "nlsGraphics_exports.h"
#include <cstdint>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOUnitsProperty : public GORestrictedStringProperty
{
private:
    std::wstring previousUnits = GO_PROPERTY_VALUE_NORMALIZED_STR;
    uint64_t setOrder = 0;

public:
    GOUnitsProperty();
    ~GOUnitsProperty() override = default;
    void
    set(ArrayOf arg) override;
    std::wstring
    getPreviousUnits();
    // Sets the units with nothing to convert from.
    void
    resetUnits(const std::wstring& units);
    // Rank of the last set, compared with the position rank to know the given order.
    uint64_t
    getSetOrder();
};
//=============================================================================
// Next rank of a units or position set, increasing over the session.
NLSGRAPHICS_IMPEXP uint64_t
nextUnitsPositionSetOrder();
//=============================================================================
} // namespace Nelson
//=============================================================================
