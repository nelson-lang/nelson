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
#include "GOScalarDoubleProperty.hpp"
#include <cstdint>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class GOFontUnitsProperty : public GORestrictedStringProperty
{
private:
    std::wstring pendingFrom;
    uint64_t setOrder = 0;

public:
    GOFontUnitsProperty();
    ~GOFontUnitsProperty() override = default;
    void
    set(ArrayOf arg) override;
    // Units before the last user change not yet applied; empty when none.
    std::wstring
    takePendingFrom();
    uint64_t
    getSetOrder();
};
//=============================================================================
// FontSize that records when the user set it, to compare with FontUnits.
class GOFontSizeProperty : public GOScalarProperty
{
private:
    uint64_t setOrder = 0;

public:
    GOFontSizeProperty() = default;
    ~GOFontSizeProperty() override = default;
    void
    set(ArrayOf arg) override;
    uint64_t
    getSetOrder();
};
class GraphicsObject;
//=============================================================================
// Font size from one font unit to another; normalized is relative to a height in pixels.
double
convertFontSizeUnits(
    double size, const std::wstring& from, const std::wstring& to, double heightInPixels);
//=============================================================================
// Height in pixels that normalized font units of an axes or of its texts refer to.
double
fontUnitsReferenceHeight(GraphicsObject* object);
//=============================================================================
// FontSize of an object in points, whatever its FontUnits.
double
fontSizeInPoints(GraphicsObject* object);
//=============================================================================
//=============================================================================
} // namespace Nelson
//=============================================================================
