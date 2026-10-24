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
#include "GOUIComponent.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// uislider: slider control ('slider' style) and range slider control
// ('range' style)
class NLSGRAPHICS_IMPEXP GOUISlider : public GOUIComponent
{
    bool rangeStyle;

public:
    //=============================================================================
    explicit GOUISlider(bool isRangeSlider = false);
    ~GOUISlider() override = default;
    //=============================================================================
    void
    updateState() override;
    //=============================================================================
    std::wstring
    getType() override;
    //=============================================================================
    std::string
    getClassName() override;
    //=============================================================================
    bool
    isRangeSlider() const
    {
        return rangeStyle;
    }
    //=============================================================================
    // recompute the automatic tick properties from Limits. Public: the widget
    // binding refreshes them before syncing the slider display.
    void
    updateAutoTicks();
    //=============================================================================
    // called by the widget during/after user interaction
    void
    onUserValueChanging(double value, double highValue);
    void
    onUserValueChanged(double value, double highValue);
    //=============================================================================
private:
    //=============================================================================
    void
    constructProperties();
    void
    setupDefaults();
    // A slider's thickness is fixed: the height of a horizontal slider and the width
    // of a vertical one stay at 3. Setting another value warns and resets it.
    void
    enforceFixedThickness();
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
