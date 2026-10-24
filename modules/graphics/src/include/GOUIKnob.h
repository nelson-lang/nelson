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
class NLSGRAPHICS_IMPEXP GOUIKnob : public GOUIComponent
{
    bool discreteStyle;

public:
    //=============================================================================
    explicit GOUIKnob(bool isDiscrete = false);
    ~GOUIKnob() override = default;
    //=============================================================================
    std::wstring
    getType() override;
    //=============================================================================
    std::string
    getClassName() override;
    //=============================================================================
    void
    updateState() override;
    //=============================================================================
    bool
    isDiscrete() const
    {
        return discreteStyle;
    }
    //=============================================================================
    // validate Value against Items/ItemsData and refresh ValueIndex; returns
    // the 1-based index (0 when Items is empty). Public: the widget binding
    // syncs the knob display from it.
    int
    syncDiscreteValueIndex();
    //=============================================================================
    void
    onUserValueChanging(double value);
    void
    onUserValueChanged(double value);
    void
    onUserDiscreteChanged(int index);
    //=============================================================================
private:
    //=============================================================================
    void
    constructProperties();
    void
    setupDefaults();
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
