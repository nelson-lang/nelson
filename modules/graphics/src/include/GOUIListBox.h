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
class NLSGRAPHICS_IMPEXP GOUIListBox : public GOUIComponent
{
public:
    //=============================================================================
    GOUIListBox();
    ~GOUIListBox() override = default;
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
    // Keeps Value/ValueIndex consistent with Items/ItemsData. Called by the model and
    // by the widget binding before it refreshes the list widget.
    void
    synchronizeValueWithItems();
    //=============================================================================
    // 1-based selection indices decoded from ValueIndex (used by the widget
    // binding to mirror the model selection)
    std::vector<int>
    currentIndicesFromProperties();
    //=============================================================================
private:
    //=============================================================================
    ArrayOf lastValidValue;
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
