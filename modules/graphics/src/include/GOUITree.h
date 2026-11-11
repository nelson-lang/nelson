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
#include "GOUIContainer.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// uitree: tree container ('tree' style) and checkbox tree container
// ('checkbox' style).
// Children are uitreenode objects mapped to native tree entries owned by the
// widget binding.
class NLSGRAPHICS_IMPEXP GOUITree : public GOUIContainer
{
    bool checkboxStyle;
    // true while applyState pushes the node expanded states to the widget
    bool pendingExpandSync = false;

public:
    //=============================================================================
    explicit GOUITree(bool isCheckboxTree = false);
    ~GOUITree() override = default;
    //=============================================================================
    std::wstring
    getType() override;
    //=============================================================================
    std::string
    getClassName() override;
    //=============================================================================
    bool
    isCheckboxTree() const
    {
        return checkboxStyle;
    }
    //=============================================================================
    // rebuild the native tree from the tree node hierarchy (through the
    // widget binding)
    void
    rebuildTree();
    //=============================================================================
    // push the node expanded states to the widget and repaint the figure
    void
    syncExpandedState();
    //=============================================================================
    bool
    isExpandSyncPending() const
    {
        return pendingExpandSync;
    }
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
