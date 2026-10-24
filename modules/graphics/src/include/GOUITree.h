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
    // pending expandAll request applied by the widget binding while
    // applyState runs: 1 expand, -1 collapse, 0 none
    int pendingExpandCollapse = 0;

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
    void
    expandAll(bool expand);
    //=============================================================================
    int
    getPendingExpandCollapse() const
    {
        return pendingExpandCollapse;
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
