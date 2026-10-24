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
class NLSGRAPHICS_IMPEXP GOUITable : public GOUIComponent
{
public:
    //=============================================================================
    GOUITable();
    ~GOUITable() override = default;
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
    // apply a user cell edit onto Data and fire CellEditCallback (shared by
    // the widget binding and the web path). Returns false when the edited
    // text was rejected: the caller must then refresh its display from Data.
    bool
    onUserCellEdited(int row, int column, const std::wstring& newText);
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
