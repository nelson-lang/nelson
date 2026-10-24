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
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOOnOffProperty : public GORestrictedStringProperty
{
public:
    GOOnOffProperty();
    ~GOOnOffProperty() override = default;
    bool
    asBool();
    void set(ArrayOf) override;
};
//=============================================================================
class GOHandleVisibilityProperty : public GORestrictedStringProperty
{
public:
    GOHandleVisibilityProperty();
    ~GOHandleVisibilityProperty() override = default;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
