//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "GOGroup.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
class NLSGRAPHICS_IMPEXP GOBubbleCloud : public GOGroup
{
private:
    void
    syncLegendTitle();

public:
    GOBubbleCloud();
    ~GOBubbleCloud() override = default;
    std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_BUBBLE_CLOUD_STR;
    }
    void
    updateState() override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
