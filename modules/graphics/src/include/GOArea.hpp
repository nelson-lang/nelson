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
#include "nlsGraphics_exports.h"
#include "GOPatch.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOArea : public GOPatch
{
public:
    GOArea();
    ~GOArea() override;
    std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_AREA_STR;
    }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
