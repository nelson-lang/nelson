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
#include "GOSurface.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOFunctionSurface : public GOSurface
{
public:
    GOFunctionSurface();
    ~GOFunctionSurface() override;
    std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_FUNCTION_SURFACE_STR;
    }
    void
    updateState() override;
    void
    paintMe(RenderInterface& gc) override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
