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
#include "GOContour.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOFunctionContour : public GOContour
{
public:
    GOFunctionContour();
    ~GOFunctionContour() override;
    std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_FUNCTION_CONTOUR_STR;
    }
    void
    updateState() override;
    std::vector<double>
    getLimits() override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
