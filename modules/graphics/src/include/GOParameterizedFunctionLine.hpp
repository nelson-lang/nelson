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
#include "GOLineSeries.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOParameterizedFunctionLine : public GOLineSeries
{
public:
    GOParameterizedFunctionLine();
    ~GOParameterizedFunctionLine() override;
    std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_PARAMETERIZED_FUNCTION_LINE_STR;
    }
    void
    updateState() override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
