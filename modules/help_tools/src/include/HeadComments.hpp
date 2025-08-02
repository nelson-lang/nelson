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
#include "Evaluator.hpp"
#include "Types.hpp"
#include "nlsHelp_tools_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
enum HEADCOMMENTS_ERROR
{
    MACRO_OK,
    NOT_A_MACRO,
    FILE_NOT_EXIST,
};
//=============================================================================
NLSHELP_TOOLS_IMPEXP wstringVector
HeadComments(Evaluator* eval, const std::wstring& filename, HEADCOMMENTS_ERROR& headError);
//=============================================================================
} // namespace Nelson
//=============================================================================
