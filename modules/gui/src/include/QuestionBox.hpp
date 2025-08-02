//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//===================================================================================
#include <string>
#include "nlsGui_exports.h"
#include "ArrayOf.hpp"
//===================================================================================
namespace Nelson {
//===================================================================================
NLSGUI_IMPEXP ArrayOf
QuestionBox(const std::wstring& title, const std::wstring& question, const std::wstring& button1,
    const std::wstring& button2, const std::wstring& button3, const std::wstring& defaultButton,
    int numberOfButtons);
//===================================================================================
}
//===================================================================================
