//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#include <Windows.h>
#endif
#include "WinOpen.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
WinOpen(const std::wstring& command)
{
#ifdef _MSC_VER
    HINSTANCE error
        = ShellExecuteW(nullptr, L"open", command.c_str(), nullptr, nullptr, SW_SHOWNORMAL);
    return error > (HINSTANCE)32;
#endif
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
