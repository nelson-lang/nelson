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
#include <string>
#include "nlsError_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSERROR_MANAGER_IMPEXP void
Error(const std::wstring& msg, const std::wstring& id = L"", bool asCaller = false) noexcept(false);
//=============================================================================
NLSERROR_MANAGER_IMPEXP void
Error(const std::string& msg, const std::string& id = "", bool asCaller = false) noexcept(false);
//=============================================================================
} // namespace Nelson
//=============================================================================
