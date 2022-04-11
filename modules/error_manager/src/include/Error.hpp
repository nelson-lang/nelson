//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsError_manager_exports.h"
#include "Messages.hpp"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
NLSERROR_MANAGER_IMPEXP void
Error(const std::wstring& msg, const std::wstring& id = L"", bool asCaller = false);
NLSERROR_MANAGER_IMPEXP void
Error(const std::string& msg, const std::string& id = "", bool asCaller = false);
//=============================================================================
} // namespace Nelson
//=============================================================================
