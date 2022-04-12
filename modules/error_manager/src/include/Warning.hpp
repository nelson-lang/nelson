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
#include <string>
#include "nlsError_manager_exports.h"
#include "WarningIds.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSERROR_MANAGER_IMPEXP void
Warning(const std::wstring& id, const std::wstring& message);
//=============================================================================
NLSERROR_MANAGER_IMPEXP void
Warning(const std::wstring& message);
//=============================================================================
NLSERROR_MANAGER_IMPEXP void
Warning(const std::string& id, const std::string& message);
//=============================================================================
NLSERROR_MANAGER_IMPEXP void
Warning(const std::string& message);
//=============================================================================
} // namespace Nelson
//=============================================================================
