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
#include "nlsTypes_exports.h"
//=============================================================================
namespace Nelson {
NLSTYPES_IMPEXP std::string
MakeValidFieldname(const std::string& fieldname, const std::string& defaultPrefix = "x");
NLSTYPES_IMPEXP std::wstring
MakeValidFieldname(const std::wstring& fieldname, const std::wstring& defaultPrefix = L"x");
} // namespace Nelson
//=============================================================================
