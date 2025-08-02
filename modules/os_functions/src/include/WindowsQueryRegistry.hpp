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
#include "ArrayOf.hpp"
#include "nlsOs_functions_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
NLSOS_FUNCTIONS_IMPEXP ArrayOf
windowsQueryRegistry(
    const std::wstring& rootkey, const std::wstring& subkey, std::wstring& errorMessage);

NLSOS_FUNCTIONS_IMPEXP ArrayOf
windowsQueryRegistryAllValuesNames(
    const std::wstring& rootkey, const std::wstring& subkey, std::wstring& errorMessage);

NLSOS_FUNCTIONS_IMPEXP ArrayOf
windowsQueryRegistryValueName(const std::wstring& rootkey, const std::wstring& subkey,
    const std::wstring& valname, std::wstring& errorMessage);
} // namespace Nelson
//=============================================================================
