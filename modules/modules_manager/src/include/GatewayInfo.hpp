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
#include "nlsModules_manager_exports.h"
#include "Types.hpp"
//=============================================================================
#define GATEWAY_INFO "GetGatewayInfo"
#define GATEWAY_NAME "GetGatewayName"
//=============================================================================
namespace Nelson {
NLSMODULES_MANAGER_IMPEXP bool
GatewayInfo(const std::wstring& dynlibname, std::wstring& moduleName, stringVector& functionsList,
    std::wstring& errorMessage);
}
//=============================================================================
