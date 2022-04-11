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
#include "nlsDynamic_link_exports.h"
#include "Types.hpp"
//=============================================================================
#define GATEWAY_INFO "GetGatewayInfo"
#define GATEWAY_NAME "GetGatewayName"
//=============================================================================
namespace Nelson {
NLSDYNAMIC_LINK_IMPEXP bool
GatewayInfo(const std::wstring& dynlibname, std::wstring& moduleName, stringVector& functionsList,
    std::wstring& errorMessage);
}
//=============================================================================
