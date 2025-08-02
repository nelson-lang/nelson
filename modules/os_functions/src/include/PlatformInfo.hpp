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
//=============================================================================
namespace Nelson {
NLSOS_FUNCTIONS_IMPEXP ArrayOf
IsPc();
NLSOS_FUNCTIONS_IMPEXP ArrayOf
IsMac();
NLSOS_FUNCTIONS_IMPEXP ArrayOf
IsUnix();
NLSOS_FUNCTIONS_IMPEXP bool
IsPcPlatform();
NLSOS_FUNCTIONS_IMPEXP bool
IsMacPlatform();
NLSOS_FUNCTIONS_IMPEXP bool
IsUnixPlatform();
NLSOS_FUNCTIONS_IMPEXP std::wstring
GetArchitecture();
NLSOS_FUNCTIONS_IMPEXP std::wstring
GetArchitectureType();
NLSOS_FUNCTIONS_IMPEXP double
GetMaxArrayOfSizeSupported();
NLSOS_FUNCTIONS_IMPEXP bool
IsBigEndian();
} // namespace Nelson
//=============================================================================
