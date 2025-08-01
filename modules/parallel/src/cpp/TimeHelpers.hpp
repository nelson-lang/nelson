//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string>
#include "Types.hpp"
//===================================<==========================================
#pragma once
//=============================================================================
namespace Nelson {
//=============================================================================
uint64
getEpoch();
//=============================================================================
std::wstring
epochToDateString(uint64 epoch);
//=============================================================================
std::wstring
milliSecondsToDHMSMsString(uint64 n);
//=============================================================================
}
//=============================================================================
