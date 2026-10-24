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
#include "Types.hpp"
#include "nlsTypes_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Unknown names raise the usual error.
NLSTYPES_IMPEXP NelsonType
StringToClass(const std::string& classname);
//=============================================================================
// haveError is set when the name cannot be mapped to a known class.
NLSTYPES_IMPEXP NelsonType
StringToClass(const std::string& classname, bool& haveError);
//=============================================================================
} // namespace Nelson
//=============================================================================
