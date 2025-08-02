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
#include "HandleGenericObject.hpp"
#include "Types.hpp"
#include "nlsCom_engine_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
NLSCOM_ENGINE_IMPEXP std::wstring
xlsIndexToRange(indexType m, indexType n);
NLSCOM_ENGINE_IMPEXP bool
isValidRange(const std::wstring& range);
} // namespace Nelson
//=============================================================================
