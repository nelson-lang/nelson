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
#include "nlsStatistics_builtin_exports.h"
#include "Evaluator.hpp"
#include "Types.hpp"
//=============================================================================
namespace Nelson::StatisticsGateway {
//=============================================================================
NLSSTATISTICS_BUILTIN_IMPEXP ArrayOfVector
nbinfitBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson::StatisticsGateway
//=============================================================================
