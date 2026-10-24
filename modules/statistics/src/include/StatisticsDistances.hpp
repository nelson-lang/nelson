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
#include "nlsStatistics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class Evaluator;
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
PDistance(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOfVector
PDist2(Evaluator* eval, const ArrayOfVector& args, int nLhs);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
SquareForm(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOfVector
KNNSearch(const ArrayOfVector& args, int nLhs);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOfVector
RangeSearch(const ArrayOfVector& args, int nLhs);
//=============================================================================
} // namespace Nelson
//=============================================================================
