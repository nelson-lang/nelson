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
/**
 * mean operation.
 */
//=============================================================================
enum MEAN_OUT_TYPE
{
    DEFAULT,
    NATIVE,
    DOUBLE
};
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
MeanAll(const ArrayOf& A, bool omitNaN, MEAN_OUT_TYPE outType, bool& needToOverload);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
Mean(const ArrayOf& A, indexType dim, bool omitNaN, MEAN_OUT_TYPE outType, bool& needToOverload);
//=============================================================================
} // namespace Nelson
//=============================================================================
