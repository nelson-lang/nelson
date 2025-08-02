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
#include "Sort.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
sortString(const ArrayOf& arrayIn, bool withIndex, indexType linesize, indexType planecount,
    indexType planesize, Dimensions& outDim, indexType dim, MISSING_PLACEMENT placement,
    bool ascend);
//=============================================================================
}
//=============================================================================
