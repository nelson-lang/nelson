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
#include <vector>
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson::DataAnalysisGateway {
//=============================================================================
// options of sum(A, dim | vecdim | 'all', outtype, nanflag): the dimension comes
// right after A, the flags may be given in any order after it
struct SumProdOptions
{
    indexType dim = 0; // 0: first non-singleton dimension, -1: 'all'
    std::vector<indexType> vecDims;
    std::wstring outType = L"default";
    bool withNaN = true;
};
//=============================================================================
SumProdOptions
parseSumProdOptions(const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson::DataAnalysisGateway
//=============================================================================
