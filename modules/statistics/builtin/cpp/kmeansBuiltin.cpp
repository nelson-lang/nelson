//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "kmeansBuiltin.hpp"
#include "Kmeans.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StatisticsGateway::kmeansBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return Kmeans(eval, nLhs, argIn);
}
//=============================================================================
