
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
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson::DataAnalysisGateway {
//=============================================================================
using callback_cumulative_function = ArrayOf (*)(const ArrayOf&, indexType, bool, bool, bool&);
//=============================================================================
ArrayOfVector
cumulativeFunctionBuiltin(int nLhs, const ArrayOfVector& argIn, std::string cumulativeFunctionName,
    callback_cumulative_function pFunction);
//=============================================================================
}
//=============================================================================
