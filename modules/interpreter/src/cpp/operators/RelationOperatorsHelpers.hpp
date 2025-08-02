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
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
relationalOperator(Evaluator* eval, const std::string& operatorName, const std::string& symbolName,
    bool compareAlsoImagPart, const ArrayOfVector& args,
    ArrayOf (*relationalOperator)(const ArrayOf& A, const ArrayOf& B, bool& needToOverload));
//=============================================================================
}
//=============================================================================
