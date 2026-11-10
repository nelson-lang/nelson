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
#include "nlsMatio_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
class Evaluator;
// matfile object back end: whole or indexed read of one variable (indices == nullptr: whole)
NLSMATIO_IMPEXP ArrayOf
MatFileRead(Evaluator* eval, const std::wstring& filename, const std::string& variableName,
    const ArrayOfVector* indices);
// whole or indexed write of one variable, the file is created as v7.3 when missing
NLSMATIO_IMPEXP void
MatFileWrite(Evaluator* eval, const std::wstring& filename, const std::string& variableName,
    const ArrayOfVector* indices, const ArrayOf& value);
// size of one variable without loading its data
NLSMATIO_IMPEXP ArrayOf
MatFileVariableSize(const std::wstring& filename, const std::string& variableName);
} // namespace Nelson
//=============================================================================
