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
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "nlsHdf5_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// indices == nullptr reads or writes the whole variable
NLSHDF5_IMPEXP ArrayOf
Nh5FileRead(Evaluator* eval, const std::wstring& filename, const std::string& variableName,
    const ArrayOfVector* indices);
//=============================================================================
NLSHDF5_IMPEXP void
Nh5FileWrite(Evaluator* eval, const std::wstring& filename, const std::string& variableName,
    const ArrayOfVector* indices, const ArrayOf& value);
//=============================================================================
NLSHDF5_IMPEXP ArrayOf
Nh5FileVariableSize(const std::wstring& filename, const std::string& variableName);
//=============================================================================
} // namespace Nelson
//=============================================================================
