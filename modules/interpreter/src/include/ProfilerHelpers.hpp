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
#include <tuple>
#include <vector>
#include <string>
#include <Types.hpp>
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
using profileParent = std::tuple<std::string, uint64>;
//=============================================================================
using profileParentStack = std::vector<profileParent>;
//=============================================================================
using internalProfileFunction = std::tuple<profileParentStack, std::string, std::wstring, bool>;
//=============================================================================
using profileFunction = std::tuple<internalProfileFunction, uint64, uint64, uint64>;
//=============================================================================
internalProfileFunction
computeProfileStack(Evaluator* eval, const std::string& currentFunctionName,
    const std::wstring& currentFilename, bool isBuiltin = true);
//=============================================================================
} // namespace Nelson
//=============================================================================
