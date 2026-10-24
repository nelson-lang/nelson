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
#include <unordered_map>
#include <vector>
#include "SimEngineTypes.hpp"
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
// One pass of goto/from label resolution. For every internal label-source block,
// copies the connected label-sink's input slice into the source's output slot(s).
// Returns true when any value moved, which the algebraic solve uses as a
// not-settled signal. External-port label sources are driven from outside and skipped.
bool
resolveLabelSourcesOnce(const std::vector<Block>& blocks,
    const std::unordered_map<std::string, std::string>& labelSinks,
    const std::unordered_map<std::string, BlockId>& nameToId,
    const std::vector<std::vector<int>>& inputIds, const std::vector<int>& outputBase,
    const SignalLayout& sigLayout, std::vector<double>& outputs);
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
