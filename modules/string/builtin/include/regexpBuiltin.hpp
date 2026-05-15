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
#include "Evaluator.hpp"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson::StringGateway {
//=============================================================================
ArrayOfVector
regexpBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
regexpiBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson::StringGateway
//=============================================================================
