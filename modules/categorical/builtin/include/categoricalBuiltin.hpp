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
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson::CategoricalGateway {
ArrayOfVector
categorical_codes_compareBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
categorical_codes_countBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
categorical_codes_isundefinedBuiltin(int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
categorical_cellstr_codesBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson::CategoricalGateway
//=============================================================================
