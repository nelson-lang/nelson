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
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson::MatioGateway {
// value = __matfile_read__(filename, name [, subscripts])
ArrayOfVector
matfileReadBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
// __matfile_write__(filename, name, value [, subscripts])
ArrayOfVector
matfileWriteBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
// sz = __matfile_size__(filename, name)
ArrayOfVector
matfileSizeBuiltin(int nLhs, const ArrayOfVector& argIn);
} // namespace Nelson::MatioGateway
//=============================================================================
