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
namespace Nelson::Hdf5Gateway {
// value = __nh5file_read__(filename, name [, subscripts])
ArrayOfVector
nh5fileReadBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
// __nh5file_write__(filename, name, value [, subscripts])
ArrayOfVector
nh5fileWriteBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
// sz = __nh5file_size__(filename, name)
ArrayOfVector
nh5fileSizeBuiltin(int nLhs, const ArrayOfVector& argIn);
} // namespace Nelson::Hdf5Gateway
//=============================================================================
