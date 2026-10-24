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
#ifndef H5_BUILT_AS_DYNAMIC_LIB
#define H5_BUILT_AS_DYNAMIC_LIB
#endif
#include <hdf5.h>
#include "ArrayOf.hpp"
#include "nlsHdf5_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class Evaluator;
//=============================================================================
NLSHDF5_IMPEXP bool
h5LoadVariable(Evaluator* eval, hid_t fid, const std::string& location,
    const std::string& variableName, ArrayOf& VariableValue);
//=============================================================================
} // namespace Nelson
//=============================================================================
