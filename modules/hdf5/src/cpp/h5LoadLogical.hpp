//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#define H5_BUILT_AS_DYNAMIC_LIB
#include <hdf5.h>
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5LoadLogical(hid_t fid, const std::string& location, const std::string& variableName, bool isEmpty,
    const Dimensions& dims, bool isSparse, uint64 nzmax, ArrayOf& VariableValue);
//=============================================================================
};
//=============================================================================
