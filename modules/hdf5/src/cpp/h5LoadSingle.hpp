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
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5LoadSingle(hid_t fid, const std::string& location, const std::string& variableName, bool isEmpty,
    bool isComplex, const Dimensions& dims, ArrayOf& VariableValue);
//=============================================================================
};
//=============================================================================
