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
#include "JuliaLibraryWrapper.hpp"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
jl_can_be_converted(const std::string& jlTypeStr1, const std::string& jlTypeStr2);
//=============================================================================
std::string
jl_value_tRepresentation(jl_value_t* value, bool& fails, int nbRows, int nbColumns);
//=============================================================================
std::string
jl_get_type_of_as_string(jl_value_t* value);
//=============================================================================
stringVector
jl_value_tfieldnames(jl_value_t* value);
//=============================================================================
stringVector
jl_value_tgetPropertyNames(jl_value_t* value);
//=============================================================================
stringVector
jl_value_tgetMethodNames(jl_value_t* value);
//=============================================================================
bool
isModule(jl_value_t* value);
//=============================================================================
bool
isJuliaDataFrame(jl_value_t* value);
//=============================================================================
// Imports DataFrames (once per call, cheap after the first) and defines the
// small Main helper functions used to build and read a DataFrame. Returns
// false when the DataFrames package is not available in the Julia environment.
bool
jl_ensure_dataframes();
//=============================================================================
} // namespace Nelson
//=============================================================================
