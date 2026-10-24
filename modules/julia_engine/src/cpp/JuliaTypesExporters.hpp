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
#include "JuliaLibraryWrapper.hpp"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
jl_value_tToArrayOf(jl_value_t* value);
//=============================================================================
ArrayOf
jl_value_tGetProperty(jl_value_t* value, const std::string& propertyName, bool& wasFound);
//=============================================================================
ArrayOf
convertSparseMatrixComplexFloat64ToArrayOf(jl_value_t* value, bool& wasConverted);
//=============================================================================
ArrayOf
convertSparseMatrixFloat64ToArrayOf(jl_value_t* value, bool& wasConverted);
//=============================================================================
ArrayOf
convertSparseMatrixFloat32ToArrayOf(jl_value_t* value, bool& wasConverted);
//=============================================================================
ArrayOf
convertSparseMatrixBoolToArrayOf(jl_value_t* value, bool& wasConverted);
//=============================================================================
ArrayOf
convertMatrixComplexFloat64ToArrayOf(jl_value_t* value, bool& wasConverted);
//=============================================================================
ArrayOf
convertMatrixComplexFloat32ToArrayOf(jl_value_t* value, bool& wasConverted);
//=============================================================================
ArrayOf
convertMatrixFloat64ToArrayOf(jl_value_t* value, bool& wasConverted);
//=============================================================================
ArrayOf
convertMatrixFloat32ToArrayOf(jl_value_t* value, bool& wasConverted);
//=============================================================================
ArrayOf
convertMatrixUInt8ToArrayOf(jl_value_t* value, bool& wasConverted);
//=============================================================================
ArrayOf
convertMatrixUInt16ToArrayOf(jl_value_t* value, bool& wasConverted);
//=============================================================================
ArrayOf
convertMatrixUInt32ToArrayOf(jl_value_t* value, bool& wasConverted);
//=============================================================================
ArrayOf
convertMatrixUInt64ToArrayOf(jl_value_t* value, bool& wasConverted);
//=============================================================================
ArrayOf
convertMatrixInt8ToArrayOf(jl_value_t* value, bool& wasConverted);
//=============================================================================
ArrayOf
convertMatrixInt16ToArrayOf(jl_value_t* value, bool& wasConverted);
//=============================================================================
ArrayOf
convertMatrixInt32ToArrayOf(jl_value_t* value, bool& wasConverted);
//=============================================================================
ArrayOf
convertMatrixInt64ToArrayOf(jl_value_t* value, bool& wasConverted);
//=============================================================================
ArrayOf
convertMatrixStringToArrayOf(jl_value_t* value, bool& wasConverted);
//=============================================================================
ArrayOf
convertCellToArrayOf(jl_value_t* value, bool& wasConverted);
//=============================================================================
// Converts a DataFrames.DataFrame into the pieces needed to build a Nelson
// table: a 1-by-N cell of columns (numeric columns keep their numeric type,
// textual and other columns become string arrays), a 1-by-N cellstr of the
// column labels, and a cellstr of row names (always empty because a DataFrame
// has no row-name concept). Returns false when the value is not a DataFrame or
// when the DataFrames package cannot be loaded.
bool
JuliaDataFrameToTableData(
    jl_value_t* value, ArrayOf& columns, ArrayOf& variableNames, ArrayOf& rowNames);
//=============================================================================
} // namespace Nelson
//=============================================================================
