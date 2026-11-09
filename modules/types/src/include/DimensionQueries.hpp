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
#include <algorithm>
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// length(x): 0 for an empty array, else its largest dimension
inline double
arrayLength(const ArrayOf& value)
{
    if (value.isEmpty()) {
        return 0.;
    }
    const Dimensions dims = value.dimensions();
    indexType longest = 0;
    for (indexType k = 0; k < dims.length(); ++k) {
        longest = std::max(longest, dims.at(k));
    }
    return static_cast<double>(longest);
}
//=============================================================================
// ndims(x): the dimension count, at least 2
inline double
arrayDimensionCount(const ArrayOf& value)
{
    return std::max(2., static_cast<double>(value.nDims()));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
