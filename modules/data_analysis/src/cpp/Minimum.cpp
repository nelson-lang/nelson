//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// min(...) is the MinPolicy instantiation of the shared extremum reduction.
// The whole implementation lives in ExtremumReduction.hpp (shared with max).
//=============================================================================
#include "Minimum.hpp"
#include "ExtremumReduction.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Minimum(bool omitNaN, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    return extremumBinary<MinPolicy>(omitNaN, A, B, needToOverload);
}
//=============================================================================
ArrayOfVector
Minimum(bool omitNaN, const ArrayOf& A, int nLhs, bool& needToOverload)
{
    return extremumVector<MinPolicy>(omitNaN, A, nLhs, needToOverload);
}
//=============================================================================
ArrayOfVector
Minimum(bool omitNaN, const ArrayOf& A, indexType dim, int nLhs, bool& needToOverload)
{
    return extremumDim<MinPolicy>(omitNaN, A, dim, nLhs, needToOverload);
}
//=============================================================================
ArrayOf
MinimumAll(bool omitNaN, const ArrayOf& A, bool& needToOverload)
{
    return extremumAll<MinPolicy>(omitNaN, A, needToOverload);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
