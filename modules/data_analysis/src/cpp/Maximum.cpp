//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// max(...) is the MaxPolicy instantiation of the shared extremum reduction.
// The whole implementation lives in ExtremumReduction.hpp (shared with min).
//=============================================================================
#include "Maximum.hpp"
#include "ExtremumReduction.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Maximum(bool omitNaN, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    return extremumBinary<MaxPolicy>(omitNaN, A, B, needToOverload);
}
//=============================================================================
ArrayOfVector
Maximum(bool omitNaN, const ArrayOf& A, int nLhs, bool& needToOverload)
{
    return extremumVector<MaxPolicy>(omitNaN, A, nLhs, needToOverload);
}
//=============================================================================
ArrayOfVector
Maximum(bool omitNaN, const ArrayOf& A, indexType dim, int nLhs, bool& needToOverload)
{
    return extremumDim<MaxPolicy>(omitNaN, A, dim, nLhs, needToOverload);
}
//=============================================================================
ArrayOf
MaximumAll(bool omitNaN, const ArrayOf& A, bool& needToOverload)
{
    return extremumAll<MaxPolicy>(omitNaN, A, needToOverload);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
