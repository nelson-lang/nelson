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
#include "nlsBuildConfig.h"
#include <algorithm>
#include <vector>
#if WITH_TBB and defined(__cpp_lib_execution)
#undef emit
#include <execution>
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename InputIterator, typename OutputIterator, typename UnaryOperation>
OutputIterator
parallelTransform(InputIterator first, InputIterator last, OutputIterator result, UnaryOperation op)
{
#if WITH_TBB and defined(__cpp_lib_execution)
    return std::transform(std::execution::par, first, last, result, op);
#else
    return std::transform(first, last, result, op);
#endif
}
//=============================================================================
}
//=============================================================================
