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
template <typename T, typename Compare = std::less<T>>
void
parallelSort(std::vector<T>& values, Compare comp = Compare())
{
#if WITH_TBB and defined(__cpp_lib_execution)
    std::sort(std::execution::par, values.begin(), values.end(), comp);
#else
    std::sort(values.begin(), values.end(), comp);
#endif
}
//=============================================================================
template <typename IteratorType,
    typename Compare = std::less<typename std::iterator_traits<IteratorType>::value_type>>
void
parallelSort(IteratorType valuesBegin, IteratorType valuesEnd, Compare comp = Compare())
{
#if WITH_TBB and defined(__cpp_lib_execution)
    std::sort(std::execution::par, valuesBegin, valuesEnd, comp);
#else
    std::sort(valuesBegin, valuesEnd, comp);
#endif
}
//=============================================================================
}
//=============================================================================
