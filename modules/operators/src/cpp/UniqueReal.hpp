//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Unique.hpp"
#include "nlsBuildConfig.h"
#include "StringHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T> class UniqueEntryReal
{
public:
    ompIndexType n;
    T v;
};
//=============================================================================
template <class T>
bool
operator<(const UniqueEntryReal<T>& a, const UniqueEntryReal<T>& b)
{
    if (std::isnan(b.v) && !std::isnan(a.v)) {
        return true;
    }
    return a.v < b.v;
}
//=============================================================================
template <class T>
bool
operator==(const UniqueEntryReal<T>& a, const UniqueEntryReal<T>& b)
{
    return a.v == b.v;
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueRealOneLhs(const ArrayOf& input)
{
    bool isRowVector = input.isRowVector();
    ompIndexType len = input.elementCount();
    const T* dp = static_cast<const T*>(input.dataPointer());
    std::vector<UniqueEntryReal<T>> values(len);

#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
        values[k].v = dp[k];
    }
    std::sort(values.begin(), values.end());
    values.erase(std::unique(values.begin(), values.end()), values.end());

    ArrayOfVector retval;
    NelsonType cls(input.dataClass());
    T* op = (T*)ArrayOf::allocateArrayOf(cls, values.size());
#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)values.size(); k++) {
        op[k] = values[k].v;
    }
    Dimensions dimsOut = isRowVector ? Dimensions(1, values.size()) : Dimensions(values.size(), 1);
    retval << ArrayOf(cls, dimsOut, op);
    return retval;
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueRealTwoLhs(const ArrayOf& input)
{
    return {};
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueRealThreeLhs(const ArrayOf& input)
{
    return {};
}
//=============================================================================
template <class T>
ArrayOfVector
UniqueReal(const ArrayOf& input, int nLhs)
{
    if (nLhs <= 1) {
        return UniqueRealOneLhs<T>(input);
    }
    if (nLhs == 2) {
        return UniqueRealTwoLhs<T>(input);
    }
    return UniqueRealThreeLhs<T>(input);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
