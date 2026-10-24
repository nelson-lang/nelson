//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// Copyright (c) 2002, 2003 Samit Basu
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
#include <array>
#include "Types.hpp"
#include "nlsTypes_exports.h"
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4290)
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
#define MAXDIMS 16
const unsigned int maxDims = MAXDIMS;
//=============================================================================
/**
 * Fixed-capacity shape descriptor used by ArrayOf: up to MAXDIMS lengths, only
 * the first length_ entries are active. simplify() drops trailing singleton
 * dimensions after the second one.
 */
//=============================================================================
class NLSTYPES_IMPEXP Dimensions
{
private:
    //=============================================================================
    std::array<indexType, MAXDIMS> data;
    //=============================================================================
    indexType length_ { 0 };
    //=============================================================================
public:
    //=============================================================================
    Dimensions();
    //=============================================================================
    Dimensions(indexType dimCount);
    //=============================================================================
    Dimensions(indexType nrows, indexType ncols);
    //=============================================================================
    Dimensions(const std::vector<indexType>& dimsVector);
    //=============================================================================
    /**
     * Mutable zero-based dimension length. Access beyond the active length
     * extends the descriptor and fills skipped dimensions with 1.
     */
    indexType&
    operator[](indexType i);
    //=============================================================================
    /**
     * Zero-based dimension length that never modifies the descriptor. With
     * checkLength, the index must be inside the active length.
     */
    indexType
    at(indexType i, bool checkLength = true) const;
    //=============================================================================
    std::vector<indexType>
    asVector();
    //=============================================================================
    [[nodiscard]] indexType
    length() const;
    //=============================================================================
    [[nodiscard]] indexType
    elementCount() const;
    //=============================================================================
    /**
     * Linear column-major index of a zero-based point. The point must not have
     * more dimensions than this shape and each coordinate must be in range.
     */
    indexType
    mapPoint(const Dimensions& point);
    //=============================================================================
    [[nodiscard]] indexType
    rows() const;
    //=============================================================================
    [[nodiscard]] indexType
    columns() const;
    //=============================================================================
    /**
     * Dimension length without growing the descriptor; missing higher
     * dimensions have length 1.
     */
    [[nodiscard]] indexType
    dimensionLength(indexType arg) const;
    //=============================================================================
    /**
     * Effective indexing shape for subscriptCount subscripts. With fewer
     * subscripts than active dimensions, the last one folds the product of the
     * trailing dimensions; otherwise missing higher dimensions have length 1.
     */
    [[nodiscard]] Dimensions
    foldForSubscriptCount(indexType subscriptCount) const;
    //=============================================================================
    void
    setDimensionLength(indexType dim, indexType len);
    /**
     * Grow each dimension to the maximum of its length here and in a.
     */
    void
    expandToCover(const Dimensions& a);
    //=============================================================================
    /**
     * Increment a multi-dimensional counter with carry, wrapping against limit
     * and starting at dimension ordinal.
     */
    void
    incrementModulo(const Dimensions& limit, int ordinal);
    //=============================================================================
    bool
    inside(const Dimensions& limit);
    //=============================================================================
    [[nodiscard]] bool
    equals(const Dimensions& alt) const;
    //=============================================================================
    void
    simplify();
    //=============================================================================
    [[nodiscard]] std::string
    toString() const;

    [[nodiscard]] std::wstring
    toWideString() const;
    //=============================================================================
    void
    reset();
    //=============================================================================
    void
    zeroOut();
    //=============================================================================
    void
    makeScalar();
    //=============================================================================
    indexType
    maximum();
    //=============================================================================
    [[nodiscard]] bool
    isScalar() const;
    //=============================================================================
    [[nodiscard]] bool
    isVector() const;
    //=============================================================================
    [[nodiscard]] bool
    isRowVector() const;
    //=============================================================================
    [[nodiscard]] bool
    isColumnVector() const;
    //=============================================================================
    [[nodiscard]] bool
    is2D() const;
    //=============================================================================
    [[nodiscard]] bool
    isSquare() const;
    //=============================================================================
    [[nodiscard]] bool
    isEmpty(bool allDimensionsIsZero) const;
    //=============================================================================
    [[nodiscard]] Dimensions
    permute(const std::vector<indexType>& permutationVector, bool emptyDimensions) const;
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
