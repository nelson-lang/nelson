//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// Copyright (c) 2002, 2003 Samit Basu
//=============================================================================
// This file is part of the Nelson.
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
 * The Dimensions class is used to keep track of the number of
 * dimensions for a given ArrayOf.  The Dimensions class represents
 * an N-tuple of integers (each non-negative).  N can be made
 * arbitrarily large.  The '[]' operator is overloaded to return
 * a reference to the dimension of interest.  Indexing beyond
 * the number of current dimensions automatically increases the
 * length of the dimension vector, and pads it with 1's.  For example
 * if our current contents are $$[3,5,2,1]$$, and we index element
 * 5, the dimension vector is automatically padded to $$[3,5,2,1,1,1]$$.
 * Calling simplify() removes the trailing '1's.
 */
//=============================================================================
class NLSTYPES_IMPEXP Dimensions
{
private:
    //=============================================================================
    /**
     * The data array of dimensions.
     */
    std::array<indexType, MAXDIMS> data;
    //=============================================================================
    /**
     * The number of dimensions currently allocated.
     */
    indexType length { 0 };
    //=============================================================================
public:
    //=============================================================================
    /**
     * The default constructor - length is set to zero.
     */
    Dimensions();
    //=============================================================================
    /**
     * Initialize the object with the given number of dimensions
     * and all contents set to zero.
     * Throws an Exception if the dimension count is negative.
     */
    Dimensions(indexType dimCount);
    //=============================================================================
    /**
     * Return a 2-D dimension object with the specified number of
     * rows and columns.
     */
    Dimensions(indexType nrows, indexType ncols);
    //=============================================================================
    /**
     * Initialize the object with the given number of dimensions
     * and all contents set to zero.
     * Throws an Exception if the dimension count is negative.
     */
    Dimensions(const std::vector<indexType>& dimsVector);
    //=============================================================================
    /**
     * Return a reference to the ith dimension.  This member function
     * will adjust the number of dimensions present if the argument
     * exceeds the current number allocated.  The extra dimensions are
     * padded with 1's.  Thus, if our contents are $$[2,3]$$, then
     * a reference to dimension 2 (these are zero-based indexing) converts
     * our contents to $$[2,3,1]$$.
     * Throws an exception if the argument is bigger tha maxIndex.
     */
    indexType&
    operator[](indexType i);
    //=============================================================================
    /**
     * Return a reference to the ith dimension.
     * Throws an exception if the argument is bigger tha maxIndex or length.
     */
    indexType
    getAt(indexType i, bool checkLength = true) const;
    //=============================================================================
    /**
     * Return dimensions as vector;
     */
    std::vector<indexType>
    getAsVector();
    //=============================================================================
    /**
     * Get the number of currently allocated dimensions.
     */
    [[nodiscard]] indexType
    getLength() const;
    //=============================================================================
    /**
     * Return the total number of elements in the associated ArrayOf object.
     * calculated via $$\Prod_{i=0}^{L-1} a_i$$, where $$L$$ is the value
     * of length, and $$a_i$$ is equivalent to data[i].
     */
    [[nodiscard]] indexType
    getElementCount() const;
    //=============================================================================
    /**
     * Map the given point using the current Dimensions.  If the argument
     * values are denoted as $$b_i$$, and our dimensions are $$a_i$$, then
     * this member function computes $$\Sum_{i=0}^{L-1} b_i \Prod_{j=0}^{j=i} a_j$$.
     * Throws an exception if:
     *   - the argument has more dimensions than the current object
     *   - any of the arguments are outside the valid range, i.e.,
     *     $$b_i < 0$$ or $$b_i >= a_i$$.
     */
    indexType
    mapPoint(const Dimensions& point);
    //=============================================================================
    /**
     * Returns the first dimension value (or zero if no dimensions have
     * been defined yet).
     */
    [[nodiscard]] indexType
    getRows() const;
    //=============================================================================
    /**
     * Returns the second dimension value (or zero if no dimensions have
     * been defined yet).
     */
    [[nodiscard]] indexType
    getColumns() const;
    //=============================================================================
    /**
     * Returns the requested dimension, or a 1 if the requested dimension
     * exceeds the currently allocated number of dimensions.  Unlike
     * the access operator, this call does not modify the contents of
     * the class.
     */
    [[nodiscard]] indexType
    getDimensionLength(indexType arg) const;
    //=============================================================================
    /**
     * A synonym for (*this)[dim] = len.
     */
    void
    setDimensionLength(indexType dim, indexType len);
    /**
     * Expand our dimensions so as to include the given point.  This operation
     * involves a sequence of operations.  Let $$b$$ denote the argument, and
     * $$a$$ denote this object.  Then first we update the length of $$a$$
     * as $$ |a| \leftarrow \max(|a|,|b|) $$.  Next, we loop over each dimension
     * and apply the following algorithm:
     *    - if $$a_i$$ is undefined but $$b_i$$ is defined, $$a_i \leftarrow b_i$$.
     *    - if $$a_i$$ is defined but $$b_i$$ is undefined, $$a_i \leftarrow a_i$$.
     *    - if $$a_i$$ and $$b_i$$ are both defined, $$a_i \leftarrow \max(a_i,b_i)$$.
     */
    void
    expandToCover(const Dimensions& a);
    //=============================================================================
    /**
     * Increment our current value in the given dimension (ordinal),
     * modulo the limit.  This is equivalent to first incrementing
     * the $$n$$-th dimension (where $$n$$ is the second argument) via
     * $$a_n \leftarrow a_n + 1$$.  Then, $$a_n$$ is checked against
     * the limit $$b_n$$.  If $$a_n >= b_n$$, then $$a_n \leftarrow 0$$,
     * and the algorithm is repeated with ordinal+1.
     */
    void
    incrementModulo(const Dimensions& limit, int ordinal);
    //=============================================================================
    /**
     * Returns comparison of $$a_n < b_n$$, where $$n$$ is the maximum defined
     * dimension in $$a$$ (this object), and $$b$$ is the given argument.
     */
    bool
    inside(const Dimensions& limit);
    //=============================================================================
    /**
     * Returns true if we match the argument (exactly).
     */
    [[nodiscard]] bool
    equals(const Dimensions& alt) const;
    //=============================================================================
    /**
     * This member function trims any excess singleton (length 1) dimensions
     * from our data array after the second dimension.  Thus, the dimension
     * vector $$[3,4,5,1,1]$$, will become $$[3,4,5]$$ after simplify() is
     * called.
     */
    void
    simplify();
    //=============================================================================
    /**
     * Print in a string some representation of this object as a row vector, i.e.,
     * $$[a_1 a_2 \ldots a_n]$$.
     */
    [[nodiscard]] std::string
    toString() const;

    [[nodiscard]] std::wstring
    toWideString() const;
    //=============================================================================
    /**
     * Reset the number of allocated dimensions to zero, and reset the
     * data pointer to NULL.
     */
    void
    reset();
    //=============================================================================
    /**
     * Set all of the allocated dimensions to zero.
     */
    void
    zeroOut();
    //=============================================================================
    /**
     * Force the contents to be $$[1,1]$$.
     */
    void
    makeScalar();
    //=============================================================================
    /**
     * Get the largest dimension value, i.e., $$\max_{i=1,\ldots,n} \{a_i\}$$.
     */
    indexType
    getMax();
    //=============================================================================
    /**
     * Returns true if and only if we are equal to $$[1,1]$$.
     */
    [[nodiscard]] bool
    isScalar() const;
    //=============================================================================
    /**
     * Returns true if and only if we are equal to $$[1,n]$$ or $$[n,1]$$ for
     * some value of $$n$$.
     */
    [[nodiscard]] bool
    isVector() const;
    //=============================================================================
    [[nodiscard]] bool
    isRowVector() const;
    //=============================================================================
    [[nodiscard]] bool
    isColumnVector() const;
    //=============================================================================
    /**
     * Returns true if we have exactly 2 dimensions allocated.
     */
    [[nodiscard]] bool
    is2D() const;
    //=============================================================================
    /**
     * Returns true if we have exactly 2 dimensions allocated and cols == rows.
     */
    [[nodiscard]] bool
    isSquare() const;
    //=============================================================================
    /**
     * Returns TRUE if we are empty (we have no elements).
     */
    [[nodiscard]] bool
    isEmpty(bool allDimensionsIsZero) const;
    //=============================================================================
    /**
     * Apply a permutation to this dimension vector, and returns the permuted
     * vector
     */
    [[nodiscard]] Dimensions
    permute(const std::vector<indexType>& permutationVector, bool emptyDimensions) const;
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
