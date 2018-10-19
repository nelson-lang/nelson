//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
// Copyright (c) 2002, 2003 Samit Basu
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

#pragma once

#include "Error.hpp"
#include "Interface.hpp"
#include "Types.hpp"
#include "nlsTypes_exports.h"
#include <iostream>

#ifdef _MSC_VER
#pragma warning(disable : 4290)
#endif

namespace Nelson {

const unsigned int maxDims = 16;

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
class NLSTYPES_IMPEXP Dimensions
{
private:
    /**
     * The data array of dimensions.
     */
    indexType data[maxDims];
    /**
     * The number of dimensions currently allocated.
     */
    indexType length;

public:
    /**
     * The default constructor - length is set to zero.
     */
    Dimensions();
    /**
     * Initialize the object with the given number of dimensions
     * and all contents set to zero.
     * Throws an Exception if the dimension count is negative.
     */
    Dimensions(indexType dimCount);
    /**
     * Return a 2-D dimension object with the specified number of
     * rows and columns.
     */
    Dimensions(indexType nrows, indexType ncols);
    /**
     * Return a reference to the ith dimension.  This member function
     * will adjust the number of dimensions present if the argument
     * exceeds the current number allocated.  The extra dimensions are
     * padded with 1's.  Thus, if our contents are $$[2,3]$$, then
     * a reference to dimension 2 (these are zero-based indexing) converts
     * our contents to $$[2,3,1]$$.
     * Throws an exception if the argument is negative.
     */
    indexType& operator[](indexType i);
    /**
     * Get the number of currently allocated dimensions.
     */
    indexType
    getLength() const;
    /**
     * Return the total number of elements in the associated ArrayOf object.
     * calculated via $$\Prod_{i=0}^{L-1} a_i$$, where $$L$$ is the value
     * of length, and $$a_i$$ is equivalent to data[i].
     */
    indexType
    getElementCount() const;
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
    /**
     * Returns the first dimension value (or zero if no dimensions have
     * been defined yet).
     */
    indexType
    getRows() const;
    /**
     * Returns the second dimension value (or zero if no dimensions have
     * been defined yet).
     */
    indexType
    getColumns() const;
    /**
     * Returns the requested dimension, or a 1 if the requested dimension
     * exceeds the currently allocated number of dimensions.  Unlike
     * the access operator, this call does not modify the contents of
     * the class.
     */
    indexType
    getDimensionLength(sizeType arg) const;
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
    expandToCover(const Dimensions& resize);
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
    /**
     * Returns comparison of $$a_n < b_n$$, where $$n$$ is the maximum defined
     * dimension in $$a$$ (this object), and $$b$$ is the given argument.
     */
    bool
    inside(const Dimensions& limit);
    /**
     * Returns true if we match the argument (exactly).
     */
    bool
    equals(const Dimensions& alt);
    /**
     * This member function trims any excess singleton (length 1) dimensions
     * from our data array after the second dimension.  Thus, the dimension
     * vector $$[3,4,5,1,1]$$, will become $$[3,4,5]$$ after simplify() is
     * called.
     */
    void
    simplify();

    /**
     * Print in a string some representation of this object as a row vector, i.e.,
     * $$[a_1 a_2 \ldots a_n]$$.
     */
    std::string
    toString() const;

    /**
     * Print some representation of this object as a row vector, i.e.,
     * $$[a_1 a_2 \ldots a_n]$$.
     */
    void
    printMe(Interface* io) const;
    /**
     * Reset the number of allocated dimensions to zero, and reset the
     * data pointer to NULL.
     */
    void
    reset();
    /**
     * Set all of the allocated dimensions to zero.
     */
    void
    zeroOut();
    /**
     * Force the contents to be $$[1,1]$$.
     */
    void
    makeScalar();
    /**
     * Get the largest dimension value, i.e., $$\max_{i=1,\ldots,n} \{a_i\}$$.
     */
    indexType
    getMax();
    /**
     * Returns true if and only if we are equal to $$[1,1]$$.
     */
    bool
    isScalar() const;
    /**
     * Returns true if and only if we are equal to $$[1,n]$$ or $$[n,1]$$ for
     * some value of $$n$$.
     */
    bool
    isVector() const;
    bool
    isRowVector() const;
    bool
    isColumnVector() const;
    /**
     * Returns true if we have exactly 2 dimensions allocated.
     */
    bool
    is2D() const;

    /**
     * Returns true if we have exactly 2 dimensions allocated and cols == rows.
     */
    bool
    isSquare() const;

    /**
     * Returns TRUE if we are empty (we have no elements).
     */
    bool
    isEmpty(bool allDimensionsIsZero) const;
};
} // namespace Nelson
