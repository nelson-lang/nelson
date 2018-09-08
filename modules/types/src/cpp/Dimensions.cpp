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
#include "Dimensions.hpp"
#include "Error.hpp"
#include <cstring>
#include <memory>
#include <stdio.h>
#include <stdlib.h>
#include <string>
//=============================================================================
#ifdef _MSC_VER
#define snprintf _snprintf
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
#define MSGBUFLEN 2048
static char msgBuffer[MSGBUFLEN];
//=============================================================================
Dimensions::Dimensions()
{
    std::uninitialized_fill_n(data, maxDims, 0);
    length = 0;
}
//=============================================================================
Dimensions::Dimensions(indexType rows, indexType cols)
{
    data[0] = rows;
    data[1] = cols;
    length = 2;
}
//=============================================================================
Dimensions::Dimensions(indexType dimCount)
{
#ifndef NLS_INDEX_TYPE_64
    if (dimCount < 0) {
        Error(_W("Illegal argument to Dimensions constructor"));
    }
#endif
    memset(data, 0, sizeof(indexType) * dimCount);
    length = dimCount;
}
//=============================================================================
indexType
Dimensions::getMax()
{
    sizeType maxL = 0;
    for (sizeType i = 0; i < length; i++) {
        maxL = (maxL > data[i]) ? maxL : data[i];
    }
    return maxL;
}
//=============================================================================
indexType& Dimensions::operator[](indexType i)
{
    if (i >= maxDims) {
        Error(_("Too many dimensions! Current limit is") + " " + std::to_string(Nelson::maxDims)
            + ".");
    }
    if (i >= length) {
        indexType new_length = i + 1;
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (indexType j = length; j < new_length; j++) {
            data[j] = 1;
        }
        length = new_length;
    }
    return data[i];
}
//=============================================================================
indexType
Dimensions::getLength() const
{
    return length;
}
//=============================================================================
indexType
Dimensions::getElementCount() const
{
    indexType retval;
    if (length == 0) {
        return 0;
    }
    retval = 1;
    for (sizeType i = 0; i < length; i++) {
        retval *= data[i];
    }
    return retval;
}
//=============================================================================
indexType
Dimensions::getRows() const
{
    if (length == 0) {
        return 0;
    } else {
        return data[0];
    }
}
//=============================================================================
indexType
Dimensions::getColumns() const
{
    if (length == 0) {
        return 0;
    } else if (length == 1) {
        return 1;
    } else {
        return data[1];
    }
}
//=============================================================================
indexType
Dimensions::getDimensionLength(sizeType arg) const
{
    if (length <= arg) {
        return 1;
    } else {
        return data[arg];
    }
}
//=============================================================================
void
Dimensions::setDimensionLength(indexType dim, indexType len)
{
    data[dim] = len;
}
//=============================================================================
indexType
Dimensions::mapPoint(const Dimensions& point)
{
    indexType retval;
    indexType nextCoeff;
    indexType testableDims;
    retval = 0;
    nextCoeff = 1;
    testableDims = (point.length < length) ? point.length : length;
    for (indexType i = 0; i < testableDims; i++) {
        if ((point.data[i] < 0) || (point.data[i] >= data[i])) {
            Error(_W("Index exceeds dimensions."));
        }
        retval += nextCoeff * point.data[i];
        nextCoeff *= data[i];
    }
    for (sizeType j = testableDims; j < point.length; j++) {
        if (point.data[j] != 0) {
            Error(_W("Index exceeds dimensions."));
        }
    }
    return retval;
}
//=============================================================================
void
Dimensions::expandToCover(const Dimensions& a)
{
    sizeType sze;
    sizeType i;
    Dimensions dimensions(*this);
    /**
     * First, compute the larger of the two: the number of current dimensions
     * and the number of requested dimensions.
     */
    sze = (a.length > length) ? a.length : dimensions.length;
    /**
     * Allocate a dimension vector to hold the new dimensions.  It should
     * be of size sze.
     */
    reset();
    /**
     * Now we loop over the dimensions.  For each dimensions, we could have
     * three cases to deal with:
     *   1. a[i] is undefined but dimensions[i] is -> newsize[i] = dimensions[i];
     *   2. a[i] is defined but dimensions[i] is not -> newsize[i] = a[i];
     *   3. a[i] and dimensions[i] are both defined ->
     *                                newsize[i] = max(a[i],dimensions[i]);
     */
    for (i = 0; i < sze; i++) {
        /**
         * Case 1:
         */
        if (i >= a.length) {
            (*this)[i] = dimensions[i];
        }
        /**
         * Case 2:
         */
        else if (i >= dimensions.length) {
            (*this)[i] = a.data[i];
        } else {
            (*this)[i] = (a.data[i] > dimensions[i]) ? a.data[i] : dimensions[i];
        }
    }
}
//=============================================================================
void
Dimensions::incrementModulo(const Dimensions& limit, int ordinal)
{
    sizeType n;
    data[ordinal]++;
    for (n = ordinal; n < length - 1; n++)
        if (data[n] >= limit.data[n]) {
            data[n] = 0;
            data[n + 1]++;
        }
}
//=============================================================================
bool
Dimensions::inside(const Dimensions& limit)
{
    return (data[length - 1] < limit.data[length - 1]);
}
//=============================================================================
void
Dimensions::simplify()
{
    if (length <= 2) {
        return;
    }
    indexType i = length - 1;
    while (i > 1 && data[i] == 1) {
        i--;
    }
    length = i + 1;
}
//=============================================================================
bool
Dimensions::equals(const Dimensions& alt)
{
    bool retval;
    retval = (length == alt.length);
    for (sizeType i = 0; i < length; i++) {
        retval = retval && (data[i] == alt.data[i]);
    }
    return retval;
}
//=============================================================================
std::string
Dimensions::toString() const
{
    std::string txt = "";
    strcpy(msgBuffer, "");
    char buf[400];
    if (length > 0) {
        for (indexType i = 0; i < length - 1; i++) {
            if (length >= 1) {
                sprintf(buf, "%lux", (unsigned long)data[i]);
            } else {
                sprintf(buf, "%lu", (unsigned long)data[i]);
            }
            strcat(msgBuffer, buf);
        }
    }
    if (length >= 1) {
        sprintf(buf, "%lu", (unsigned long)data[length - 1]);
        strcat(msgBuffer, buf);
    }
    txt = msgBuffer;
    return txt;
}
//=============================================================================
void
Dimensions::printMe(Interface* io) const
{
    std::string txt = toString();
    io->outputMessage(txt);
}
//=============================================================================
void
Dimensions::reset()
{
    length = 0;
    memset(data, 0, sizeof(indexType) * maxDims);
}
//=============================================================================
void
Dimensions::zeroOut()
{
    for (sizeType i = 0; i < length; i++) {
        data[i] = 0;
    }
}
//=============================================================================
void
Dimensions::makeScalar()
{
    reset();
    length = 2;
    data[0] = 1;
    data[1] = 1;
}
//=============================================================================
bool
Dimensions::isScalar() const
{
    if (getLength() == 2) {
        return (data[0] == 1 && data[1] == 1);
    }
    return (getElementCount() == 1);
}
//=============================================================================
bool
Dimensions::isVector() const
{
    return (isRowVector() || isColumnVector());
}
//=============================================================================
bool
Dimensions::isRowVector() const
{
    return (getElementCount() == data[1]);
}
//=============================================================================
bool
Dimensions::isColumnVector() const
{
    return (getElementCount() == data[0]);
}
//=============================================================================
bool
Dimensions::is2D() const
{
    return length <= 2;
    // return (getElementCount() == (getRows()*getColumns()));
}
//=============================================================================
bool
Dimensions::isSquare() const
{
    return is2D() && (getRows() == getColumns());
}
//=============================================================================
bool
Dimensions::isEmpty(bool allDimensionsIsZero) const
{
    if (allDimensionsIsZero) {
        indexType l = getLength();
        for (indexType k = 0; k < l; k++) {
            if (getDimensionLength(k) != 0) {
                return false;
            }
        }
        return true;
    }
    if (getLength() == 0) {
        return true;
    }
    return (getElementCount() == 0);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
