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
#include <cstring>
#include <string>
#include <memory>
#include <cstdio>
#include <cstdlib>
#include "Dimensions.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Dimensions::Dimensions() { std::fill(std::begin(data), std::end(data), 0); }
//=============================================================================
Dimensions::Dimensions(const std::vector<indexType>& dimsVector)
{
    indexType szVector = dimsVector.size();
#ifndef NLS_INDEX_TYPE_64
    for (auto k : dimsVector) {
        if (k < 0) {
            Error(_W("Illegal argument to Dimensions constructor"));
        }
    }
#else
    if (szVector > maxDims) {
        Error(_W("Illegal argument to Dimensions constructor"));
    }
#endif
    reset();
    for (indexType k = 0; k < szVector; ++k) {
        data[k] = dimsVector[k];
    }
    length = szVector;
}
//=============================================================================
Dimensions::Dimensions(indexType rows, indexType cols)
{
    reset();
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
    reset();
    length = dimCount;
}
//=============================================================================
std::vector<indexType>
Dimensions::getAsVector()
{
    std::vector<indexType> vector;
    vector.resize(length, 0);
    for (indexType k = 0; k < length; ++k) {
        vector[k] = data[k];
    }
    return vector;
}
//=============================================================================
indexType
Dimensions::getMax()
{
    sizeType maxL = 0;
    for (indexType i = 0; i < length; i++) {
        maxL = (maxL > data[i]) ? maxL : data[i];
    }
    return maxL;
}
//=============================================================================
indexType&
Dimensions::operator[](indexType i)
{
    if (i >= maxDims) {
        Error(_("Too many dimensions! Current limit is") + " " + std::to_string(Nelson::maxDims)
            + ".");
    }
    if (i >= length) {
        indexType new_length = i + 1;
        for (indexType j = length; j < new_length; j++) {
            data[j] = 1;
        }
        length = new_length;
    }
    return data[i];
}
//=============================================================================
indexType
Dimensions::getAt(indexType i, bool checkLength) const
{
    if (i >= maxDims) {
        Error(_("Too many dimensions! Current limit is") + " " + std::to_string(Nelson::maxDims)
            + ".");
    }
    if (checkLength) {
        if (i >= length) {
            Error(_("Invalid dimension position."));
        }
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
    for (indexType i = 0; i < length; i++) {
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
    }
    return data[0];
}
//=============================================================================
indexType
Dimensions::getColumns() const
{
    if (length == 0) {
        return 0;
    }
    if (length == 1) {
        return 1;
    }
    return data[1];
}
//=============================================================================
indexType
Dimensions::getDimensionLength(indexType arg) const
{
    if (length <= arg) {
        return 1;
    }
    return data[arg];
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
    for (indexType j = testableDims; j < point.length; j++) {
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
    indexType n;
    data[ordinal]++;
    for (n = ordinal; n < length - 1; n++) {
        if (data[n] >= limit.data[n]) {
            data[n] = 0;
            data[n + 1]++;
        }
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
Dimensions::equals(const Dimensions& alt) const
{
    bool retval;
    retval = (length == alt.length);
    for (indexType i = 0; i < length; i++) {
        retval = retval && (data[i] == alt.data[i]);
    }
    return retval;
}
//=============================================================================
std::wstring
Dimensions::toWideString() const
{
#define MIDDLE_WMULTIPLY L"\U000000D7"
    std::wstring text;
    if (length > 4) {
        text.append(std::to_wstring(length) + L"-D");
    } else {
        if (length > 0) {
            for (indexType i = 0; i < length - 1; i++) {
                unsigned long long int val = static_cast<unsigned long long int>(data[i]);
                text.append(std::to_wstring(val));
                if (length >= 1) {
                    text.append(MIDDLE_WMULTIPLY);
                }
            }
        }
        if (length >= 1) {
            unsigned long long int val = static_cast<unsigned long long int>(data[length - 1]);
            text.append(std::to_wstring(val));
        }
    }
    return text;
}
//=============================================================================
std::string
Dimensions::toString() const
{
    return wstring_to_utf8(toWideString());
}
//=============================================================================
void
Dimensions::reset()
{
    std::fill(data.begin(), data.end(), 0);
    length = 0;
}
//=============================================================================
void
Dimensions::zeroOut()
{
    std::fill_n(data.begin(), length, (indexType)0);
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
    return (getLength() == 2 && getRows() == 1);
}
//=============================================================================
bool
Dimensions::isColumnVector() const
{
    return (getLength() == 2 && getColumns() == 1);
}
//=============================================================================
bool
Dimensions::is2D() const
{
    return length <= 2;
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
Dimensions
Dimensions::permute(const std::vector<indexType>& permutationVector, bool emptyDimensions) const
{
    Dimensions out(permutationVector.size());
    for (ompIndexType i = 0; i < (ompIndexType)permutationVector.size(); i++) {
        if (emptyDimensions) {
            out.data[i] = data[permutationVector[i] - 1];
        } else {
            out.data[i] = data[permutationVector[i] - 1] ? data[permutationVector[i] - 1] : 1;
        }
    }
    return out;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
