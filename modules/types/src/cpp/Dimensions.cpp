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
#include <cstring>
#include <string>
#include <memory>
#include <cstdio>
#include <cstdlib>
#include "Dimensions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
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
            Error(L"Nelson:types:invalidDimensions:illegalArgumentDimensionsConstructor",
                L"Illegal argument to Dimensions constructor");
        }
    }
#else
    if (szVector > maxDims) {
        Error(L"Nelson:types:invalidDimensions:illegalArgumentDimensionsConstructor",
            L"Illegal argument to Dimensions constructor");
    }
#endif
    reset();
    for (indexType k = 0; k < szVector; ++k) {
        data[k] = dimsVector[k];
    }
    length_ = szVector;
}
//=============================================================================
Dimensions::Dimensions(indexType rows, indexType cols)
{
    reset();
    data[0] = rows;
    data[1] = cols;
    length_ = 2;
}
//=============================================================================
Dimensions::Dimensions(indexType dimCount)
{
#ifndef NLS_INDEX_TYPE_64
    if (dimCount < 0) {
        Error(L"Nelson:types:invalidDimensions:illegalArgumentDimensionsConstructor",
            L"Illegal argument to Dimensions constructor");
    }
#endif
    reset();
    length_ = dimCount;
}
//=============================================================================
std::vector<indexType>
Dimensions::asVector()
{
    std::vector<indexType> vector;
    vector.resize(length_, 0);
    for (indexType k = 0; k < length_; ++k) {
        vector[k] = data[k];
    }
    return vector;
}
//=============================================================================
indexType
Dimensions::maximum()
{
    sizeType maxL = 0;
    for (indexType i = 0; i < length_; i++) {
        maxL = (maxL > data[i]) ? maxL : data[i];
    }
    return maxL;
}
//=============================================================================
indexType&
Dimensions::operator[](indexType i)
{
    if (i >= maxDims) {
        Error(formatI18nMessage(
            Nelson::resolveMessageTemplate("Nelson:types:tooManyDimensionsCurrent",
                "Too many dimensions! Current limit is %d."),
            Nelson::maxDims));
    }
    if (i >= length_) {
        indexType new_length = i + 1;
        for (indexType j = length_; j < new_length; j++) {
            data[j] = 1;
        }
        length_ = new_length;
    }
    return data[i];
}
//=============================================================================
indexType
Dimensions::at(indexType i, bool checkLength) const
{
    if (i >= maxDims) {
        Error(formatI18nMessage(
            Nelson::resolveMessageTemplate("Nelson:types:tooManyDimensionsCurrent",
                "Too many dimensions! Current limit is %d."),
            Nelson::maxDims));
    }
    if (checkLength) {
        if (i >= length_) {
            Error("Nelson:types:invalidDimensions:invalidDimensionPosition",
                "Invalid dimension position.");
        }
    }
    return data[i];
}
//=============================================================================
indexType
Dimensions::length() const
{
    return length_;
}
//=============================================================================
indexType
Dimensions::elementCount() const
{
    indexType retval;
    if (length_ == 0) {
        return 0;
    }
    retval = 1;
    for (indexType i = 0; i < length_; i++) {
        retval *= data[i];
    }
    return retval;
}
//=============================================================================
indexType
Dimensions::rows() const
{
    if (length_ == 0) {
        return 0;
    }
    return data[0];
}
//=============================================================================
indexType
Dimensions::columns() const
{
    if (length_ == 0) {
        return 0;
    }
    if (length_ == 1) {
        return 1;
    }
    return data[1];
}
//=============================================================================
indexType
Dimensions::dimensionLength(indexType arg) const
{
    if (length_ <= arg) {
        return 1;
    }
    return data[arg];
}
//=============================================================================
Dimensions
Dimensions::foldForSubscriptCount(indexType subscriptCount) const
{
    Dimensions result(subscriptCount);
    if (subscriptCount == 0) {
        return result;
    }
    for (indexType i = 0; i < subscriptCount - 1; i++) {
        result[i] = dimensionLength(i);
    }
    indexType folded = 1;
    for (indexType k = subscriptCount - 1; k < length_; k++) {
        folded *= data[k];
    }
    result[subscriptCount - 1] = folded;
    return result;
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
    testableDims = (point.length_ < length_) ? point.length_ : length_;
    for (indexType i = 0; i < testableDims; i++) {
        if ((point.data[i] < 0) || (point.data[i] >= data[i])) {
            Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
        }
        retval += nextCoeff * point.data[i];
        nextCoeff *= data[i];
    }
    for (indexType j = testableDims; j < point.length_; j++) {
        if (point.data[j] != 0) {
            Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
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
    sze = (a.length_ > length_) ? a.length_ : dimensions.length_;
    reset();
    // Each dimension takes the one that is defined, or the larger when both are.
    for (i = 0; i < sze; i++) {
        if (i >= a.length_) {
            (*this)[i] = dimensions[i];
        } else if (i >= dimensions.length_) {
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
    for (n = ordinal; n < length_ - 1; n++) {
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
    return (data[length_ - 1] < limit.data[length_ - 1]);
}
//=============================================================================
void
Dimensions::simplify()
{
    if (length_ <= 2) {
        return;
    }
    indexType i = length_ - 1;
    while (i > 1 && data[i] == 1) {
        i--;
    }
    length_ = i + 1;
}
//=============================================================================
bool
Dimensions::equals(const Dimensions& alt) const
{
    bool retval;
    retval = (length_ == alt.length_);
    for (indexType i = 0; i < length_; i++) {
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
    if (length_ > 4) {
        text.append(std::to_wstring(length_) + L"-D");
    } else {
        if (length_ > 0) {
            for (indexType i = 0; i < length_ - 1; i++) {
                unsigned long long int val = static_cast<unsigned long long int>(data[i]);
                text.append(std::to_wstring(val));
                if (length_ >= 1) {
                    text.append(MIDDLE_WMULTIPLY);
                }
            }
        }
        if (length_ >= 1) {
            unsigned long long int val = static_cast<unsigned long long int>(data[length_ - 1]);
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
    length_ = 0;
}
//=============================================================================
void
Dimensions::zeroOut()
{
    std::fill_n(data.begin(), length_, (indexType)0);
}
//=============================================================================
void
Dimensions::makeScalar()
{
    reset();
    length_ = 2;
    data[0] = 1;
    data[1] = 1;
}
//=============================================================================
bool
Dimensions::isScalar() const
{
    if (length() == 2) {
        return (data[0] == 1 && data[1] == 1);
    }
    return (elementCount() == 1);
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
    return (length() == 2 && rows() == 1);
}
//=============================================================================
bool
Dimensions::isColumnVector() const
{
    return (length() == 2 && columns() == 1);
}
//=============================================================================
bool
Dimensions::is2D() const
{
    return length_ <= 2;
}
//=============================================================================
bool
Dimensions::isSquare() const
{
    return is2D() && (rows() == columns());
}
//=============================================================================
bool
Dimensions::isEmpty(bool allDimensionsIsZero) const
{
    if (allDimensionsIsZero) {
        indexType l = length();
        for (indexType k = 0; k < l; k++) {
            if (dimensionLength(k) != 0) {
                return false;
            }
        }
        return true;
    }
    if (length() == 0) {
        return true;
    }
    return (elementCount() == 0);
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
