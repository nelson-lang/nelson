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
#include <algorithm>
#include <cmath>
#include <string>
#include <type_traits>
#include "omp_for_loop.hpp"
#include "ArrayOf.hpp"
#include "BinaryOperatorsHelpers.hpp"
#include "MatrixCheck.hpp"
#include "NewWithException.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
enum class SparseRelationOperation
{
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual
};
//=============================================================================
static SparseRelationOperation
sparseRelationOperationFromName(const std::wstring& operatorName)
{
    if (operatorName == L"==") {
        return SparseRelationOperation::Equal;
    }
    if (operatorName == L"~=") {
        return SparseRelationOperation::NotEqual;
    }
    if (operatorName == L"<") {
        return SparseRelationOperation::Less;
    }
    if (operatorName == L"<=") {
        return SparseRelationOperation::LessEqual;
    }
    if (operatorName == L">") {
        return SparseRelationOperation::Greater;
    }
    return SparseRelationOperation::GreaterEqual;
}
//=============================================================================
static std::string
asciiOperatorName(const std::wstring& operatorName)
{
    std::string result;
    result.reserve(operatorName.size());
    for (wchar_t ch : operatorName) {
        result.push_back(static_cast<char>(ch));
    }
    return result;
}
//=============================================================================
// A 64-bit integer cannot be converted to double without loss above 2^53 and a
// double cannot be converted to the integer class without losing its
// fractional part: both operands are compared exactly instead.
//=============================================================================
static bool
isMixed64BitIntegerDoublePair(const ArrayOf& A, const ArrayOf& B)
{
    auto is64 = [](const ArrayOf& v) {
        return v.dataClass() == NLS_INT64 || v.dataClass() == NLS_UINT64;
    };
    return (is64(A) && B.dataClass() == NLS_DOUBLE) || (is64(B) && A.dataClass() == NLS_DOUBLE);
}
//=============================================================================
// ordering of the integer value against the double value: -1, 0, 1; unordered
// (NaN) is reported through the flag
//=============================================================================
static int
compareInt64WithDouble(int64 value, double d, bool& unordered)
{
    unordered = std::isnan(d);
    if (unordered) {
        return 0;
    }
    if (d >= 9223372036854775808.0) {
        return -1;
    }
    if (d < -9223372036854775808.0) {
        return 1;
    }
    double f = std::floor(d);
    auto fi = static_cast<int64>(f);
    if (value < fi) {
        return -1;
    }
    if (value > fi) {
        return 1;
    }
    return (d > f) ? -1 : 0;
}
//=============================================================================
static int
compareUint64WithDouble(uint64 value, double d, bool& unordered)
{
    unordered = std::isnan(d);
    if (unordered) {
        return 0;
    }
    if (d >= 18446744073709551616.0) {
        return -1;
    }
    if (d < 0.0) {
        return 1;
    }
    double f = std::floor(d);
    auto fi = static_cast<uint64>(f);
    if (value < fi) {
        return -1;
    }
    if (value > fi) {
        return 1;
    }
    return (d > f) ? -1 : 0;
}
//=============================================================================
static logical
relationFromOrdering(int ordering, bool unordered, SparseRelationOperation operation)
{
    switch (operation) {
    case SparseRelationOperation::Equal:
        return static_cast<logical>(!unordered && ordering == 0);
    case SparseRelationOperation::NotEqual:
        return static_cast<logical>(unordered || ordering != 0);
    case SparseRelationOperation::Less:
        return static_cast<logical>(!unordered && ordering < 0);
    case SparseRelationOperation::LessEqual:
        return static_cast<logical>(!unordered && ordering <= 0);
    case SparseRelationOperation::Greater:
        return static_cast<logical>(!unordered && ordering > 0);
    case SparseRelationOperation::GreaterEqual:
        return static_cast<logical>(!unordered && ordering >= 0);
    }
    return 0;
}
//=============================================================================
// 64-bit integer array against a real double scalar: the scalar is turned into
// an exact integer threshold so that the comparison runs as a plain integer
// loop (x > 5.5 is x > 5, x <= -0.5 is x <= -1, NaN compares false...).
// `operation` is written as "integer operation scalar".
//=============================================================================
template <class T>
static ArrayOf
integerScalarThresholdRelation(
    const ArrayOf& integerArray, double scalar, SparseRelationOperation operation)
{
    const double lowest = std::is_signed<T>::value ? -9223372036854775808.0 : 0.0;
    const double above = std::is_signed<T>::value ? 9223372036854775808.0 : 18446744073709551616.0;
    Dimensions dims = integerArray.dimensions();
    indexType n = dims.elementCount();
    auto* Cp = new_with_exception<logical>(n, false);
    const auto* x = static_cast<const T*>(integerArray.dataPointer());
    bool isConstant = false;
    bool constantValue = false;
    T t = 0;
    if (std::isnan(scalar)) {
        isConstant = true;
        constantValue = (operation == SparseRelationOperation::NotEqual);
    } else if (scalar >= above || scalar < lowest) {
        // every integer of the class is below (or above) the scalar
        bool integerIsBelow = (scalar >= above);
        isConstant = true;
        switch (operation) {
        case SparseRelationOperation::Less:
        case SparseRelationOperation::LessEqual:
            constantValue = integerIsBelow;
            break;
        case SparseRelationOperation::Greater:
        case SparseRelationOperation::GreaterEqual:
            constantValue = !integerIsBelow;
            break;
        case SparseRelationOperation::NotEqual:
            constantValue = true;
            break;
        default:
            constantValue = false;
            break;
        }
    } else {
        double whole = std::floor(scalar);
        t = static_cast<T>(whole);
        if (whole != scalar) {
            switch (operation) {
            case SparseRelationOperation::Equal:
                isConstant = true;
                constantValue = false;
                break;
            case SparseRelationOperation::NotEqual:
                isConstant = true;
                constantValue = true;
                break;
            case SparseRelationOperation::Less:
            case SparseRelationOperation::LessEqual:
                operation = SparseRelationOperation::LessEqual;
                break;
            default:
                operation = SparseRelationOperation::Greater;
                break;
            }
        }
    }
    if (isConstant) {
        std::fill_n(Cp, n, static_cast<logical>(constantValue));
        return ArrayOf(NLS_LOGICAL, dims, Cp, false);
    }
    switch (operation) {
    case SparseRelationOperation::Equal:
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType k = 0; k < (ompIndexType)n; ++k) {
            Cp[k] = static_cast<logical>(x[k] == t);
        }
        break;
    case SparseRelationOperation::NotEqual:
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType k = 0; k < (ompIndexType)n; ++k) {
            Cp[k] = static_cast<logical>(x[k] != t);
        }
        break;
    case SparseRelationOperation::Less:
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType k = 0; k < (ompIndexType)n; ++k) {
            Cp[k] = static_cast<logical>(x[k] < t);
        }
        break;
    case SparseRelationOperation::LessEqual:
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType k = 0; k < (ompIndexType)n; ++k) {
            Cp[k] = static_cast<logical>(x[k] <= t);
        }
        break;
    case SparseRelationOperation::Greater:
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType k = 0; k < (ompIndexType)n; ++k) {
            Cp[k] = static_cast<logical>(x[k] > t);
        }
        break;
    case SparseRelationOperation::GreaterEqual:
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType k = 0; k < (ompIndexType)n; ++k) {
            Cp[k] = static_cast<logical>(x[k] >= t);
        }
        break;
    }
    return ArrayOf(NLS_LOGICAL, dims, Cp, false);
}
//=============================================================================
static ArrayOf
mixed64BitIntegerDoubleRelation(
    const ArrayOf& A, const ArrayOf& B, const std::wstring& operatorName)
{
    const bool integerIsLeft = (A.dataClass() != NLS_DOUBLE);
    const ArrayOf& I = integerIsLeft ? A : B;
    const ArrayOf& D = integerIsLeft ? B : A;
    SparseRelationOperation operation = sparseRelationOperationFromName(operatorName);
    if (D.isScalar()) {
        // "double operation integer" is "integer swapped-operation double"
        SparseRelationOperation seenFromInteger = operation;
        if (!integerIsLeft) {
            if (operation == SparseRelationOperation::Less) {
                seenFromInteger = SparseRelationOperation::Greater;
            } else if (operation == SparseRelationOperation::LessEqual) {
                seenFromInteger = SparseRelationOperation::GreaterEqual;
            } else if (operation == SparseRelationOperation::Greater) {
                seenFromInteger = SparseRelationOperation::Less;
            } else if (operation == SparseRelationOperation::GreaterEqual) {
                seenFromInteger = SparseRelationOperation::LessEqual;
            }
        }
        double scalar = static_cast<const double*>(D.dataPointer())[0];
        if (I.dataClass() == NLS_INT64) {
            return integerScalarThresholdRelation<int64>(I, scalar, seenFromInteger);
        }
        return integerScalarThresholdRelation<uint64>(I, scalar, seenFromInteger);
    }
    const auto* ptrD = static_cast<const double*>(D.dataPointer());
    const bool isSigned = (I.dataClass() == NLS_INT64);
    const auto* ptrI64 = static_cast<const int64*>(I.dataPointer());
    const auto* ptrU64 = static_cast<const uint64*>(I.dataPointer());
    auto relationAt = [&](indexType indexI, indexType indexD) -> logical {
        bool unordered = false;
        int ordering = isSigned ? compareInt64WithDouble(ptrI64[indexI], ptrD[indexD], unordered)
                                : compareUint64WithDouble(ptrU64[indexI], ptrD[indexD], unordered);
        if (!integerIsLeft) {
            ordering = -ordering;
        }
        return relationFromOrdering(ordering, unordered, operation);
    };
    Dimensions dimsI = I.dimensions();
    Dimensions dimsD = D.dimensions();
    if (SameSizeCheck(dimsI, dimsD) || I.isScalar()) {
        // no broadcasting: the double array fixes the size
        const bool integerIsScalar = I.isScalar();
        Dimensions dims = D.dimensions();
        indexType Clen = dims.elementCount();
        auto* Cp = new_with_exception<logical>(Clen, false);
        OMP_PARALLEL_FOR_LOOP(Clen)
        for (ompIndexType i = 0; i < (ompIndexType)Clen; ++i) {
            Cp[i] = relationAt(integerIsScalar ? 0 : (indexType)i, (indexType)i);
        }
        return ArrayOf(NLS_LOGICAL, dims, Cp, false);
    }
    BinaryBroadcast info = binaryOperatorBroadcast(A, B, asciiOperatorName(operatorName));
    const auto& integerStrides = integerIsLeft ? info.aStrides : info.bStrides;
    const auto& doubleStrides = integerIsLeft ? info.bStrides : info.aStrides;
    indexType Clen = info.dimensions.elementCount();
    auto* Cp = new_with_exception<logical>(Clen, false);
    for (indexType i = 0; i < Clen; ++i) {
        indexType indexI = binaryOperatorBroadcastIndex(info, i, integerStrides);
        indexType indexD = binaryOperatorBroadcastIndex(info, i, doubleStrides);
        Cp[i] = relationAt(indexI, indexD);
    }
    return ArrayOf(NLS_LOGICAL, info.dimensions, Cp, false);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
