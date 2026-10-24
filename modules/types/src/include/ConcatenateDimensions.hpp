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
#include "Dimensions.hpp"
#include "ErrorID.hpp"
//=============================================================================
// Size rules shared by every concatenation (operators, cat, handles, sparse,
// cell2mat). axis is the 0-based concatenated dimension.
//=============================================================================
namespace Nelson {
//=============================================================================
// The only error raised when the sizes of concatenated arrays disagree.
inline void
raiseConcatenationDimensionMismatch()
{
    Error(L"Nelson:catenate:dimensionMismatch",
        L"Dimensions of arrays being concatenated are not consistent.");
}
//=============================================================================
// True when every dimension but axis agrees, trailing ones included.
inline bool
concatenationDimensionsAgree(const Dimensions& dimsA, const Dimensions& dimsB, indexType axis)
{
    indexType nbDims = std::max(dimsA.length(), dimsB.length());
    for (indexType k = 0; k < nbDims; k++) {
        if (k != axis && dimsA.dimensionLength(k) != dimsB.dimensionLength(k)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
inline void
checkConcatenationDimensions(const Dimensions& dimsA, const Dimensions& dimsB, indexType axis)
{
    if (!concatenationDimensionsAgree(dimsA, dimsB, axis)) {
        raiseConcatenationDimensionMismatch();
    }
}
//=============================================================================
inline bool
isZeroByZeroDimensions(const Dimensions& dims)
{
    return dims.is2D() && dims.rows() == 0 && dims.columns() == 0;
}
//=============================================================================
// An empty operand next to a non-empty one is dropped when it is 0x0 or when
// both are 2-D; otherwise ([zeros(0, 1), NaN(1, 1, 2)], [zeros(1, 0, 2), 1:3])
// it must still agree with the other one.
inline void
checkEmptyOperandDimensions(
    const Dimensions& dimsEmpty, const Dimensions& dimsOther, indexType axis)
{
    if (dimsEmpty.is2D() && (dimsOther.is2D() || isZeroByZeroDimensions(dimsEmpty))) {
        return;
    }
    checkConcatenationDimensions(dimsEmpty, dimsOther, axis);
}
//=============================================================================
// Same rule for two operands, except that a real dense scalar also drops an
// N-D empty operand of its own class when that class is numeric, logical, char
// or string ([zeros(0, 0, 2), 5] is 5; [zeros(0, 0, 2), int8(5)],
// [complex(zeros(0, 0, 2)), 1i] and [cell(0, 0, 2), {5}] are errors).
template <class Operand>
inline void
checkEmptyOperand(const Operand& empty, const Operand& other, indexType axis)
{
    if (other.isScalar() && !other.isSparse() && !other.isComplex()
        && other.dataClass() == empty.dataClass()
        && (other.isNumeric() || other.isLogical() || other.isCharacterArray()
            || other.isStringArray())) {
        return;
    }
    checkEmptyOperandDimensions(empty.dimensions(), other.dimensions(), axis);
}
//=============================================================================
// Three or more operands, not all empty: once one of them is N-D, every
// operand but a 0x0 one must agree, empty or not
// ([zeros(0, 1), zeros(1, 0), NaN(1, 1, 2)] is an error).
template <class Operands>
inline void
checkNdOperandsDimensions(const Operands& operands, indexType axis)
{
    bool hasNdOperand = false;
    for (const auto& operand : operands) {
        if (!operand.is2D()) {
            hasNdOperand = true;
            break;
        }
    }
    if (!hasNdOperand) {
        return;
    }
    bool started = false;
    Dimensions firstDims;
    for (const auto& operand : operands) {
        Dimensions dims = operand.dimensions();
        if (isZeroByZeroDimensions(dims)) {
            continue;
        }
        if (started) {
            checkConcatenationDimensions(firstDims, dims, axis);
        } else {
            firstDims = dims;
            started = true;
        }
    }
}
//=============================================================================
// Size of the concatenation of empty arrays, accumulated from left to right.
// A 0x0 operand is dropped and consistent operands are joined along axis.
// When two 2-D sizes disagree, an operand with no dimension above 1 yields to
// the other ([zeros(0, 1), zeros(2, 0)] is 2x0), two operands empty along axis
// keep the larger ([zeros(3, 0), zeros(4, 0)] is 4x0), two operands with no
// dimension above 1 give 0x0 that later operands can join, and anything else
// gives 0x0 that ignores every later 2-D operand. Once a disagreement involves
// an N-D operand, every later disagreement gives 0x0 that later operands can
// join.
class EmptyConcatenationDimensions
{
public:
    explicit EmptyConcatenationDimensions(indexType axis) : axis(axis) { }
    //=============================================================================
    void
    append(const Dimensions& dims)
    {
        if (isZeroByZeroDimensions(dims)) {
            return;
        }
        if (blocked) {
            if (!dims.is2D()) {
                blocked = false;
                ndMismatch = true;
            }
            return;
        }
        if (!started) {
            current = dims;
            started = true;
            return;
        }
        if (concatenationDimensionsAgree(current, dims, axis)) {
            current[axis] = current.dimensionLength(axis) + dims.dimensionLength(axis);
            return;
        }
        if (ndMismatch || !current.is2D() || !dims.is2D()) {
            ndMismatch = true;
            current = Dimensions(0, 0);
            return;
        }
        appendInconsistent2D(dims);
    }
    //=============================================================================
    Dimensions
    dimensions() const
    {
        return started ? current : Dimensions(0, 0);
    }
    //=============================================================================
private:
    indexType axis;
    Dimensions current;
    bool started = false;
    bool ndMismatch = false;
    bool blocked = false;
    //=============================================================================
    void
    appendInconsistent2D(const Dimensions& dims)
    {
        indexType largestCurrent = std::max(current.rows(), current.columns());
        indexType largestDims = std::max(dims.rows(), dims.columns());
        bool smallCurrent = largestCurrent <= 1;
        bool smallDims = largestDims <= 1;
        if (smallCurrent != smallDims) {
            if (smallCurrent) {
                current = dims;
            }
            return;
        }
        if (!smallCurrent && current.dimensionLength(axis) == 0
            && dims.dimensionLength(axis) == 0) {
            if (largestDims > largestCurrent) {
                current = dims;
            }
            return;
        }
        blocked = !smallCurrent;
        current = Dimensions(0, 0);
    }
};
//=============================================================================
// Size of the concatenation of two empty arrays.
inline Dimensions
concatenatedEmptyDimensions(const Dimensions& dimsA, const Dimensions& dimsB, indexType axis)
{
    EmptyConcatenationDimensions concatenation(axis);
    concatenation.append(dimsA);
    concatenation.append(dimsB);
    return concatenation.dimensions();
}
//=============================================================================
// True when every operand is empty; dims then receives the size of their
// concatenation.
template <class Operands>
inline bool
allEmptyOperandsDimensions(const Operands& operands, indexType axis, Dimensions& dims)
{
    EmptyConcatenationDimensions concatenation(axis);
    for (const auto& operand : operands) {
        if (!operand.isEmpty(false)) {
            return false;
        }
        concatenation.append(operand.dimensions());
    }
    dims = concatenation.dimensions();
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
