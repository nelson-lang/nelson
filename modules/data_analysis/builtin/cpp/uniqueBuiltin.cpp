//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cwctype>
#include <numeric>
#include <vector>
#include "uniqueBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "OverloadRequired.hpp"
#include "Unique.hpp"
#include "UniqueMissing.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Warning.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
isTreatMissingAsDistinctName(const std::wstring& name);
static bool
parseTreatMissingAsDistinct(const ArrayOfVector& argIn, size_t valuePosition);
//=============================================================================
static ArrayOf
doubleIndexVector(const std::vector<indexType>& v)
{
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, v.size());
    for (size_t k = 0; k < v.size(); ++k) {
        ptr[k] = (double)v[k];
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(v.size(), 1), ptr);
}
//=============================================================================
// Moves the occurrence indices to the last occurrence ('last' option).
// Stable mode also reorders the unique values by that position.
static ArrayOfVector
applyLastOccurrence(const ArrayOfVector& base, bool asRows, bool stable)
{
    ArrayOf C0 = base[0];
    ArrayOf ia0 = base[1];
    ArrayOf ic0 = base[2];
    indexType G = ia0.elementCount();
    indexType N = ic0.elementCount();
    const double* ic = (const double*)ic0.dataPointer();

    std::vector<indexType> lastIdx(G, 0);
    for (indexType k = 0; k < N; ++k) {
        indexType g = (indexType)ic[k] - 1;
        if (g >= 0 && g < G) {
            lastIdx[g] = k + 1; // k increases, so the final write is the maximum
        }
    }

    ArrayOfVector retval;
    if (!stable) {
        retval << C0;
        retval << doubleIndexVector(lastIdx);
        retval << ic0;
        return retval;
    }

    std::vector<indexType> order(G);
    std::iota(order.begin(), order.end(), (indexType)0);
    std::stable_sort(order.begin(), order.end(),
        [&](indexType a, indexType b) { return lastIdx[a] < lastIdx[b]; });

    std::vector<indexType> rank(G, 0);
    std::vector<indexType> newIa(G, 0);
    for (indexType pos = 0; pos < G; ++pos) {
        rank[order[pos]] = pos;
        newIa[pos] = lastIdx[order[pos]];
    }

    ArrayOf reorderedC;
    ArrayOf permutation = doubleIndexVector([&]() {
        std::vector<indexType> p(G);
        for (indexType pos = 0; pos < G; ++pos) {
            p[pos] = order[pos] + 1;
        }
        return p;
    }());
    if (asRows) {
        indexType cols = C0.columns();
        std::vector<indexType> allCols(cols);
        std::iota(allCols.begin(), allCols.end(), (indexType)1);
        ArrayOfVector idx;
        idx.push_back(permutation);
        idx.push_back(doubleIndexVector(allCols));
        reorderedC = C0.nDimSubset(idx);
    } else {
        reorderedC = C0.vectorSubset(permutation);
    }

    double* newIc = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, N);
    for (indexType k = 0; k < N; ++k) {
        indexType g = (indexType)ic[k] - 1;
        newIc[k] = (double)(rank[g] + 1);
    }

    retval << reorderedC;
    retval << doubleIndexVector(newIa);
    retval << ArrayOf(NLS_DOUBLE, Dimensions(N, 1), newIc);
    return retval;
}
//=============================================================================
// 'legacy' returns ia and ic as row vectors for a row-vector input.
static ArrayOf
toRowVector(const ArrayOf& v)
{
    ArrayOf r(v);
    Dimensions rowDims(1, r.elementCount());
    r.reshape(rowDims);
    return r;
}
//=============================================================================
ArrayOfVector
Nelson::DataAnalysisGateway::uniqueBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // C = unique(A)
    // C = unique(A,'rows')
    // C = unique(A,'stable')
    // C = unique(A,'first') / unique(A,'last')
    // C = unique(A,'legacy') / unique(A,'rows','legacy')
    // C = unique(..., 'TreatMissingAsDistinct', tf)
    //[C,ia,ic] = unique(...)

    ArrayOfVector retval;
    nargincheck(argIn, 1, 6);
    nargoutcheck(nLhs, 0, 3);

    ArrayOf input(argIn[0]);
    // a sparse input is processed densely; the unique values stay sparse
    bool wasSparse = input.isSparse();
    if (wasSparse) {
        input.makeDense();
    }
    bool asRows = false;
    bool stable = false;
    bool lastOccurrence = false;
    bool legacy = false;
    bool modernFlagGiven = false;
    bool sortedGiven = false;
    bool stableGiven = false;
    bool treatMissingAsDistinct = true;
    bool treatMissingGiven = false;

    for (size_t k = 1; k < argIn.size(); ++k) {
        if (!(argIn[k].isRowVectorCharacterArray() || argIn[k].isScalarStringArray())) {
            Error(L"Nelson:data_analysis:uniqueOptionsCharacterVectorsStrings",
                L"unique options must be character vectors or strings");
        }
        std::wstring direction = argIn[k].asWideString();
        if (direction == L"rows") {
            asRows = true;
        } else if (direction == L"stable") {
            stable = true;
            stableGiven = true;
            modernFlagGiven = true;
        } else if (direction == L"sorted") {
            stable = false;
            sortedGiven = true;
            modernFlagGiven = true;
        } else if (direction == L"first") {
            lastOccurrence = false;
            modernFlagGiven = true;
        } else if (direction == L"last") {
            lastOccurrence = true;
            modernFlagGiven = true;
        } else if (direction == L"legacy") {
            legacy = true;
        } else if (isTreatMissingAsDistinctName(direction)) {
            treatMissingAsDistinct = parseTreatMissingAsDistinct(argIn, k + 1);
            treatMissingGiven = true;
            ++k;
        } else {
            Error(L"Nelson:data_analysis:uniqueOptionRowsSortedStable",
                L"unique option must be 'rows', 'sorted', 'stable', 'first', 'last' or 'legacy'");
        }
    }
    if (sortedGiven && stableGiven) {
        Error(L"Nelson:data_analysis:uniqueSortedStableCombination",
            L"You can specify either 'sorted' or 'stable' but not both.");
    }
    if (legacy) {
        // 'legacy': values sorted ascending, ia points at the last occurrence,
        // index outputs follow the input orientation.
        if (modernFlagGiven) {
            Error(L"Nelson:data_analysis:uniqueLegacyCombination",
                L"The 'legacy' flag cannot be combined with the 'sorted', 'stable', 'first' or "
                L"'last' flags.");
        }
        if (treatMissingGiven) {
            Error(L"Nelson:data_analysis:uniqueLegacyTreatMissing",
                L"The 'legacy' flag cannot be combined with 'TreatMissingAsDistinct'.");
        }
        stable = false;
        lastOccurrence = true;
    }
    Dimensions dims(input.dimensions());
    if (asRows && (!dims.is2D())) {
        Error(L"Nelson:data_analysis:rowsModeWorks2DMatrix",
            L"'rows' mode only works for 2D matrix.");
    }

    bool needToOverload = false;
    if (input.isCell() && asRows) {
        Warning(L"Nelson:unique:rowsCellNotSupported",
            L"'rows' option is not supported for cell array inputs.");
        asRows = false;
    }
    if (lastOccurrence || !treatMissingAsDistinct) {
        // The last-occurrence rework and the missing merge need both index
        // outputs (ia and ic).
        ArrayOfVector base = Unique(input, 3, asRows, needToOverload, stable);
        if (needToOverload) {
            OverloadRequired("unique");
        }
        if (!treatMissingAsDistinct) {
            base = UniqueMergeMissing(base, asRows);
        }
        if (lastOccurrence && base.size() >= 3) {
            retval = applyLastOccurrence(base, asRows, stable);
        } else {
            retval = base;
        }
        if (legacy && !asRows && input.isRowVector()) {
            for (size_t k = 1; k < retval.size(); ++k) {
                retval[k] = toRowVector(retval[k]);
            }
        }
    } else {
        retval = Unique(input, nLhs, asRows, needToOverload, stable);
        if (needToOverload) {
            OverloadRequired("unique");
        }
    }
    if ((int)retval.size() > std::max(1, nLhs)) {
        retval.resize(std::max(1, nLhs));
    }
    if (wasSparse && !retval.empty()) {
        retval[0].makeSparse();
    }
    return retval;
}
//=============================================================================
static bool
isTreatMissingAsDistinctName(const std::wstring& name)
{
    const std::wstring expected = L"treatmissingasdistinct";
    return name.size() == expected.size()
        && std::equal(name.begin(), name.end(), expected.begin(),
            [](wchar_t a, wchar_t b) { return (wchar_t)std::towlower(a) == b; });
}
//=============================================================================
static bool
parseTreatMissingAsDistinct(const ArrayOfVector& argIn, size_t valuePosition)
{
    if (valuePosition >= argIn.size()) {
        Error(L"Nelson:data_analysis:uniqueTreatMissingAsDistinctValue",
            L"'TreatMissingAsDistinct' must be followed by a logical scalar.");
    }
    ArrayOf value = argIn[valuePosition];
    if (!value.isScalar() || !(value.isLogical() || value.isNumeric())) {
        Error(L"Nelson:data_analysis:uniqueTreatMissingAsDistinctValue",
            L"'TreatMissingAsDistinct' must be followed by a logical scalar.");
    }
    if (value.isLogical()) {
        return value.asLogicalScalar();
    }
    value.promoteType(NLS_DOUBLE);
    return value.asDoubleScalar() != 0.;
}
//=============================================================================
