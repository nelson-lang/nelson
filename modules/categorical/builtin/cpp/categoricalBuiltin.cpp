//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "categoricalBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <unordered_map>
#include <unordered_set>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
bool
isAsciiBlank(char ch)
{
    return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' || ch == '\f' || ch == '\v';
}
//=============================================================================
std::string
trimAscii(const std::string& value)
{
    size_t first = 0;
    while (first < value.size() && isAsciiBlank(value[first])) {
        ++first;
    }
    size_t last = value.size();
    while (last > first && isAsciiBlank(value[last - 1])) {
        --last;
    }
    return value.substr(first, last - first);
}
//=============================================================================
ArrayOf
cellColumnFromStrings(const std::vector<std::string>& values)
{
    Dimensions dims(static_cast<indexType>(values.size()), 1);
    if (values.empty()) {
        return ArrayOf(NLS_CELL_ARRAY, dims, nullptr);
    }
    auto* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, values.size(), stringVector(), false));
    for (size_t k = 0; k < values.size(); ++k) {
        elements[k] = ArrayOf::characterArray(values[k]);
    }
    return ArrayOf(NLS_CELL_ARRAY, dims, elements);
}
//=============================================================================
} // namespace
//=============================================================================
static const double*
getDoubleCodes(const ArrayOf& value, size_t& count)
{
    if (value.dataClass() != NLS_DOUBLE || value.isSparse()) {
        Error(L"Nelson:categorical:codesMustBeFullDouble",
            L"Categorical codes must be a full double array.");
    }
    count = value.elementCount();
    return static_cast<const double*>(value.dataPointer());
}
//=============================================================================
ArrayOfVector
Nelson::CategoricalGateway::categorical_cellstr_codesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 3);
    const ArrayOf& input = argIn[0];
    if (!input.isCell()) {
        retval << ArrayOf::doubleScalar(0);
        retval << ArrayOf::empty();
        retval << ArrayOf::logicalScalar(false);
        return retval;
    }

    const indexType elementCount = input.elementCount();
    const auto* elements = static_cast<const ArrayOf*>(input.dataPointer());
    std::vector<std::string> values;
    values.reserve(static_cast<size_t>(elementCount));
    std::vector<std::string> categories;
    categories.reserve(static_cast<size_t>(elementCount));
    std::unordered_set<std::string> seenCategories;
    seenCategories.reserve(static_cast<size_t>(std::min<indexType>(elementCount, 256)));

    for (indexType k = 0; k < elementCount; ++k) {
        if (!elements[k].isRowVectorCharacterArray()) {
            retval << ArrayOf::doubleScalar(0);
            retval << ArrayOf::empty();
            retval << ArrayOf::logicalScalar(false);
            return retval;
        }
        std::string value = trimAscii(elements[k].asUtf8String());
        if (value.empty()) {
            retval << ArrayOf::doubleScalar(0);
            retval << ArrayOf::empty();
            retval << ArrayOf::logicalScalar(false);
            return retval;
        }
        values.push_back(value);
        if (seenCategories.insert(value).second) {
            categories.push_back(value);
        }
    }

    std::sort(categories.begin(), categories.end());
    std::unordered_map<std::string, indexType> categoryIndex;
    categoryIndex.reserve(categories.size());
    for (size_t k = 0; k < categories.size(); ++k) {
        categoryIndex.emplace(categories[k], static_cast<indexType>(k + 1));
    }

    auto* codes = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, elementCount, stringVector(), false));
    for (indexType k = 0; k < elementCount; ++k) {
        codes[k] = static_cast<double>(categoryIndex[values[static_cast<size_t>(k)]]);
    }

    retval << ArrayOf(NLS_DOUBLE, input.dimensions(), codes);
    retval << cellColumnFromStrings(categories);
    retval << ArrayOf::logicalScalar(true);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::CategoricalGateway::categorical_codes_isundefinedBuiltin(
    int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    size_t count = 0;
    const double* codes = getDoubleCodes(argIn[0], count);
    auto* out = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, count, stringVector(), false));
    for (size_t k = 0; k < count; ++k) {
        out[k] = (codes[k] == 0) ? 1 : 0;
    }
    retval << ArrayOf(NLS_LOGICAL, argIn[0].dimensions(), out);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::CategoricalGateway::categorical_codes_countBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    size_t count = 0;
    const double* codes = getDoubleCodes(argIn[0], count);
    indexType nbCategories = argIn[1].asScalarIndex(false, true);
    Dimensions dims(nbCategories, 1);
    auto* out = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, nbCategories, stringVector(), true));
    for (size_t k = 0; k < count; ++k) {
        double code = codes[k];
        if (code >= 1 && code <= static_cast<double>(nbCategories)) {
            out[static_cast<indexType>(code) - 1] += 1;
        }
    }
    retval << ArrayOf(NLS_DOUBLE, dims, out);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::CategoricalGateway::categorical_codes_compareBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 1);
    size_t leftCount = 0;
    size_t rightCount = 0;
    const double* left = getDoubleCodes(argIn[0], leftCount);
    const double* right = getDoubleCodes(argIn[1], rightCount);
    if (leftCount != rightCount && leftCount != 1 && rightCount != 1) {
        Error(L"Nelson:dimagree:valueCompatibleSizes", L"%s must have compatible sizes.",
            L"Categorical code arrays");
    }
    std::wstring op = argIn[2].asWideString();
    size_t outCount = leftCount > rightCount ? leftCount : rightCount;
    Dimensions outDims = leftCount == 1 ? argIn[1].dimensions() : argIn[0].dimensions();
    auto* out = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, outCount, stringVector(), false));
    for (size_t k = 0; k < outCount; ++k) {
        double a = left[leftCount == 1 ? 0 : k];
        double b = right[rightCount == 1 ? 0 : k];
        bool value = false;
        if (a != 0 && b != 0) {
            if (op == L"eq") {
                value = a == b;
            } else if (op == L"ne") {
                value = a != b;
            } else if (op == L"lt") {
                value = a < b;
            } else if (op == L"le") {
                value = a <= b;
            } else if (op == L"gt") {
                value = a > b;
            } else if (op == L"ge") {
                value = a >= b;
            } else {
                Error(L"Nelson:categorical:unsupportedComparison",
                    L"Unsupported categorical comparison operator.");
            }
        } else if (op == L"ne") {
            value = (a != b);
        }
        out[k] = value ? 1 : 0;
    }
    retval << ArrayOf(NLS_LOGICAL, outDims, out);
    return retval;
}
//=============================================================================
