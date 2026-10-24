//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "composeBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "StringPrintf.hpp"
#include <algorithm>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
struct FormatSpec
{
    size_t start;
    size_t end;
};
//=============================================================================
static std::vector<FormatSpec>
formatSpecs(const std::wstring& format)
{
    const std::wstring conversions = L"diuoxXfFeEgGcs";
    std::vector<FormatSpec> specs;
    for (size_t k = 0; k < format.size(); ++k) {
        if (format[k] != L'%') {
            continue;
        }
        if (k + 1 < format.size() && format[k + 1] == L'%') {
            ++k;
            continue;
        }
        size_t j = k + 1;
        while (j < format.size() && conversions.find(format[j]) == std::wstring::npos) {
            ++j;
        }
        if (j < format.size()) {
            specs.push_back({ k, j + 1 });
            k = j;
        }
    }
    return specs;
}
//=============================================================================
static std::wstring
formatOne(Evaluator* eval, const std::wstring& format, const ArrayOfVector& args)
{
    ArrayOfVector printfArgs;
    printfArgs.push_back(ArrayOf::characterArray(format));
    for (const auto& arg : args) {
        printfArgs.push_back(arg);
    }
    std::wstring result;
    std::wstring errorMessage;
    if (!StringPrintf(result, errorMessage, eval, printfArgs)) {
        Error(L"Nelson:compose:formatFailed", errorMessage);
    }
    return result;
}
//=============================================================================
static ArrayOf
scalarElement(const ArrayOf& value, size_t index)
{
    if (value.isScalar()) {
        return value;
    }
    if (value.isNumeric()) {
        std::vector<double> data = value.asDoubleVector();
        return ArrayOf::doubleScalar(data[std::min(index, data.size() - 1)]);
    }
    if (value.isStringArray() || value.isCellArrayOfCharacterVectors()) {
        wstringVector data = value.asWideStringVector(false);
        return ArrayOf::characterArray(data[std::min(index, data.size() - 1)]);
    }
    return value;
}
//=============================================================================
static std::wstring
formatPartial(Evaluator* eval, const std::wstring& format, const std::vector<double>& values)
{
    std::vector<FormatSpec> specs = formatSpecs(format);
    std::wstring out;
    size_t cursor = 0;
    size_t count = std::min(values.size(), specs.size());
    for (size_t k = 0; k < count; ++k) {
        out.append(format.substr(cursor, specs[k].start - cursor));
        ArrayOfVector args;
        args.push_back(ArrayOf::doubleScalar(values[k]));
        out.append(
            formatOne(eval, format.substr(specs[k].start, specs[k].end - specs[k].start), args));
        cursor = specs[k].end;
    }
    out.append(format.substr(cursor));
    return out;
}
//=============================================================================
static indexType
groupCount(indexType columns, size_t specCount)
{
    size_t perGroup = std::max(specCount, static_cast<size_t>(1));
    return static_cast<indexType>((static_cast<size_t>(columns) + perGroup - 1) / perGroup);
}
//=============================================================================
static ArrayOf
composeMatrix(Evaluator* eval, const std::wstring& format, const ArrayOf& value, size_t specCount)
{
    Dimensions dims = value.dimensions();
    indexType rows = dims.rows();
    indexType cols = dims.columns();
    indexType groups = groupCount(cols, specCount);
    specCount = std::max(specCount, static_cast<size_t>(1));
    std::vector<double> data = value.asDoubleVector();
    wstringVector output;
    output.reserve(static_cast<size_t>(rows * groups));
    for (indexType g = 0; g < groups; ++g) {
        for (indexType r = 0; r < rows; ++r) {
            std::vector<double> values;
            for (size_t s = 0; s < specCount; ++s) {
                indexType c = static_cast<indexType>(g * specCount + s);
                if (c < cols) {
                    values.push_back(data[static_cast<size_t>(r + c * rows)]);
                }
            }
            output.push_back(formatPartial(eval, format, values));
        }
    }
    return ArrayOf::stringArray(output, Dimensions(rows, groups));
}
//=============================================================================
static ArrayOf
emptyResult(const Dimensions& dims, size_t specCount)
{
    return ArrayOf::stringArray(
        wstringVector(), Dimensions(dims.rows(), groupCount(dims.columns(), specCount)));
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::StringGateway::composeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1);
    if (!(argIn[0].isRowVectorCharacterArray() || argIn[0].isStringArray())) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    wstringVector formats = argIn[0].asWideStringVector(false);
    if (argIn[0].isRowVectorCharacterArray()) {
        formats = { argIn[0].asWideString() };
    }
    if (argIn.size() == 1) {
        wstringVector values;
        values.reserve(formats.size());
        for (const auto& format : formats) {
            values.push_back(formatOne(eval, format, ArrayOfVector()));
        }
        ArrayOfVector retval;
        retval << ArrayOf::stringArray(values,
            argIn[0].isStringArray() ? argIn[0].dimensions()
                                     : Dimensions(static_cast<indexType>(values.size()), 1));
        return retval;
    }
    std::wstring format = formats[0];
    size_t specCount = formatSpecs(format).size();
    for (size_t k = 1; k < argIn.size(); ++k) {
        if (argIn[k].isEmpty() && !argIn[k].isCharacterArray()) {
            ArrayOfVector retval;
            retval << emptyResult(argIn[k].dimensions(), specCount);
            return retval;
        }
    }
    if (argIn.size() == 2 && argIn[1].isNumeric() && formats.size() == 1) {
        ArrayOfVector retval;
        retval << composeMatrix(eval, format, argIn[1], specCount);
        return retval;
    }
    size_t count = 1;
    Dimensions outputDims;
    for (size_t k = 1; k < argIn.size(); ++k) {
        if (!argIn[k].isCharacterArray() && static_cast<size_t>(argIn[k].elementCount()) > count) {
            count = static_cast<size_t>(argIn[k].elementCount());
            outputDims = argIn[k].dimensions();
        }
    }
    wstringVector output;
    output.reserve(std::max(count, formats.size()));
    for (size_t i = 0; i < std::max(count, formats.size()); ++i) {
        std::wstring currentFormat = formats[std::min(i, formats.size() - 1)];
        ArrayOfVector args;
        for (size_t k = 1; k < argIn.size(); ++k) {
            args.push_back(scalarElement(argIn[k], i));
        }
        output.push_back(formatOne(eval, currentFormat, args));
    }
    ArrayOfVector retval;
    if (count < formats.size()) {
        outputDims = argIn[0].dimensions();
    } else if (count == 1) {
        outputDims = Dimensions(1, 1);
    }
    retval << ArrayOf::stringArray(output, outputDims);
    return retval;
}
//=============================================================================
