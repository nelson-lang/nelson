//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "structfunBuiltin.hpp"
#include "cellfunBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NewWithException.hpp"
#include "StringHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// case-insensitive prefix abbreviations of the option names
// 'UniformOutput' and 'ErrorHandler' (at least two characters, e.g. 'un').
static bool
matchesFunctionHandleOption(const std::wstring& argName, const std::wstring& fullName)
{
    if (argName.size() < 2 || argName.size() > fullName.size()) {
        return false;
    }
    return StringHelpers::iequals(argName, fullName.substr(0, argName.size()));
}
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::structfunBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, -1);
    ArrayOf funArg = argIn[0];
    ArrayOf s = argIn[1];
    if (!s.isStruct() || !s.isScalar()) {
        Error(L"Nelson:data_structures:secondArgumentScalarStructure",
            L"Second argument must be a scalar structure.");
    }
    if (!funArg.isFunctionHandle()) {
        Error(L"Nelson:data_structures:firstArgumentFunctionHandle",
            L"First argument must be a function handle.");
    }
    bool uniformOutput = true;
    for (size_t k = 2; k < argIn.size(); k += 2) {
        if (k + 1 >= argIn.size()) {
            Error(L"Nelson:data_structures:optionsNameValuePairs",
                L"Options must be name-value pairs.");
        }
        if (!argIn[k].isRowVectorCharacterArray() && !argIn[k].isScalarStringArray()) {
            Error(L"Nelson:data_structures:optionsNameValuePairs",
                L"Options must be name-value pairs.");
        }
        std::wstring name = argIn[k].asWideString();
        if (matchesFunctionHandleOption(name, L"UniformOutput")) {
            ArrayOf value = argIn[k + 1];
            uniformOutput = value.isLogical() ? (value.asLogicalScalar() != 0)
                                              : (value.asDoubleScalar() != 0);
        } else if (!matchesFunctionHandleOption(name, L"ErrorHandler")) {
            Error(L"Nelson:data_structures:unknownOptionName", L"Unknown option name.");
        }
    }
    // structfun(f, s, options) is cellfun(f, struct2cell(s), options): the
    // fields as a column cell; a non-uniform output is wrapped back in a struct.
    stringVector fns = s.fieldNames();
    auto* elements = new_with_exception<ArrayOf>(fns.size(), false);
    for (size_t i = 0; i < fns.size(); ++i) {
        elements[i] = s.field(fns[i]);
    }
    ArrayOfVector cellfunArgs;
    cellfunArgs.reserve(argIn.size());
    cellfunArgs.push_back(funArg);
    cellfunArgs.push_back(ArrayOf(NLS_CELL_ARRAY, Dimensions(fns.size(), 1), elements));
    for (size_t k = 2; k < argIn.size(); ++k) {
        cellfunArgs.push_back(argIn[k]);
    }
    ArrayOfVector retval = cellfunBuiltin(eval, nLhs, cellfunArgs);
    if (!uniformOutput) {
        for (auto& output : retval) {
            ArrayOfVector values;
            values.reserve(fns.size());
            const auto* cells = static_cast<const ArrayOf*>(output.dataPointer());
            for (size_t i = 0; i < fns.size(); ++i) {
                values.push_back(cells[i]);
            }
            output = ArrayOf::structScalar(fns, values);
        }
    }
    return retval;
}
//=============================================================================
