//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "websaveBuiltin.hpp"
#include "Error.hpp"
#include "WebSave.hpp"
#include "WebOptions.hpp"
#include "ClassName.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
getWebOptions(Evaluator* eval, const ArrayOfVector& argIn, bool& haveExplicitOptions)
{
    ArrayOf webOptionsArrayOf;
    if (argIn.size() % 2 == 0) {
        haveExplicitOptions = false;
        Context* ctx = eval->getContext();
        FunctionDef* funcDef = nullptr;
        bool isFun = ctx->lookupFunction("weboptions", funcDef);
        if (!isFun) {
            Error(_W("weboptions not found."));
        }
        int nargout = 1;
        ArrayOfVector input;
        ArrayOfVector ret;
        std::vector<Nelson::StackEntry> stack = eval->cstack;
        try {
            ret = funcDef->evaluateFunction(eval, input, nargout);
            if (ret.size() != 1) {
                Error(_W("Invalid LHS."));
            }
        } catch (Exception&) {
            Error(_W("weboptions call fails."));
        }
        eval->cstack = stack;
        webOptionsArrayOf = ret[0];
    } else {
        haveExplicitOptions = true;
        webOptionsArrayOf = argIn[argIn.size() - 1];
    }
    return webOptionsArrayOf;
}
//=============================================================================
static void
checkQueryNameValue(const ArrayOfVector& argIn, bool haveExplicitOptions, ArrayOfVector& names,
    ArrayOfVector& values)
{
    ArrayOfVector queryValueName;
    size_t idxOptions = haveExplicitOptions ? 1 : 0;
    for (size_t k = 2; k < argIn.size() - idxOptions; k = k + 2) {
        ArrayOf paramName = argIn[k];
        ArrayOf paramValue = argIn[k + 1];

        std::wstring name = paramName.getContentAsWideString();

        names.push_back(paramName);
        if (paramValue.isSparse()) {
            Error(_W("Sparse not supported."));
        }
        if (paramValue.isCharacterArray() || paramValue.isStringArray()) {
            std::wstring val = paramValue.getContentAsWideString();
            values.push_back(ArrayOf::characterArrayConstructor(val));
        } else if (paramValue.isLogical() || paramValue.isNumeric()) {
            values.push_back(paramValue);
        } else {
            Error(_W("Type not supported. Only char, string, numeroc or logical allowed."));
        }
    }
}
//=============================================================================
ArrayOfVector
Nelson::WebtoolsGateway::websaveBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() < 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    bool haveExplicitOptions;
    ArrayOf webOptionsArrayOf = getWebOptions(eval, argIn, haveExplicitOptions);
    WebOptions options(webOptionsArrayOf);

    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring url = param2.getContentAsWideString();
    std::wstring filename = param1.getContentAsWideString();

    ArrayOfVector names;
    ArrayOfVector values;
    checkQueryNameValue(argIn, haveExplicitOptions, names, values);
    ArrayOf res = WebSave(url, filename, names, values, options, eval->haveEventsLoop());
    retval.push_back(res);
    return retval;
}
//=============================================================================
