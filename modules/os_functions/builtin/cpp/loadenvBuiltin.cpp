//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "loadenvBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "LoadEnvironment.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OsFunctionsGateway::loadenvBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);

    std::wstring filename = argIn[0].getContentAsWideString();
    bool applyToEnv = (nLhs == 0);

    std::wstring errorMessage;
    std::vector<std::pair<std::wstring, std::wstring>> result;
    try {
        result = LoadEnvironment(filename, applyToEnv, errorMessage);
    } catch (const std::exception& ex) {
        raiseError(
            L"Nelson:os_functions:ERROR_EXCEPTION", ERROR_EXCEPTION, utf8_to_wstring(ex.what()));
        return retval;

    } catch (...) {
        if (errorMessage.empty()) {
            raiseError(L"Nelson:os_functions:ERROR_UNKNOWN_ERROR_LOADING_ENVIRONMENT",
                ERROR_UNKNOWN_ERROR_LOADING_ENVIRONMENT);
            return retval;
        }
    }

    if (!errorMessage.empty()) {
        raiseError(L"Nelson:os_functions:ERROR_EXCEPTION", ERROR_EXCEPTION, errorMessage);
        return retval;
    }

    if (nLhs == 1) {
        wstringVector keys;
        wstringVector values;
        keys.reserve(result.size());
        values.reserve(result.size());
        for (const auto& kv : result) {
            keys.push_back(kv.first);
            values.push_back(kv.second);
        }

        FunctionDef* funcDef = nullptr;
        if (!eval) {
            raiseError(L"Nelson:os_functions:ERROR_EVALUATOR_IS_NOT_INITIALIZED",
                ERROR_EVALUATOR_IS_NOT_INITIALIZED);
        }
        Context* context = eval->getContext();
        if (!context) {
            raiseError(L"Nelson:os_functions:ERROR_CONTEXT_IS_NOT_INITIALIZED",
                ERROR_CONTEXT_IS_NOT_INITIALIZED);
        }
        if (context->lookupFunction("dictionary", funcDef)) {
            ArrayOfVector args;
            args << ArrayOf::stringArrayConstructor(keys, Dimensions(keys.size(), 1));
            args << ArrayOf::stringArrayConstructor(values, Dimensions(values.size(), 1));
            return funcDef->evaluateFunction(eval, args, 1);
        } else {
            raiseError(
                L"Nelson:os_functions:ERROR_FUNCTION_DICTIONARY_NOT_FOUND_IN_THE_CURRENT_CONTEXT",
                ERROR_FUNCTION_DICTIONARY_NOT_FOUND_IN_THE_CURRENT_CONTEXT);
        }
    }
    return retval;
}
//=============================================================================
