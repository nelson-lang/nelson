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
#include "ErrorID.hpp"
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

    std::wstring filename = argIn[0].asWideString();
    bool applyToEnv = (nLhs == 0);

    std::wstring errorMessage;
    std::vector<std::pair<std::wstring, std::wstring>> result;
    try {
        result = LoadEnvironment(filename, applyToEnv, errorMessage);
    } catch (const std::exception& ex) {
        errorMessage
            = utf8_to_wstring(std::string("NelsonException: ")) + utf8_to_wstring(ex.what());
    } catch (...) {
        if (errorMessage.empty()) {
            errorMessage = resolveMessageTemplate(L"Nelson:os_functions:unknownErrorWhileLoading",
                L"Unknown error while loading environment");
        }
    }

    if (!errorMessage.empty()) {
        Error(L"Nelson:loadenv:loadFailed", errorMessage);
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
            Error(L"Nelson:loadenv:evaluatorNotInitialized", L"Evaluator is not initialized.");
        }
        Context* context = eval->getContext();
        if (!context) {
            Error(L"Nelson:loadenv:contextNotInitialized", L"Context is not initialized.");
        }
        if (context->lookupFunction("dictionary", funcDef)) {
            ArrayOfVector args;
            args << ArrayOf::stringArray(keys, Dimensions(keys.size(), 1));
            args << ArrayOf::stringArray(values, Dimensions(values.size(), 1));
            return funcDef->evaluateFunction(eval, args, 1);
        } else {
            Error(L"Nelson:loadenv:dictionaryNotFound",
                L"Function 'dictionary' not found in the current context.");
        }
    }
    return retval;
}
//=============================================================================
