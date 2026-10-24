//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__rng_eval__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Rng.hpp"
#include "Context.hpp"
#include "FunctionDef.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <algorithm>
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
evaluateFunction(Evaluator* eval, const ArrayOf& name, const ArrayOfVector& args, int nLhs);
//=============================================================================
// [settings, varargout] = __rng_eval__(settings, name, varargin)
// Calls name(varargin{:}) with a private engine built from settings (the
// Type, Seed, State struct returned by rng; an empty State means seeded
// only) and returns the engine settings after the call. The current engine
// is never read nor modified.
ArrayOfVector
Nelson::RandomGateway::__rng_eval__Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    ArrayOfVector outputs;
    ArrayOf settings;
    {
        RngEngineScope scope(argIn[0]);
        if (argIn.size() > 1) {
            if (!argIn[1].isRowVectorCharacterArray()) {
                Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                    L"string");
            }
            ArrayOfVector args(argIn);
            args.pop_front();
            args.pop_front();
            outputs = evaluateFunction(eval, argIn[1], args, std::max(nLhs - 1, 0));
        }
        settings = RngGetSettings();
    }
    ArrayOfVector retval;
    retval << settings;
    for (size_t k = 0; k < outputs.size() && (int)retval.size() < nLhs; ++k) {
        retval << outputs[k];
    }
    return retval;
}
//=============================================================================
// A builtin is called without overload dispatch (as builtin() does), any
// other function as a regular call.
static ArrayOfVector
evaluateFunction(Evaluator* eval, const ArrayOf& name, const ArrayOfVector& args, int nLhs)
{
    Context* context = eval->getContext();
    FunctionDef* funcDef = nullptr;
    const std::string functionName = name.asUtf8String();
    if (!context->lookupFunction(functionName, funcDef, true)) {
        if (!context->lookupFunction(functionName, funcDef)) {
            Error(L"Nelson:UndefinedFunction:undefinedVariableFunctionValue",
                L"Undefined variable or function: %s", name.asWideString());
        }
        return funcDef->evaluateFunction(eval, args, nLhs);
    }
    bool withOverload = eval->withOverload;
    eval->withOverload = false;
    try {
        ArrayOfVector outputs = funcDef->evaluateFunction(eval, args, nLhs);
        eval->withOverload = withOverload;
        return outputs;
    } catch (const NelsonException&) {
        eval->withOverload = withOverload;
        throw;
    }
}
//=============================================================================
