//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "builtinBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::builtinBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    Context* context = eval->getContext();
    FunctionDef* funcDef = nullptr;
    ArrayOf param1 = argIn[0];
    std::string fname = argIn[0].getContentAsCString();
    if (!context->lookupFunction(fname, funcDef, true)) {
        Error(_W("function \'") + utf8_to_wstring(fname) + _W("\' is not a builtin."));
    }
    ArrayOfVector newarg(argIn);
    newarg.pop_front();
    ArrayOfVector retval;
    try {
        eval->withOverload = false;
        retval = funcDef->evaluateFunction(eval, newarg, nLhs);
        eval->withOverload = true;
    } catch (const Exception&) {
        eval->withOverload = true;
        throw;
    }

    return retval;
}
//=============================================================================
