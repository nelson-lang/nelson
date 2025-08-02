//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Cast.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
evaluateFunction(
    Evaluator* eval, const ArrayOf& arrayIn, const std::string& functionName, bool& succeeded)
{
    ArrayOf res = {};
    FunctionDef* funcDef = nullptr;
    succeeded = false;

    Context* context = eval->getContext();
    context->lookupFunction(functionName, funcDef);
    if (funcDef) {
        ArrayOfVector argsIn;
        argsIn << arrayIn;
        ArrayOfVector r = funcDef->evaluateFunction(eval, argsIn, 1);
        res = r[0];
        succeeded = true;
    }
    return res;
}
//=============================================================================
ArrayOf
Cast(Evaluator* eval, const ArrayOf& arrayIn, const std::string& classname, bool isComplex,
    bool isSparse, bool withCheck)
{
    FunctionDef* funcDef = nullptr;
    std::string functionName = isSparse ? "sparse" : classname;

    bool succeeded = false;
    ArrayOf res = evaluateFunction(eval, arrayIn, functionName, succeeded);
    if (!succeeded) {
        Error(_W("Unsupported data conversion."), L"Nelson:cast:UnsupportedPrototype");
    }
    if (isComplex) {
        res = evaluateFunction(eval, res, "complex", succeeded);
    }
    if (!succeeded || (withCheck && (ClassName(res) != classname))) {
        Error(_W("Unsupported data conversion."), L"Nelson:cast:UnsupportedPrototype");
    }
    return res;
}
//=============================================================================
ArrayOf
Cast(Evaluator* eval, const ArrayOf& arrayIn, const ArrayOf& likeThis)
{
    return Cast(
        eval, arrayIn, ClassName(likeThis), likeThis.isComplex(), likeThis.isSparse(), false);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
