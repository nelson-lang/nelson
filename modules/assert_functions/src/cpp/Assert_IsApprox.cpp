//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Assert_IsApprox.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
Assert_IsApprox(Evaluator* eval, const ArrayOf& computedArray, const ArrayOf& expectedArray,
    double precision, std::wstring& msg)
{
    bool bRes = false;
    Context* context = eval->getContext();
    FunctionDef* funcDef = nullptr;
    std::string IsApproxName = "isapprox";
    if (context->lookupFunction(IsApproxName, funcDef)) {
        if ((funcDef->type() == NLS_BUILT_IN_FUNCTION) || (funcDef->type() == NLS_MACRO_FUNCTION)) {
            ArrayOfVector argInCopy(3);
            argInCopy << computedArray;
            argInCopy << expectedArray;
            argInCopy << ArrayOf::doubleConstructor(precision);
            try {
                ArrayOfVector resVect = funcDef->evaluateFunction(eval, argInCopy, 1);
                if (resVect.size() != 1) {
                    Error(_W("isapprox returns more than one output argument."));
                }
                ArrayOf r = resVect[0];
                if (r.isScalar() && r.isLogical()) {
                    bRes = r.getContentAsLogicalScalar() ? true : false;
                } else {
                    Error(_W("isapprox must return an logical."));
                }
            } catch (const Exception&) {
                throw;
            }
        }
    } else {
        Error("isapprox function not found.");
    }
    if (!bRes) {
        msg = _W("Assertion failed: expected and computed values are too different.");
    } else {
        msg.clear();
    }
    return bRes;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
