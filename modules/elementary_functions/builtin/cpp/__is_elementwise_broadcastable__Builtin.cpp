//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__is_elementwise_broadcastable__Builtin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "AnonymousElementwiseAnalyzer.hpp"
#include "FunctionDef.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// Internal helper used by bsxfun: returns true when the argument is an anonymous
// function handle whose body is a provably element-wise expression that is also
// safe to evaluate once on broadcasting operands (see the interpreter's
// isAnonymousBodyElementwiseBroadcastSafe). It lets bsxfun skip the repmat
// materialization of its operands and rely on Nelson's operator broadcasting.
// Any other handle (named handles, non element-wise or non-broadcasting bodies)
// yields false, so the caller keeps its unchanged general path.
ArrayOfVector
Nelson::ElementaryFunctionsGateway::__is_elementwise_broadcastable__Builtin(
    int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    bool result = false;
    if (argIn[0].isFunctionHandle()) {
        function_handle fh = argIn[0].asFunctionHandle();
        auto* fptr = reinterpret_cast<FunctionDef*>(fh.anonymousHandle);
        if (fptr != nullptr && fptr->type() == NLS_ANONYMOUS_MACRO_FUNCTION) {
            auto* def = dynamic_cast<AnonymousMacroFunctionDef*>(fptr);
            if (def != nullptr) {
                result = isAnonymousBodyElementwiseBroadcastSafe(def);
            }
        }
    }
    ArrayOfVector retval(1);
    retval << ArrayOf::logicalScalar(result);
    return retval;
}
//=============================================================================
