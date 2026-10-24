//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "narginBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NargIn.hpp"
#include "characters_encoding.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::narginBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 0, 1);
    if (argIn.empty()) {
        Context* context = eval->getContext();
        if (context->getCurrentScope()->getName() == "base") {
            Error(L"Nelson:core:notAllowedInBaseScope", L"not allowed in base scope.");
        } else {
            int nargin = context->getCurrentScope()->getNargIn();
            retval << ArrayOf::doubleScalar(nargin);
        }
    } else {
        ArrayOf param1 = argIn[0];
        if (param1.isRowVectorCharacterArray()) {
            std::wstring name = param1.asWideString();
            retval << ArrayOf::doubleScalar(NargIn(eval, name));
        } else if (param1.isFunctionHandle()) {
            function_handle fh = param1.asFunctionHandle();
            if (fh.anonymousHandle == nullptr) {
                Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                    L"function handle");
            }
            AnonymousMacroFunctionDef* anonymousFunction
                = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
            if (anonymousFunction) {
                if (anonymousFunction->isBoundNestedFunctionHandle()) {
                    // A handle to a nested function is not resolvable by name from
                    // this scope; introspect the bound function directly.
                    retval << ArrayOf::doubleScalar(anonymousFunction->boundFunctionNargin());
                } else if (anonymousFunction->isFunctionHandle()) {
                    retval << ArrayOf::doubleScalar(
                        NargIn(eval, utf8_to_wstring(anonymousFunction->getName())));
                } else {
                    retval << ArrayOf::doubleScalar(anonymousFunction->nargin());
                }
            }
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string or function handle");
        }
    }
    return retval;
}
//=============================================================================
