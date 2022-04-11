//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "narginBuiltin.hpp"
#include "Error.hpp"
#include "NargIn.hpp"
#include "characters_encoding.hpp"
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
            Error(_W("not allowed in base scope."));
        } else {
            int nargin = context->getCurrentScope()->getNargIn();
            retval << ArrayOf::doubleConstructor(nargin);
        }
    } else // argIn.size() == 1
    {
        ArrayOf param1 = argIn[0];
        std::wstring name;
        if (param1.isRowVectorCharacterArray()) {
            name = param1.getContentAsWideString();
        } else if (param1.isFunctionHandle()) {
            function_handle fh = param1.getContentAsFunctionHandle();
            if (fh.anonymous.empty() && fh.name.empty()) {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_FUNCTION_HANDLE_EXPECTED);
            }
            if (fh.anonymous.empty()) {
                name = utf8_to_wstring(fh.name);
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_OR_FUNCTION_HANDLE_EXPECTED);
        }
        retval << ArrayOf::doubleConstructor(NargIn(eval, name));
    }
    return retval;
}
//=============================================================================
