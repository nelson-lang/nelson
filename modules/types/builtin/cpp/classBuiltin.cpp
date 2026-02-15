//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "classBuiltin.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TypeGateway::classBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 2);
    if (argIn.size() == 1) {
        std::string str = ClassName(argIn[0]);
        retval << ArrayOf::characterArrayConstructor(str);
        return retval;
    }
    if (argIn.size() == 2) {
        Context* ctx = eval->getContext();
        if (ctx->getCurrentScope()->getName() == "base") {
            raiseError(L"Nelson:types:ERROR_DECLARATION_ONLY_ALLOWED_FROM_CLASS_CONSTRUCTOR",
                ERROR_DECLARATION_ONLY_ALLOWED_FROM_CLASS_CONSTRUCTOR);
        }
        ArrayOf arg1 = ArrayOf(argIn[0]);
        if (arg1.getDataClass() == NLS_STRUCT_ARRAY) {
            arg1.ensureSingleOwner();
            ArrayOf arg2 = argIn[1];
            std::string newType = arg2.getContentAsCString();
            arg1.promoteType(NLS_CLASS_ARRAY);
            arg1.setClassType(newType);
            retval << arg1;
        } else {
            raiseError2(L"nelson:validators:mustBeType", 1, NLS_STRUCT_ARRAY_STR);
        }
    }
    return retval;
}
//=============================================================================
