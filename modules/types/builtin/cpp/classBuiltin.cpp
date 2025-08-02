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
    if (argIn.size() == 1) {
        std::string str = ClassName(argIn[0]);
        retval << ArrayOf::characterArrayConstructor(str);
    } else if (argIn.size() == 2) {
        Context* ctx = eval->getContext();
        if (ctx->getCurrentScope()->getName() == "base") {
            Error(_W("This declaration is only allowed from a class constructor."));
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
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRUCT_EXPECTED);
        }
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
