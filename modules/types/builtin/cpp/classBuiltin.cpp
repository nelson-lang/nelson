//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "classBuiltin.hpp"
#include "OverloadFunction.hpp"
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
        bool bSuccess = false;
        if (eval->mustOverloadBasicTypes()) {
            retval = OverloadFunction(eval, nLhs, argIn, "class", bSuccess);
        }
        if (!bSuccess) {
            if (argIn[0].isClassStruct() || argIn[0].isHandle() || argIn[0].isGraphicsObject()) {
                retval = OverloadFunction(eval, nLhs, argIn, "class", bSuccess);
                if (bSuccess) {
                    return retval;
                }
            }
            std::string str = ClassName(argIn[0]);
            retval << ArrayOf::characterArrayConstructor(str);
        }
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
            if ((newType == NLS_SPARSE_STR) || (newType == NLS_CELL_ARRAY_STR)
                || (newType == NLS_STRING_ARRAY_STR) || (newType == NLS_STRUCT_ARRAY_STR)
                || (newType == NLS_LOGICAL_STR) || (newType == NLS_UINT8_STR)
                || (newType == NLS_INT8_STR) || (newType == NLS_UINT16_STR)
                || (newType == NLS_INT16_STR) || (newType == NLS_UINT32_STR)
                || (newType == NLS_INT32_STR) || (newType == NLS_UINT64_STR)
                || (newType == NLS_INT64_STR) ||
                //  (newType == NLS_DOUBLE_STR) ||
                //	(newType == NLS_SINGLE_STR) ||
                (newType == NLS_SCOMPLEX_STR) || (newType == NLS_DCOMPLEX_STR)
                || (newType == NLS_CHAR_STR) || (newType == NLS_FUNCTION_HANDLE_STR)
                || (newType == NLS_GO_HANDLE_STR) || (newType == NLS_HANDLE_STR)
                || (newType == NLS_GENERIC_STR)) {
                Error(ERROR_TYPE_ALREADY_RESERVED);
            }
            arg1.setStructType(newType);
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
