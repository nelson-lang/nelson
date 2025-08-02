//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include "persistentBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "IsValidVariableName.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MemoryGateway::persistentBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    Context* context = eval->getContext();
    if (context->getCurrentScope()->getName() == "base") {
        Error(_W("A 'persistent' declaration is only allowed in a script file function."));
    }
    for (size_t k = 0; k < argIn.size(); k++) {
        if (!argIn[k].isRowVectorCharacterArray()) {
            Error(fmt::sprintf(ERROR_WRONG_ARGUMENT_X_TYPE_STRING_EXPECTED, k + 1));
        }
        std::string arg = argIn[k].getContentAsCString();
        if (!IsValidVariableName(arg)) {
            Error(_W("Argument must contain a valid variable name."));
        }
        if (context->isLockedVariable(arg)) {
            Error(_W("variable is locked."));
        }
    }
    for (const auto& k : argIn) {
        std::string arg = k.getContentAsCString();
        context->addPersistentVariable(arg);
    }
    return retval;
}
//=============================================================================
