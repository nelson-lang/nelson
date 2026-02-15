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
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "persistentBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "IsValidVariableName.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
#include "characters_encoding.hpp"
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
        raiseError(L"Nelson:memory_manager:ERROR_A_PERSISTENT_DECLARATION_IS_ONLY_ALLOWED_IN_A_"
                   L"SCRIPT_FILE_FUNCTION",
            ERROR_A_PERSISTENT_DECLARATION_IS_ONLY_ALLOWED_IN_A_SCRIPT_FILE_FUNCTION);
    }
    for (size_t k = 0; k < argIn.size(); k++) {
        if (!argIn[k].isRowVectorCharacterArray()) {
            raiseError2(L"nelson:validators:mustBeType", k + 1, NLS_STRING_ARRAY_STR);
        }
        std::string arg = argIn[k].getContentAsCString();
        if (!IsValidVariableName(arg)) {
            raiseError(L"Nelson:memory_manager:ERROR_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME",
                ERROR_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME);
        }
        if (context->isLockedVariable(arg)) {
            raiseError(L"Nelson:memory_manager:ERROR_VARIABLE_IS_LOCKED", ERROR_VARIABLE_IS_LOCKED);
        }
    }
    for (const auto& k : argIn) {
        std::string arg = k.getContentAsCString();
        context->addPersistentVariable(arg);
    }
    return retval;
}
//=============================================================================
