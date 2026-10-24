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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "IsValidVariableName.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
        Error(L"Nelson:persistent:notInFunction",
            L"A 'persistent' declaration is only allowed in a script file function.");
    }
    for (size_t k = 0; k < argIn.size(); k++) {
        if (!argIn[k].isRowVectorCharacterArray()) {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (k + 1),
                L"string");
        }
        std::string arg = argIn[k].asUtf8String();
        if (!IsValidVariableName(arg)) {
            Error(L"Nelson:memory_manager:invalidVariableName",
                L"#%d argument must contain a valid variable name.", (k + 1));
        }
        if (context->isLockedVariable(arg)) {
            Error(L"Nelson:memory_manager:variableLocked", L"variable is locked: %s",
                utf8_to_wstring(arg));
        }
    }
    for (const auto& k : argIn) {
        std::string arg = k.asUtf8String();
        context->addPersistentVariable(arg);
    }
    return retval;
}
//=============================================================================
