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
#include "NargOut.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "MacroFunctionDef.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
int
NargOut(Evaluator* eval, const std::wstring& functionName)
{
    FunctionDefPtr fptr = nullptr;
    bool bIsFun = eval->lookupFunction(wstring_to_utf8(functionName), fptr);
    if (bIsFun) {
        if (fptr->type() == NLS_MACRO_FUNCTION) {
            return ((MacroFunctionDef*)(fptr))->nargout();
        }
        if (fptr->type() == NLS_MEX_FUNCTION) {
            raiseError(L"Nelson:core:ERROR_FUNCTION_DOES_NOT_KNOW_HOW_TO_ANSWER_NARGIN_NARGOUT",
                ERROR_FUNCTION_DOES_NOT_KNOW_HOW_TO_ANSWER_NARGIN_NARGOUT,
                utf8_to_wstring(fptr->getName()));
        }
        return fptr->outputArgCount();
    }
    raiseError2(_E("nelson:runtime:functionNotFound"), functionName);
    return -1;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
