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
#include "NargIn.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "MacroFunctionDef.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
int
NargIn(Evaluator* eval, const std::wstring& functionName)
{
    FunctionDefPtr fptr = nullptr;
    bool bIsFun = eval->lookupFunction(wstring_to_utf8(functionName), fptr);
    if (bIsFun) {
        if (fptr->type() == NLS_MACRO_FUNCTION) {
            return ((MacroFunctionDef*)(fptr))->nargin();
        }
        if (fptr->type() == NLS_MEX_FUNCTION) {
            std::string msg
                = formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:core:doesKnowHowAnswer",
                                        "'%s' does not know how to answer nargin/nargout."),
                    fptr->getName());
            Error("Nelson:nargin:notSupported", msg);
        }
        return fptr->inputArgCount();
    }
    Error(L"Nelson:core:functionNotFound", L"Function not found.");
    return -1;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
