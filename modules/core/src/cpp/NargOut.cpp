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
#include <fmt/xchar.h>
#include "NargOut.hpp"
#include "Error.hpp"
#include "i18n.hpp"
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
            std::string msg = fmt::sprintf(
                _("'%s' does not know how to answer nargin/nargout."), fptr->getName());
            Error(msg);
        }
        return fptr->outputArgCount();
    }
    Error(_W("function not found."));
    return -1;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
