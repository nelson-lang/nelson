//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringPrintf.hpp"
#include "PrintfFunction.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
StringPrintf(
    std::wstring& result, std::wstring& errormsg, Evaluator* eval, const ArrayOfVector& arg)
{
    return printfFunction(arg, errormsg, result);
}
//=============================================================================
}
//=============================================================================
