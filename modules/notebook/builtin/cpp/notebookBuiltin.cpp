//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "notebookBuiltin.hpp"
#include "Error.hpp"
#include "NotebookReader.hpp"
#include "EvaluateCommand.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// notebook('file.nlx' | 'file.m')
// Run a Notebook document headless: evaluate each of its code cells, in
// order, against the base workspace (variables persist from one cell to the
// next, like running the document). Errors propagate to the caller.
ArrayOfVector
Nelson::NotebookGateway::notebookBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    const std::wstring filename = argIn[0].asWideString();
    const wstringVector cells = ReadNotebookCode(filename);
    for (const auto& code : cells) {
        if (code.find_first_not_of(L" \t\r\n") == std::wstring::npos) {
            continue;
        }
        EvaluateCommand(eval, code, false);
    }
    if (nLhs > 0) {
        retval << ArrayOf::logicalScalar(true);
    }
    return retval;
}
//=============================================================================
