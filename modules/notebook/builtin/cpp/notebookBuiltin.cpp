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
#include "NotebookEditor.hpp"
#include "i18n.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NotebookReader.hpp"
#include "EvaluateCommand.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// notebook() opens a new document, notebook(file) runs it, notebook(file, 'edit') opens it.
ArrayOfVector
Nelson::NotebookGateway::notebookBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 2);
    nargoutcheck(nLhs, 0, 1);
    const std::wstring filename = argIn.empty() ? std::wstring() : argIn[0].asWideString();
    bool edit = argIn.empty();
    if (argIn.size() == 2) {
        if (!argIn[1].isRowVectorCharacterArray() || argIn[1].asWideString() != L"edit") {
            Error(_W("Wrong value for #2 argument."));
        }
        edit = true;
    }
    if (edit) {
        if (!openNotebookEditor(eval, filename)) {
            Error(L"Nelson:notebook:editorUnavailable",
                _W("The Notebook editor requires the Nelson desktop."));
        }
    } else {
        const wstringVector cells = ReadNotebookCode(filename);
        for (const auto& code : cells) {
            if (code.find_first_not_of(L" \t\r\n") == std::wstring::npos) {
                continue;
            }
            EvaluateCommand(eval, code, false);
        }
    }
    if (nLhs > 0) {
        retval << ArrayOf::logicalScalar(true);
    }
    return retval;
}
//=============================================================================
