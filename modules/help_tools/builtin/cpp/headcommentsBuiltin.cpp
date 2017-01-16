//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "headcommentsBuiltin.hpp"
#include "Error.hpp"
#include "HeadComments.hpp"
#include "characters_encoding.hpp"
#include "MacroFunctionDef.hpp"
#include "ToCellString.hpp"
#include "IsFile.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::HelpToolsGateway::headcommentsBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    std::wstring filename = L"";
    if (argIn.size() == 1)
    {
        ArrayOf arg1 = argIn[0];
        std::wstring functionName = L"";
        if (arg1.isSingleString())
        {
            functionName = arg1.getContentsAsWideString();
            if (IsFile(functionName))
            {
                filename = functionName;
            }
            else
            {
                Context *context = eval->getContext();
                FunctionDef *funcDef = nullptr;
                if (context->lookupFunction(wstring_to_utf8(functionName), funcDef))
                {
                    if (funcDef->type() == NLS_MACRO_FUNCTION)
                    {
                        MacroFunctionDef *fm = (MacroFunctionDef *)funcDef;
                        filename = fm->fileName;
                    }
                    else
                    {
                        Error(eval, _W("built-in have no comments."));
                    }
                }
                else
                {
                    Error(eval, _W("function does not exist."));
                }
            }
        }
        else if (arg1.isFunctionHandle())
        {
            function_handle fh = arg1.getContentsAsFunctionHandle();
            FunctionDef *fun = (FunctionDef *)fh;
            if (eval->getContext()->getGlobalScope()->isPointerOnFunction(fun))
            {
                if (fun->type() == NLS_MACRO_FUNCTION)
                {
                    MacroFunctionDef *fm = (MacroFunctionDef *)fun;
                    filename = fm->fileName;
                }
                else
                {
                    Error(eval, _W("built-in have no comments."));
                }
            }
            else
            {
                Error(eval, _W("function does not exist."));
            }
        }
        HEADCOMMENTS_ERROR err = HEADCOMMENTS_ERROR::NO_ERROR;
        wstringVector comments = HeadComments(eval, filename, err);
        switch (err)
        {
            case HEADCOMMENTS_ERROR::NO_ERROR:
            {
                if (nLhs == 0)
                {
                    Interface *io = eval->getInterface();
                    if (io)
                    {
                        for (size_t i = 0; i < comments.size(); i++)
                        {
                            io->outputMessage(comments[i] + L"\n");
                        }
                    }
                }
                else
                {
                    retval.push_back(ToCellStringAsColumn(comments));
                }
            }
            break;
            case HEADCOMMENTS_ERROR::NOT_A_MACRO:
            {
                Error(eval, _W("A valid function expected."));
            }
            break;
            case HEADCOMMENTS_ERROR::FILE_NOT_EXIST:
            {
                Error(eval, _W("File does not exist."));
            }
            break;
        }
    }
    return retval;
}
//=============================================================================
