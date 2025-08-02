//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "headcommentsBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HeadComments.hpp"
#include "FileSystemWrapper.hpp"
#include "MacroFunctionDef.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HelpToolsGateway::headcommentsBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    std::wstring filename;
    if (argIn.size() == 1) {
        ArrayOf arg1 = argIn[0];
        std::wstring functionName;
        if (arg1.isRowVectorCharacterArray()) {
            functionName = arg1.getContentAsWideString();
            if (FileSystemWrapper::Path::is_regular_file(functionName)) {
                filename = functionName;
            } else {
                Context* context = eval->getContext();
                FunctionDef* funcDef = nullptr;
                if (context->lookupFunction(wstring_to_utf8(functionName), funcDef)) {
                    if (funcDef->type() == NLS_MACRO_FUNCTION) {
                        auto* fm = (MacroFunctionDef*)funcDef;
                        filename = fm->getFilename();
                    } else {
                        Error(_W("built-in have no comments."));
                    }
                } else {
                    Error(_W("function does not exist."));
                }
            }
        } else if (arg1.isFunctionHandle()) {
            function_handle fh = arg1.getContentAsFunctionHandle();
            FunctionDefPtr fun = reinterpret_cast<FunctionDef*>(fh.anonymousHandle);
            std::string name = fun->getName();

            if (!eval->getContext()->lookupFunction(name, fun)) {
                Error(_W("function does not exist."));
            }
            if (fun->type() == NLS_MACRO_FUNCTION) {
                auto* fm = (MacroFunctionDef*)fun;
                filename = fm->getFilename();
            } else {
                Error(_W("built-in, mex, anonymous function have no comments."));
            }
        }
        HEADCOMMENTS_ERROR err = HEADCOMMENTS_ERROR::MACRO_OK;
        wstringVector comments = HeadComments(eval, filename, err);
        switch (err) {
        case HEADCOMMENTS_ERROR::MACRO_OK: {
            if (nLhs == 0) {
                Interface* io = eval->getInterface();
                if (io) {
                    for (const auto& comment : comments) {
                        io->outputMessage(comment + L"\n");
                    }
                }
            } else {
                retval << ArrayOf::toCellArrayOfCharacterColumnVectors(comments);
            }
        } break;
        case HEADCOMMENTS_ERROR::NOT_A_MACRO: {
            Error(_W("A valid function expected."));
        } break;
        case HEADCOMMENTS_ERROR::FILE_NOT_EXIST: {
            Error(_W("File does not exist."));
        } break;
        }
    }
    return retval;
}
//=============================================================================
