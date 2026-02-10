//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fgetsBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "File.hpp"
#include "FileGetLine.hpp"
#include "FilesManager.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::fgetsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    int nbCharacters = -1;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        nbCharacters = static_cast<int>(param2.getContentAsDoubleScalar());
        if (nbCharacters >= 0) {
            if (std::isinf((double)nbCharacters)) {
                nbCharacters = -1;
            }
        } else {
            raiseError(L"Nelson:stream:ERROR_SECOND_ARGUMENT_MUST_BE_GREATER_THAN_ZERO",
                ERROR_SECOND_ARGUMENT_MUST_BE_GREATER_THAN_ZERO);
        }
    }
    ArrayOf param1 = argIn[0];
    if (param1.isDoubleType()) {
        auto* fm = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
        if (fm == nullptr) {
            raiseError(
                L"Nelson:stream:ERROR_PROBLEM_WITH_FILE_MANAGER", ERROR_PROBLEM_WITH_FILE_MANAGER);
        }
        auto iValue = static_cast<int32>(param1.getContentAsDoubleScalar());
        if (fm->isStdStream(iValue)) {
            raiseError(L"Nelson:stream:ERROR_NOT_IMPLEMENTED_FOR_REQUESTED_FILEID",
                ERROR_NOT_IMPLEMENTED_FOR_REQUESTED_FILEID);
        }
        if (fm->isOpened(iValue)) {
            File* f = fm->getFile(iValue);
            std::wstring result;
            if (FileGetLine(f, nbCharacters, true, result)) {
                retval << ArrayOf::characterArrayConstructor(result);
            } else {
                retval << ArrayOf::doubleConstructor(-1);
            }
        } else {
            raiseError(L"Nelson:stream:ERROR_INVALID_FILEID", ERROR_INVALID_FILEID);
        }
    } else {
        raiseError(L"Nelson:stream:ERROR_INVALID_FILEID", ERROR_INVALID_FILEID);
    }
    return retval;
}
//=============================================================================
