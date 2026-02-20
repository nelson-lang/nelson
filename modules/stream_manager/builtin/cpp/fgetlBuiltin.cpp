//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fgetlBuiltin.hpp"
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
Nelson::StreamGateway::fgetlBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    if (param1.isDoubleType()) {
        auto* fm = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
        if (fm == nullptr) {
            raiseError2(_E("nelson:io:fileManagerError"));
        }
        auto iValue = static_cast<int32>(param1.getContentAsDoubleScalar());
        if (fm->isStdStream(iValue)) {
            raiseError(L"Nelson:stream:ERROR_NOT_IMPLEMENTED_FOR_REQUESTED_FILEID",
                ERROR_NOT_IMPLEMENTED_FOR_REQUESTED_FILEID);
        }
        if (fm->isOpened(iValue)) {
            File* f = fm->getFile(iValue);
            std::wstring result;
            if (FileGetLine(f, -1, false, result)) {
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
