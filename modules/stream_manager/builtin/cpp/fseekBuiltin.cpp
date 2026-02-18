//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fseekBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "File.hpp"
#include "FileSeek.hpp"
#include "FilesManager.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::fseekBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    ArrayOf param3 = argIn[2];
    int ORIGIN = 0;
    if (param3.isRowVectorCharacterArray()) {
        std::wstring str = param3.getContentAsWideString();
        if ((str == L"bof") || (str == L"set")) {
            ORIGIN = -1;
        } else if ((str == L"cof") || (str == L"cur")) {
            ORIGIN = 0;
        } else if ((str == L"eof") || (str == L"end")) {
            ORIGIN = 1;
        } else {
            raiseError(L"Nelson:stream_manager:ERROR_INVALID_ORIGIN", ERROR_INVALID_ORIGIN);
        }
    } else {
        int iValue = static_cast<int>(param3.getContentAsDoubleScalar());
        switch (iValue) {
        case -1:
        case 0:
        case 1: {
            ORIGIN = iValue;
        } break;
        default: {
            raiseError(L"Nelson:stream_manager:ERROR_INVALID_ORIGIN", ERROR_INVALID_ORIGIN);
        } break;
        }
    }
    auto iOffset = static_cast<int64>(param2.getContentAsDoubleScalar());
    auto* fm = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
    if (fm == nullptr) {
        raiseError2(L"nelson:io:fileManagerError");
    }
    auto iValue = static_cast<int32>(param1.getContentAsDoubleScalar());
    if (fm->isOpened(iValue)) {
        File* f = fm->getFile(iValue);
        if (!FileSeek(f, iOffset, ORIGIN)) {
            retval << ArrayOf::doubleConstructor(-1);
        } else {
            retval << ArrayOf::doubleConstructor(0);
        }
    } else {
        raiseError2(L"nelson:io:invalidFileId");
    }
    return retval;
}
//=============================================================================
