//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include "ferrorBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "FilesManager.hpp"
#include "FileError.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::ferrorBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 2);
    FilesManager* fm
        = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
    if (fm == nullptr) {
        Error(L"Nelson:stream_manager:fileManagerNotAvailable", L"Problem with file manager.");
    }
    bool withClear = false;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        std::wstring str = param2.asWideString();
        if (str != L"clear") {
            Error(L"Nelson:ferror:invalidArgument", L"'clear' expected as second argument.");
        }
        withClear = true;
    }
    ArrayOf param1 = argIn[0];
    int32 iValue = static_cast<int32>(param1.asDoubleScalar(false));
    if (!fm->isOpened(iValue)) {
        Error(L"Nelson:stream_manager:invalidFileIdentifier", L"Invalid file identifier.");
    }
    int errorCode = 0;
    std::string errorMessage;
    if (!FileError(fm, iValue, withClear, errorCode, errorMessage)) {
        Error(L"Nelson:stream_manager:invalidFileIdentifier", L"Invalid file identifier.");
    }
    retval << ArrayOf::characterArray(errorMessage);
    retval << ArrayOf::doubleScalar((double)errorCode);
    return retval;
}
//=============================================================================
