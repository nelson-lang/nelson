//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
        Error(_W("Problem with file manager."));
    }
    bool withClear = false;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        std::wstring str = param2.getContentAsWideString();
        if (str != L"clear") {
            Error(_W("'clear' expected as second argument."));
        }
        withClear = true;
    }
    ArrayOf param1 = argIn[0];
    int32 iValue = static_cast<int32>(param1.getContentAsDoubleScalar(false));
    if (!fm->isOpened(iValue)) {
        Error(_W("Invalid file identifier."));
    }
    int errorCode = 0;
    std::string errorMessage;
    if (!FileError(fm, iValue, withClear, errorCode, errorMessage)) {
        Error(_W("Invalid file identifier."));
    }
    retval << ArrayOf::characterArrayConstructor(errorMessage);
    retval << ArrayOf::doubleConstructor((double)errorCode);
    return retval;
}
//=============================================================================
