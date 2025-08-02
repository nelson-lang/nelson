//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "nfilenameBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GetCurrentNFilename.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::nfilenameBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    int iExt = 0;
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 1) {
        if (argIn[0].isRowVectorCharacterArray()) {
            std::wstring argstr = argIn[0].getContentAsWideString();
            if ((argstr == L"fullpath") || (argstr == L"fullpathext")) {
                if (argstr == L"fullpath") {
                    iExt = 1;
                }
                if (argstr == L"fullpathext") {
                    iExt = 2;
                }
            } else {
                Error(
                    _W("Wrong value for #1 argument, \'fullpathext\' or  \'fullpath\' expected."));
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
    }
    FileSystemWrapper::Path path(GetCurrentNFilenameW(eval));
    switch (iExt) {
    case 0:
        path = path.stem();
        break;
    case 1:
        path = path.replace_extension();
        break;
    case 2:
        break;
    }
    retval << ArrayOf::characterArrayConstructor(path.generic_wstring());
    return retval;
}
//=============================================================================
