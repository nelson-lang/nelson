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
#include "ErrorID.hpp"
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
            std::wstring argstr = argIn[0].asWideString();
            if ((argstr == L"fullpath") || (argstr == L"fullpathext")) {
                if (argstr == L"fullpath") {
                    iExt = 1;
                }
                if (argstr == L"fullpathext") {
                    iExt = 2;
                }
            } else {
                Error(L"Nelson:nfilename:invalidValue",
                    L"Wrong value for #%d argument, \'fullpathext\' or \'fullpath\' expected.", 1);
            }
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string");
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
    retval << ArrayOf::characterArray(path.generic_wstring());
    return retval;
}
//=============================================================================
