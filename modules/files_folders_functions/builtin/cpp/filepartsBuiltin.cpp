//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "filepartsBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FileParts.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::filepartsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if ((argIn.size() == 1) || (argIn.size() == 2)) {
        std::wstring wpath;
        std::wstring wtype;
        if (argIn.size() == 2) {
            nargoutcheck(nLhs, 0, 1);
            if (argIn[1].isRowVectorCharacterArray()) {
                wtype = argIn[1].getContentAsWideString();
                if (wtype == L"path") {
                    // OK
                } else if (wtype == L"filename") {
                    // OK
                } else if (wtype == L"extension") {
                    // OK
                } else {
                    Error(_W("Argument #2 must contain a valid string 'path', 'filename' or "
                             "'extension' expected."));
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
            }
        } else {
            nargoutcheck(nLhs, 0, 3);
        }
        if (argIn[0].isRowVectorCharacterArray()) {
            wpath = argIn[0].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring respath;
        std::wstring resfilename;
        std::wstring resextension;
        FileParts(wpath, respath, resfilename, resextension);
        if (wtype.empty()) {
            retval << ArrayOf::characterArrayConstructor(respath);
            if (nLhs > 1) {
                retval << ArrayOf::characterArrayConstructor(resfilename);
            }
            if (nLhs > 2) {
                retval << ArrayOf::characterArrayConstructor(resextension);
            }
        } else {
            if (wtype == L"path") {
                retval << ArrayOf::characterArrayConstructor(respath);
            } else if (wtype == L"filename") {
                retval << ArrayOf::characterArrayConstructor(resfilename);
            } else if (wtype == L"extension") {
                retval << ArrayOf::characterArrayConstructor(resextension);
            }
        }
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
