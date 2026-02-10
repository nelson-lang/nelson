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
#include "PredefinedErrorMessages.hpp"
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
                    raiseError(L"Nelson:files_folders_functions:ERROR_ARGUMENT_2_MUST_CONTAIN_"
                               L"VALID_STRING_PATH_FILENAME_OR_EXTENSION",
                        ERROR_ARGUMENT_2_MUST_CONTAIN_VALID_STRING_PATH_FILENAME_OR_EXTENSION);
                }
            } else {
                raiseError(L"Nelson:files_folders_functions:ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED",
                    ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED, 2, NLS_STRING_ARRAY_STR);
            }
        } else {
            nargoutcheck(nLhs, 0, 3);
        }
        if (argIn[0].isRowVectorCharacterArray()) {
            wpath = argIn[0].getContentAsWideString();
        } else {
            raiseError(L"Nelson:files_folders_functions:ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED",
                ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED, 1, NLS_STRING_ARRAY_STR);
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
        raiseError(L"Nelson:files_folders_functions:ERROR_WRONG_NUMBERS_INPUT_ARGS",
            ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
