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
#include "ErrorID.hpp"
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
    nargincheck(argIn, 1, 2);
    std::wstring wpath;
    std::wstring wtype;
    if (argIn.size() == 2) {
        nargoutcheck(nLhs, 0, 1);
        if (argIn[1].isRowVectorCharacterArray()) {
            wtype = argIn[1].asWideString();
            if (wtype == L"path") {
                // OK
            } else if (wtype == L"filename") {
                // OK
            } else if (wtype == L"extension") {
                // OK
            } else {
                Error(L"Nelson:fileparts:invalidArgument",
                    L"Argument #%d must contain a valid string 'path', 'filename' or 'extension' "
                    L"expected.",
                    2);
            }
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                L"string");
        }
    } else {
        nargoutcheck(nLhs, 0, 3);
    }
    if (argIn[0].isRowVectorCharacterArray()) {
        wpath = argIn[0].asWideString();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    std::wstring respath;
    std::wstring resfilename;
    std::wstring resextension;
    FileParts(wpath, respath, resfilename, resextension);
    if (wtype.empty()) {
        retval << ArrayOf::characterArray(respath);
        if (nLhs > 1) {
            retval << ArrayOf::characterArray(resfilename);
        }
        if (nLhs > 2) {
            retval << ArrayOf::characterArray(resextension);
        }
    } else {
        if (wtype == L"path") {
            retval << ArrayOf::characterArray(respath);
        } else if (wtype == L"filename") {
            retval << ArrayOf::characterArray(resfilename);
        } else if (wtype == L"extension") {
            retval << ArrayOf::characterArray(resextension);
        }
    }
    return retval;
}
//=============================================================================
