//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "copyfileBuiltin.hpp"
#include "CopyFile.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::copyfileBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() == 2 || argIn.size() == 3) {
        bool bForce = false;
        if (argIn.size() == 3) {
            std::wstring arg3 = argIn[2].getContentAsWideString();
            if ((arg3 == L"f") || (arg3 == L"F")) {
                bForce = true;
            } else {
                Error("'f' expected.");
            }
        }
        bool bRes = false;
        std::wstring errorMessage;
        ArrayOf arg2 = argIn[1];
        std::wstring dest = arg2.getContentAsWideString();
        ArrayOf arg1 = argIn[0];
        if (arg1.isRowVectorCharacterArray() || arg1.isScalarStringArray()) {
            std::wstring src = arg1.getContentAsWideString();
            if (FileSystemWrapper::Path::is_regular_file(src)) {
                bRes = CopyFile(src, dest, bForce, errorMessage);
            } else {
                bRes = CopyDirectory(src, dest, bForce, errorMessage);
            }
        } else if (arg1.isCell() || arg1.isStringArray()) {
            wstringVector src = arg1.getContentAsWideStringVector(true);
            bRes = CopyFiles(src, dest, bForce, errorMessage);
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_OR_CELL_EXPECTED);
        }
        if (nLhs == 0) {
            if (!bRes) {
                Error(errorMessage);
            }
        } else {
            retval << ArrayOf::logicalConstructor(bRes);
            if (nLhs > 1) {
                retval << ArrayOf::characterArrayConstructor(errorMessage);
            }
            nargoutcheck(nLhs, 0, 2);
        }
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
