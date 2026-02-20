//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "mkdirBuiltin.hpp"
#include "Error.hpp"
#include "MakeDirectory.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::mkdirBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 2);
    std::wstring parentDir;
    std::wstring newDir;
    if (argIn.size() == 2) {
        if (argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray()) {
            newDir = argIn[1].getContentAsWideString();
        } else {
            raiseError2(_E("nelson:validators:mustBeTypeAtPosition"), 2, NLS_LOGICAL_STR);
        }
    }
    if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        parentDir = argIn[0].getContentAsWideString();
    } else {
        raiseError2(_E("nelson:validators:mustBeTypeAtPosition"), 1, NLS_LOGICAL_STR);
    }
    std::wstring message;
    bool bOK = false;
    if (newDir.empty()) {
        bOK = MakeDirectory(parentDir, message);
    } else {
        bOK = MakeDirectory(parentDir, newDir, message);
    }
    if (nLhs == 0) {
        if (!bOK) {
            raiseError(L"Nelson:files_folders_functions:makeDirectory", ERROR_MAKEDIRECTORY_ERROR,
                message);
        }
    } else {
        if (nLhs > 0) {
            retval << ArrayOf::logicalConstructor(bOK);
        }
        if (nLhs > 1) {
            retval << ArrayOf::characterArrayConstructor(message);
        }
    }
    return retval;
}
//=============================================================================
