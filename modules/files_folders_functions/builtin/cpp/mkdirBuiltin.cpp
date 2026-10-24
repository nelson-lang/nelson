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
#include "ErrorID.hpp"
#include "MakeDirectory.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::mkdirBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 3);
    std::wstring parentDir;
    std::wstring newDir;
    if (argIn.size() == 2) {
        if (argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray()) {
            newDir = argIn[1].asWideString();
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                L"logical");
        }
    }
    if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        parentDir = argIn[0].asWideString();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    std::wstring message;
    std::wstring messageId;
    bool bOK = false;
    if (newDir.empty()) {
        bOK = MakeDirectory(parentDir, message, messageId);
    } else {
        bOK = MakeDirectory(parentDir, newDir, message, messageId);
    }
    if (nLhs == 0) {
        if (!bOK) {
            Error(L"Nelson:mkdir:createFailed", message);
        }
    } else {
        if (nLhs > 0) {
            retval << ArrayOf::logicalScalar(bOK);
        }
        if (nLhs > 1) {
            retval << ArrayOf::characterArray(message);
        }
        if (nLhs > 2) {
            retval << ArrayOf::characterArray(messageId);
        }
    }
    return retval;
}
//=============================================================================
