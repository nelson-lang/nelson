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
#include "copyfileBuiltin.hpp"
#include "CopyFile.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::copyfileBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    bool bForce = false;
    if (argIn.size() == 3) {
        std::wstring arg3 = argIn[2].asWideString();
        if ((arg3 == L"f") || (arg3 == L"F")) {
            bForce = true;
        } else {
            Error("Nelson:files_folders_functions:invalidOption", "'f' expected.");
        }
    }
    bool bRes = false;
    std::wstring errorMessage;
    std::wstring messageId;
    ArrayOf arg2 = argIn[1];
    std::wstring dest = arg2.asWideString();
    ArrayOf arg1 = argIn[0];
    if (!(arg1.isRowVectorCharacterArray() || arg1.isScalarStringArray() || arg1.isCell()
            || arg1.isStringArray())) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"string or cell");
    }
    // [status, message] = copyfile(...) must not throw on a copy failure:
    // it returns status = false and a message. With 0 or 1 output it throws.
    try {
        if (arg1.isRowVectorCharacterArray() || arg1.isScalarStringArray()) {
            std::wstring src = arg1.asWideString();
            if (FileSystemWrapper::Path::is_regular_file(src)) {
                bRes = CopyFile(src, dest, bForce, errorMessage);
            } else {
                bRes = CopyDirectory(src, dest, bForce, errorMessage);
            }
        } else {
            wstringVector src = arg1.asWideStringVector(true);
            bRes = CopyFiles(src, dest, bForce, errorMessage);
        }
    } catch (const NelsonException& e) {
        if (nLhs <= 1) {
            throw;
        }
        bRes = false;
        errorMessage = e.message();
    }
    if (nLhs == 0) {
        if (!bRes) {
            Error(L"Nelson:copyfile:copyFailed", errorMessage);
        }
    } else {
        nargoutcheck(nLhs, 0, 3);
        if (!bRes) {
            messageId = L"Nelson:copyfile:CopyError";
        }
        retval << ArrayOf::logicalScalar(bRes);
        if (nLhs > 1) {
            retval << ArrayOf::characterArray(errorMessage);
        }
        if (nLhs > 2) {
            retval << ArrayOf::characterArray(messageId);
        }
    }
    return retval;
}
//=============================================================================
