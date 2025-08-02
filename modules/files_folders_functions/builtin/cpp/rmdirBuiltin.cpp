//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "rmdirBuiltin.hpp"
#include "Error.hpp"
#include "RemoveDirectory.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::rmdirBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() == 1 || argIn.size() == 2) {
        bool bbSubfolder = false;
        std::wstring arg1 = argIn[0].getContentAsWideString();
        if (argIn.size() == 2) {
            std::wstring arg2 = argIn[1].getContentAsWideString();
            if ((arg2 == L"s") || (arg2 == L"S")) {
                bbSubfolder = true;
            } else {
                Error("'s' expected.");
            }
        }
        std::wstring errorMessage;
        bool res = RemoveDirectory(arg1, bbSubfolder, errorMessage);
        if (nLhs == 0) {
            if (static_cast<int>(res) == false) {
                Error(errorMessage);
            }
        } else {
            retval << ArrayOf::logicalConstructor(res);
            if (nLhs > 1) {
                retval << ArrayOf::characterArrayConstructor(errorMessage);
            } else {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
        }
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
