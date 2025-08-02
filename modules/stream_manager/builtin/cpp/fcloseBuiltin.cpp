//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fcloseBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FileClose.hpp"
#include "FilesManager.hpp"
#include "NelsonConfiguration.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::fcloseBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    auto* fm = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
    if (fm == nullptr) {
        Error(_W("Problem with file manager."));
    }
    nargincheck(argIn, 1, 1);
    ArrayOf param1 = argIn[0];
    if (param1.isDoubleType()) {
        nargoutcheck(nLhs, 0, 1);
        auto iValue = static_cast<int32>(param1.getContentAsDoubleScalar());
        if (fm->isOpened(iValue)) {
            if (FileClose(fm, iValue)) {
                retval << ArrayOf::doubleConstructor(0.);
            } else {
                retval << ArrayOf::doubleConstructor(-1.);
            }
        } else {
            Error(_W("Invalid file identifier."));
        }
    } else if (param1.isRowVectorCharacterArray()) {
        nargoutcheck(nLhs, 0, 0);
        std::wstring str = param1.getContentAsWideString();
        if (str == L"all") {
            Nelson::FilesManager* nfm;
            try {
                nfm = new Nelson::FilesManager();
            } catch (const std::bad_alloc&) {
                nfm = nullptr;
            }
            if (nfm) {
                if (fm) {
                    delete fm;
                    fm = nullptr;
                }
                NelsonConfiguration::getInstance()->setFileManager((void*)nfm);
            } else {
                Error(_W("Cannot close files."));
            }
        } else {
            Error(_W("Wrong value for #1: 'all' expected."));
        }
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_OR_DOUBLE_EXPECTED);
    }
    return retval;
}
//=============================================================================
