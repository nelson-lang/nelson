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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "FileClose.hpp"
#include "FilesManager.hpp"
#include "NelsonConfiguration.hpp"
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
        Error(L"Nelson:stream_manager:fileManagerNotAvailable", L"Problem with file manager.");
    }
    nargincheck(argIn, 1, 1);
    ArrayOf param1 = argIn[0];
    if (param1.isDoubleType()) {
        nargoutcheck(nLhs, 0, 1);
        auto iValue = static_cast<int32>(param1.asDoubleScalar());
        if (fm->isOpened(iValue)) {
            if (FileClose(fm, iValue)) {
                retval << ArrayOf::doubleScalar(0.);
            } else {
                retval << ArrayOf::doubleScalar(-1.);
            }
        } else {
            if (nLhs == 1) {
                retval << ArrayOf::doubleScalar(-1.);
            } else {
                Error(L"Nelson:stream_manager:invalidFileIdentifier", L"Invalid file identifier.");
            }
        }
    } else if (param1.isRowVectorCharacterArray()) {
        nargoutcheck(nLhs, 0, 0);
        std::wstring str = param1.asWideString();
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
                Error(L"Nelson:fclose:cannotClose", L"Cannot close files.");
            }
        } else {
            Error(L"Nelson:fclose:invalidArgument", L"Wrong value for #%d: '%s' expected.", 1,
                L"all");
        }
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"string or double");
    }
    return retval;
}
//=============================================================================
