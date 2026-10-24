//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ftellBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "File.hpp"
#include "FileTell.hpp"
#include "FilesManager.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::ftellBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    auto iValue = static_cast<int32>(param1.asDoubleScalar());
    auto* fm = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
    if (fm == nullptr) {
        Error(L"Nelson:stream_manager:fileManagerNotAvailable", L"Problem with file manager.");
    }
    if (fm->isOpened(iValue)) {
        File* f = fm->getFile(iValue);
        if (f == nullptr) {
            Error(L"Nelson:stream_manager:invalidFileIdentifier", L"Invalid file identifier.");
        }
        auto dpos = static_cast<double>(FileTell(f));
        retval << ArrayOf::doubleScalar(dpos);
    } else {
        Error(L"Nelson:stream_manager:invalidFileIdentifier", L"Invalid file identifier.");
    }
    return retval;
}
//=============================================================================
