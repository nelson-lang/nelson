//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fsizeBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "File.hpp"
#include "FileSize.hpp"
#include "FilesManager.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::fsizeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    if (param1.isDoubleType()) {
        auto* fm = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
        if (fm == nullptr) {
            raiseError2(_E("nelson:io:fileManagerError"));
        }
        auto iValue = static_cast<int32>(param1.getContentAsDoubleScalar());
        if (fm->isStdStream(iValue)) {
            raiseError(L"Nelson:stream_manager:ERROR_NOT_IMPLEMENTED_FOR_REQUESTED_FILEID_GENERIC",
                ERROR_NOT_IMPLEMENTED_FOR_REQUESTED_FILEID_GENERIC);
        }
        if (fm->isOpened(iValue)) {
            File* f = fm->getFile(iValue);
            auto sz = static_cast<double>(FileSize(f));
            retval << ArrayOf::doubleConstructor(sz);
        } else {
            raiseError2(_E("nelson:io:invalidFileId"));
        }
    } else {
        raiseError2(_E("nelson:io:invalidFileId"));
    }
    return retval;
}
//=============================================================================
