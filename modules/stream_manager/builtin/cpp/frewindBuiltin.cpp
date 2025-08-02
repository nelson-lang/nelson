//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "frewindBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "File.hpp"
#include "FileRewind.hpp"
#include "FilesManager.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::frewindBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    auto* fm = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
    if (fm == nullptr) {
        Error(_W("Problem with file manager."));
    }
    ArrayOf param1 = argIn[0];
    auto iValue = static_cast<int32>(param1.getContentAsDoubleScalar());
    if (fm->isOpened(iValue)) {
        File* f = fm->getFile(iValue);
        if (f->isInterfaceMethod()) {
            Error(_W("Rewind failed."));
        } else {
            FileRewind(f);
        }
    } else {
        Error(_W("Invalid file identifier."));
    }
    return retval;
}
//=============================================================================
