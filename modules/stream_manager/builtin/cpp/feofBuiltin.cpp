//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "feofBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FilesManager.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::feofBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    FilesManager* fm
        = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
    if (fm == nullptr) {
        Error(_W("Problem with file manager."));
    }
    ArrayOf param1 = argIn[0];
    int32 iValue = static_cast<int32>(param1.getContentAsDoubleScalar(false));
    if (!fm->isOpened(iValue)) {
        Error(_W("Invalid file identifier."));
    }
    FILE* fileptr = static_cast<FILE*>(fm->getFilePointer(iValue));
    retval << ArrayOf::doubleConstructor(feof(fileptr));
    return retval;
}
//=============================================================================
