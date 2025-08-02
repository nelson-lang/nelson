//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fgetsBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "File.hpp"
#include "FileGetLine.hpp"
#include "FilesManager.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::fgetsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    int nbCharacters = -1;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        nbCharacters = static_cast<int>(param2.getContentAsDoubleScalar());
        if (nbCharacters >= 0) {
            if (std::isinf((double)nbCharacters)) {
                nbCharacters = -1;
            }
        } else {
            Error(_W("Second argument must be greater than zero."));
        }
    }
    ArrayOf param1 = argIn[0];
    if (param1.isDoubleType()) {
        auto* fm = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
        if (fm == nullptr) {
            Error(_W("Problem with file manager."));
        }
        auto iValue = static_cast<int32>(param1.getContentAsDoubleScalar());
        if (fm->isStdStream(iValue)) {
            Error(_W("Not implemented for requested file identifier."));
        }
        if (fm->isOpened(iValue)) {
            File* f = fm->getFile(iValue);
            std::wstring result;
            if (FileGetLine(f, nbCharacters, true, result)) {
                retval << ArrayOf::characterArrayConstructor(result);
            } else {
                retval << ArrayOf::doubleConstructor(-1);
            }
        } else {
            Error(_W("Invalid file identifier."));
        }
    } else {
        Error(_W("Invalid file identifier."));
    }
    return retval;
}
//=============================================================================
