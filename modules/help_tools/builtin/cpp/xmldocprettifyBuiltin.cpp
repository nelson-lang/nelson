//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "xmldocprettifyBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Exception.hpp"
#include "characters_encoding.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "XmlDocPrettify.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// xmldocprettify(source_dirs)
ArrayOfVector
Nelson::HelpToolsGateway::xmldocprettifyBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 0);
    std::wstring errorMessage;
    if (argIn.empty()) {
        XmlDocPrettify(errorMessage);
    } else {
        wstringVector xmlFilesOrDirectories;
        if (argIn[0].isRowVectorCharacterArray()) {
            xmlFilesOrDirectories.push_back(argIn[0].getContentAsWideString());
        } else if (argIn[0].isCellArrayOfCharacterVectors() || argIn[0].isStringArray()) {
            xmlFilesOrDirectories = argIn[0].getContentAsWideStringVector();
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_CELL_OF_STRINGS_EXPECTED);
        }
        XmlDocPrettify(xmlFilesOrDirectories, errorMessage);
    }
    if (!errorMessage.empty()) {
        Error(errorMessage);
    }
    return retval;
}
//=============================================================================
