//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "xmlprettyprintBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Exception.hpp"
#include "characters_encoding.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "XmlPrettyPrint.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// xmlprettyprintBuiltin(source_dirs)
ArrayOfVector
Nelson::XmlGateway::xmlprettyprintBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, -1);
    nargoutcheck(nLhs, 0, 0);
    bool formatSpace = true;
    std::wstring errorMessage;
    wstringVector xmlFilesOrDirectories;
    if (argIn[0].isRowVectorCharacterArray()) {
        xmlFilesOrDirectories.push_back(argIn[0].getContentAsWideString());
    } else if (argIn[0].isCellArrayOfCharacterVectors() || argIn[0].isStringArray()) {
        xmlFilesOrDirectories = argIn[0].getContentAsWideStringVector();
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_CELL_OF_STRINGS_EXPECTED);
    }
    if (argIn.size() > 1) {
        if (argIn[1].isLogical()) {
            formatSpace = argIn[1].getContentAsLogicalScalar();
        } else {
            Error(ERROR_WRONG_ARGUMENT_2_TYPE_LOGICAL_EXPECTED);
        }
    }

    XmlPrettyPrint(xmlFilesOrDirectories, formatSpace, errorMessage);
    if (!errorMessage.empty()) {
        Error(errorMessage);
    }
    return retval;
}
//=============================================================================
