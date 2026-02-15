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
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);
    bool formatSpace = true;
    std::wstring errorMessage;
    wstringVector xmlFilesOrDirectories;
    if (argIn[0].isRowVectorCharacterArray()) {
        xmlFilesOrDirectories.push_back(argIn[0].getContentAsWideString());
    } else if (argIn[0].isCellArrayOfCharacterVectors() || argIn[0].isStringArray()) {
        xmlFilesOrDirectories = argIn[0].getContentAsWideStringVector();
    } else {
        raiseError2(
            L"nelson:validators:mustBeType", 1, ERROR_TYPE_CELL_OF_STRINGS);
    }
    if (argIn.size() > 1) {
        if (argIn[1].isLogical()) {
            formatSpace = argIn[1].getContentAsLogicalScalar();
        } else {
            raiseError2(L"nelson:validators:mustBeType", 2, NLS_LOGICAL_STR);
        }
    }

    XmlPrettyPrint(xmlFilesOrDirectories, formatSpace, errorMessage);
    if (!errorMessage.empty()) {
        Error(errorMessage, L"Nelson:xml:ERROR_XMLPRETTYPRINT_ERROR");
    }
    return retval;
}
//=============================================================================
