//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__xmldoclinkchecker__Builtin.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
#include "XmlDocLinkChecker.hpp"
#include "FileSystemWrapper.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
readDirectoriesArgument(
    const ArrayOf& argument, wstringVector& directories, std::wstring& errorMessage)
{
    directories.clear();
    if (argument.isRowVectorCharacterArray()) {
        directories.push_back(argument.getContentAsWideString());
    } else if (argument.isCell()) {
        directories = argument.getContentAsWideStringVector(true);
    } else {
        errorMessage = ERROR_WRONG_ARGUMENT_1_TYPE_CELL_OF_STRINGS_EXPECTED;
        return false;
    }
    bool permissionDenied = false;
    for (const auto& directory : directories) {
        if (!FileSystemWrapper::Path::is_directory(directory, permissionDenied)) {
            errorMessage
                = permissionDenied ? _W("Permission denied.") : _W("Existing directory expected.");
            return false;
        }
    }
    return true;
}
//=============================================================================
static bool
readFilesArgument(const ArrayOf& argument, wstringVector& files, std::wstring& errorMessage)
{
    files.clear();
    if (argument.isEmpty()) {
        return true;
    }
    if (argument.isRowVectorCharacterArray()) {
        files.push_back(argument.getContentAsWideString());
    } else if (argument.isCell()) {
        files = argument.getContentAsWideStringVector(true);
    } else {
        errorMessage = ERROR_WRONG_ARGUMENT_2_TYPE_CELL_OF_STRINGS_EXPECTED;
        return false;
    }
    bool permissionDenied = false;
    for (const auto& file : files) {
        if (!FileSystemWrapper::Path::is_regular_file(file, permissionDenied)) {
            errorMessage
                = permissionDenied ? _W("Permission denied.") : _W("Existing file expected.");
            return false;
        }
    }
    return true;
}
//=============================================================================
ArrayOfVector
Nelson::HelpToolsGateway::__xmldoclinkchecker__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 3);

    std::wstring errorMessage;
    wstringVector xmlDirectories;
    if (!readDirectoriesArgument(argIn[0], xmlDirectories, errorMessage)) {
        Error(errorMessage);
    }

    wstringVector xmlFilesToCheck;
    if (!readFilesArgument(argIn[1], xmlFilesToCheck, errorMessage)) {
        Error(errorMessage);
    }

    wstringVector errors;
    wstringVector warnings;
    bool state = xmldoclinkchecker(xmlDirectories, xmlFilesToCheck, errors, warnings, errorMessage);
    if (!state && !errorMessage.empty()) {
        Error(errorMessage);
    }

    ArrayOfVector retval;
    retval << ArrayOf::logicalConstructor(state);
    if (nLhs >= 2) {
        retval << ArrayOf::toCellArrayOfCharacterColumnVectors(errors);
    }
    if (nLhs >= 3) {
        retval << ArrayOf::toCellArrayOfCharacterColumnVectors(warnings);
    }
    return retval;
}
//=============================================================================