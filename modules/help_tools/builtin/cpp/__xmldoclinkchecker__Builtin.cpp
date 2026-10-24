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
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
        directories.push_back(argument.asWideString());
    } else if (argument.isCell()) {
        directories = argument.asWideStringVector(true);
    } else {
        errorMessage = Nelson::formatI18nMessage(
            L"Wrong type for argument #%d: %s expected.", (1), L"cell of strings");
        return false;
    }
    bool permissionDenied = false;
    for (const auto& directory : directories) {
        if (!FileSystemWrapper::Path::is_directory(directory, permissionDenied)) {
            errorMessage = permissionDenied
                ? Nelson::resolveMessageTemplate(
                      L"Nelson:help_tools:permissionDenied", L"Permission denied.")
                : _W("Existing directory expected.");
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
        files.push_back(argument.asWideString());
    } else if (argument.isCell()) {
        files = argument.asWideStringVector(true);
    } else {
        errorMessage = Nelson::formatI18nMessage(
            L"Wrong type for argument #%d: %s expected.", (2), L"cell of strings");
        return false;
    }
    bool permissionDenied = false;
    for (const auto& file : files) {
        if (!FileSystemWrapper::Path::is_regular_file(file, permissionDenied)) {
            errorMessage = permissionDenied
                ? Nelson::resolveMessageTemplate(
                      L"Nelson:help_tools:permissionDenied", L"Permission denied.")
                : resolveMessageTemplate(
                      L"Nelson:help_tools:existingFile", L"Existing file expected.");
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
        Error(L"Nelson:xmldoclinkchecker:checkFailed", errorMessage);
    }

    wstringVector xmlFilesToCheck;
    if (!readFilesArgument(argIn[1], xmlFilesToCheck, errorMessage)) {
        Error(L"Nelson:xmldoclinkchecker:checkFailed", errorMessage);
    }

    wstringVector errors;
    wstringVector warnings;
    bool state = xmldoclinkchecker(xmlDirectories, xmlFilesToCheck, errors, warnings, errorMessage);
    if (!state && !errorMessage.empty()) {
        Error(L"Nelson:xmldoclinkchecker:checkFailed", errorMessage);
    }

    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(state);
    if (nLhs >= 2) {
        retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(errors);
    }
    if (nLhs >= 3) {
        retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(warnings);
    }
    return retval;
}
//=============================================================================
