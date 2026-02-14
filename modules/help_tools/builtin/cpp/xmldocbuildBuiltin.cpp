//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "xmldocbuildBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Exception.hpp"
#include "XmlTarget.hpp"
#include "characters_encoding.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FileSystemWrapper.hpp"
#include "XmlDocBuild.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// xmldocbuild(source_dirs, destination_dir, main_title, export_format, overwrite)
ArrayOfVector
Nelson::HelpToolsGateway::xmldocbuildBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    bool res = false;
    std::wstring errorMessage;
    const auto buildReturn = [&](bool localRes, const std::wstring& localError) {
        ArrayOfVector localRetval;
        if (nLhs == 0) {
            if (!localError.empty()) {
                Error(localError, L"Nelson:help_tools:ERROR_RUNTIME_MESSAGE");
            }
            localRetval << ArrayOf::logicalConstructor(localRes);
        } else {
            localRetval << ArrayOf::logicalConstructor(localRes);
            if (nLhs == 2) {
                localRetval << ArrayOf::characterArrayConstructor(localError);
            }
        }
        return localRetval;
    };
    nargincheck(argIn, 5, 5);
    nargoutcheck(nLhs, 0, 2);
    ArrayOf argSourceDirs = argIn[0];
    wstringVector listOfDirectories;
    if (argSourceDirs.isRowVectorCharacterArray()) {
        std::wstring dir = argSourceDirs.getContentAsWideString();
        listOfDirectories.push_back(dir);
    } else if (argSourceDirs.isCell()) {
        listOfDirectories = argSourceDirs.getContentAsWideStringVector(true);
    } else {
        raiseError2(
            L"Nelson:error_manager:wrong_type_with_expected", 1, ERROR_TYPE_CELL_OF_STRINGS);
    }
    bool permissionDenied;
    for (const auto& listOfDirectory : listOfDirectories) {
        if (!FileSystemWrapper::Path::is_directory(listOfDirectory, permissionDenied)) {
            if (permissionDenied) {
                raiseError(L"Nelson:help_tools:ERROR_PERMISSION_DENIED", ERROR_PERMISSION_DENIED);
            }
            raiseError(L"Nelson:help_tools:ERROR_DIRECTORY_NOT_EXIST", ERROR_DIRECTORY_NOT_EXIST,
                listOfDirectory);
        }
    }
    ArrayOf argDestinationDir = argIn[1];
    std::wstring dstDirectory = argDestinationDir.getContentAsWideString();
    if (!FileSystemWrapper::Path::is_directory(dstDirectory, permissionDenied)) {
        if (permissionDenied) {
            raiseError(L"Nelson:help_tools:ERROR_PERMISSION_DENIED", ERROR_PERMISSION_DENIED);
        }
        raiseError(L"Nelson:help_tools:ERROR_DIRECTORY_NOT_EXIST", ERROR_DIRECTORY_NOT_EXIST,
            dstDirectory);
    }
    ArrayOf argMainTitle = argIn[2];
    std::wstring mainTitle = argMainTitle.getContentAsWideString();
    ArrayOf argExportFormat = argIn[3];
    std::wstring exportFormat = argExportFormat.getContentAsWideString();
    if ((exportFormat != L"html") && (exportFormat != L"md")) {
        raiseError(L"Nelson:help_tools:ERROR_FORMAT_NOT_SUPPORTED_HTML_OR_MD_EXPECTED",
            ERROR_FORMAT_NOT_SUPPORTED_HTML_OR_MD_EXPECTED);
    }
    DOCUMENT_OUTPUT outputTarget = DOCUMENT_OUTPUT::HTML_WEB;
    if (exportFormat == L"html") {
        outputTarget = DOCUMENT_OUTPUT::HTML_WEB;
    }
    if (exportFormat == L"md") {
        outputTarget = DOCUMENT_OUTPUT::MARKDOWN;
    }
    ArrayOf argOverwrite = argIn[4];
    logical forceOverwrite = argOverwrite.getContentAsLogicalScalar();

    res = xmldocbuild(listOfDirectories, dstDirectory, mainTitle, outputTarget,
        forceOverwrite ? true : false, errorMessage);

    retval = buildReturn(res, errorMessage);
    return retval;
}
//=============================================================================
