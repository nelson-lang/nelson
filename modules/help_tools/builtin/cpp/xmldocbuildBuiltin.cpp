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
// Helper function to handle return values based on nLhs, result, and error message
static ArrayOfVector
returnValueOrError(int nLhs, bool res, const std::wstring& errorMessage)
{
    ArrayOfVector retval;
    if (nLhs == 0) {
        if (!errorMessage.empty()) {
            Error(errorMessage);
        }
        retval << ArrayOf::logicalConstructor(res);
    } else {
        retval << ArrayOf::logicalConstructor(res);
        if (nLhs == 2) {
            retval << ArrayOf::characterArrayConstructor(errorMessage);
        }
    }
    return retval;
}
//=============================================================================
// xmldocbuild(source_dirs, destination_dir, main_title, export_format, overwrite)
ArrayOfVector
Nelson::HelpToolsGateway::xmldocbuildBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
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
        return returnValueOrError(
            nLhs, false, ERROR_WRONG_ARGUMENT_1_TYPE_CELL_OF_STRINGS_EXPECTED);
    }
    bool permissionDenied;
    for (const auto& listOfDirectorie : listOfDirectories) {
        if (!FileSystemWrapper::Path::is_directory(listOfDirectorie, permissionDenied)) {
            if (permissionDenied) {
                return returnValueOrError(nLhs, false, _W("Permission denied."));
            }
            return returnValueOrError(nLhs, false, _W("Existing directory expected."));
        }
    }
    ArrayOf argDestinationDir = argIn[1];
    std::wstring dstDirectory = argDestinationDir.getContentAsWideString();
    if (!FileSystemWrapper::Path::is_directory(dstDirectory, permissionDenied)) {
        if (permissionDenied) {
            return returnValueOrError(nLhs, false, _W("Permission denied."));
        }
        return returnValueOrError(nLhs, false, _W("Existing directory expected."));
    }
    ArrayOf argMainTitle = argIn[2];
    std::wstring mainTitle = argMainTitle.getContentAsWideString();
    ArrayOf argExportFormat = argIn[3];
    std::wstring exportFormat = argExportFormat.getContentAsWideString();
    if ((exportFormat != L"html") && (exportFormat != L"md")) {
        Error(_W("format not supported: 'html', or 'md' expected."));
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

    std::wstring errorMessage;
    bool res = xmldocbuild(listOfDirectories, dstDirectory, mainTitle, outputTarget,
        forceOverwrite ? true : false, errorMessage);

    return returnValueOrError(nLhs, res, errorMessage);
}
//=============================================================================
