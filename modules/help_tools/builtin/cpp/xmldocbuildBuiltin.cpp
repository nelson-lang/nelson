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
#include "FileSystemWrapper.hpp"
#include "XmlDocDirectory.hpp"
#include "XmlDocDocument.hpp"
#include "XmlDocListOfDirectories.hpp"
#include "XmlTarget.hpp"
#include "characters_encoding.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// xmldocbuild(source_dirs, destination_dir, main_title, export_format, overwrite)
ArrayOfVector
Nelson::HelpToolsGateway::xmldocbuildBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 5, 5);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf argSourceDirs = argIn[0];
    wstringVector listOfDirectories;
    if (argSourceDirs.isRowVectorCharacterArray()) {
        std::wstring dir = argSourceDirs.getContentAsWideString();
        listOfDirectories.push_back(dir);
    } else if (argSourceDirs.isCell()) {
        listOfDirectories = argSourceDirs.getContentAsWideStringVector(true);
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_CELL_OF_STRINGS_EXPECTED);
    }
    bool permissionDenied;
    for (const auto& listOfDirectorie : listOfDirectories) {
        if (!FileSystemWrapper::Path::is_directory(listOfDirectorie, permissionDenied)) {
            if (permissionDenied) {
                Error(_W("Permission denied."));
            }
            Error(_W("Existing directory expected."));
        }
    }
    ArrayOf argDestinationDir = argIn[1];
    std::wstring dstDirectory = argDestinationDir.getContentAsWideString();
    if (!FileSystemWrapper::Path::is_directory(dstDirectory, permissionDenied)) {
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
        Error(_W("Existing directory expected."));
    }
    ArrayOf argMainTitle = argIn[2];
    std::wstring mainTitle = argMainTitle.getContentAsWideString();
    ArrayOf argExportFormat = argIn[3];
    std::wstring exportFormat = argExportFormat.getContentAsWideString();
    if ((exportFormat != L"help") && (exportFormat != L"html") && (exportFormat != L"md")) {
        Error(_W("format not supported: 'help', 'html' or 'md' expected."));
    }
    DOCUMENT_OUTPUT outputTarget;
    if (exportFormat == L"help") {
        outputTarget = DOCUMENT_OUTPUT::QT_HELP;
    }
    if (exportFormat == L"html") {
        outputTarget = DOCUMENT_OUTPUT::HMTL;
    }
    if (exportFormat == L"md") {
        outputTarget = DOCUMENT_OUTPUT::MARKDOWN;
    }
    ArrayOf argOverwrite = argIn[4];
    logical forceOverwrite = argOverwrite.getContentAsLogicalScalar();
    XmlDocListOfDirectories xmlDirs(
        listOfDirectories, dstDirectory, mainTitle, forceOverwrite ? true : false, outputTarget);
    if (xmlDirs.read()) {
        std::wstring outputModuleName = xmlDirs.getOutputHelpBasename();
        try {
            if (outputTarget == DOCUMENT_OUTPUT::MARKDOWN) {
                xmlDirs.writeAsMarkdown();
            } else {
                xmlDirs.writeAsHtml();
            }
        } catch (Exception& e) {
            Error(e.getMessage());
        }
        retval << ArrayOf::characterArrayConstructor(outputModuleName);
    } else {
        Error(xmlDirs.getLastError());
    }
    return retval;
}
//=============================================================================
