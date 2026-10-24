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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NelsonException.hpp"
#include "XmlTarget.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FileSystemWrapper.hpp"
#include "XmlDocBuild.hpp"
#include "Warning.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
returnValueOrError(int nLhs, bool res, const std::wstring& errorMessage)
{
    ArrayOfVector retval;
    if (nLhs == 0) {
        if (!errorMessage.empty()) {
            Error(L"Nelson:xmldocbuild:buildFailed", errorMessage);
        }
        retval << ArrayOf::logicalScalar(res);
    } else {
        retval << ArrayOf::logicalScalar(res);
        if (nLhs >= 2) {
            retval << ArrayOf::characterArray(errorMessage);
        }
    }
    return retval;
}
//=============================================================================
// xmldocbuild(source_dirs, destination_dir, main_title, export_format, overwrite)
ArrayOfVector
Nelson::HelpToolsGateway::xmldocbuildBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 5, 6);
    nargoutcheck(nLhs, 0, 3);
    ArrayOf argSourceDirs = argIn[0];
    wstringVector listOfDirectories;
    if (argSourceDirs.isRowVectorCharacterArray()) {
        std::wstring dir = argSourceDirs.asWideString();
        listOfDirectories.push_back(dir);
    } else if (argSourceDirs.isCell()) {
        listOfDirectories = argSourceDirs.asWideStringVector(true);
    } else {
        return returnValueOrError(nLhs, false,
            Nelson::formatI18nMessage(
                L"Wrong type for argument #%d: %s expected.", (1), L"cell of strings"));
    }
    bool permissionDenied;
    for (const auto& listOfDirectory : listOfDirectories) {
        if (!FileSystemWrapper::Path::is_directory(listOfDirectory, permissionDenied)) {
            if (permissionDenied) {
                return returnValueOrError(nLhs, false,
                    Nelson::resolveMessageTemplate(
                        L"Nelson:help_tools:permissionDenied", L"Permission denied."));
            }
            return returnValueOrError(nLhs, false, _W("Existing directory expected."));
        }
    }
    ArrayOf argDestinationDir = argIn[1];
    std::wstring dstDirectory = argDestinationDir.asWideString();
    if (!FileSystemWrapper::Path::is_directory(dstDirectory, permissionDenied)) {
        if (permissionDenied) {
            return returnValueOrError(nLhs, false,
                Nelson::resolveMessageTemplate(
                    L"Nelson:help_tools:permissionDenied", L"Permission denied."));
        }
        return returnValueOrError(nLhs, false, _W("Existing directory expected."));
    }
    ArrayOf argMainTitle = argIn[2];
    std::wstring mainTitle = argMainTitle.asWideString();
    ArrayOf argExportFormat = argIn[3];
    std::wstring exportFormat = argExportFormat.asWideString();
    DOCUMENT_OUTPUT outputTarget = DOCUMENT_OUTPUT::HTML_WEB;
    if (exportFormat == L"md") {
        outputTarget = DOCUMENT_OUTPUT::MARKDOWN;
    } else if (exportFormat == L"typ") {
        outputTarget = DOCUMENT_OUTPUT::TYPST;
    } else if (exportFormat != L"html") {
        Error(L"Nelson:xmldocbuild:invalidFormat",
            L"format not supported: 'html', 'md' or 'typ' expected.");
    }
    ArrayOf argOverwrite = argIn[4];
    logical forceOverwrite = argOverwrite.asLogicalScalar();

    // Optional: XML help roots indexed to resolve <link linkend="${module}name">.
    wstringVector indexDirectories;
    if (argIn.size() == 6) {
        ArrayOf argIndexDirs = argIn[5];
        if (argIndexDirs.isEmpty()) {
            // no additional index directory
        } else if (argIndexDirs.isRowVectorCharacterArray()) {
            indexDirectories.push_back(argIndexDirs.asWideString());
        } else if (argIndexDirs.isCell()) {
            indexDirectories = argIndexDirs.asWideStringVector(true);
        } else {
            return returnValueOrError(nLhs, false,
                Nelson::formatI18nMessage(
                    L"Wrong type for argument #%d: %s expected.", (6), L"cell of strings"));
        }
        for (const auto& indexDirectory : indexDirectories) {
            if (!FileSystemWrapper::Path::is_directory(indexDirectory, permissionDenied)) {
                if (permissionDenied) {
                    return returnValueOrError(nLhs, false,
                        Nelson::resolveMessageTemplate(
                            L"Nelson:help_tools:permissionDenied", L"Permission denied."));
                }
                return returnValueOrError(nLhs, false, _W("Existing directory expected."));
            }
        }
    }

    std::wstring errorMessage;
    wstringVector warnings;
    bool res = xmldocbuild(listOfDirectories, dstDirectory, mainTitle, outputTarget,
        forceOverwrite ? true : false, errorMessage, indexDirectories, warnings);

    if (nLhs < 3) {
        for (const auto& warning : warnings) {
            Warning(warning);
        }
    }
    ArrayOfVector retval = returnValueOrError(nLhs, res, errorMessage);
    if (nLhs >= 3) {
        retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(warnings);
    }
    return retval;
}
//=============================================================================
