//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "xmldocbuildBuiltin.hpp"
#include "Error.hpp"
#include "IsDirectory.hpp"
#include "XmlDocDirectory.hpp"
#include "XmlDocDocument.hpp"
#include "XmlDocListOfDirectories.hpp"
#include "XmlTarget.hpp"
#include "characters_encoding.hpp"
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
//=============================================================================
using namespace Nelson;
//=============================================================================
// xmldocbuild(source_dirs, destination_dir, main_title, export_format, overwrite)
ArrayOfVector
Nelson::HelpToolsGateway::xmldocbuildBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 5) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
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
    for (size_t k = 0; k < listOfDirectories.size(); k++) {
        if (!IsDirectory(listOfDirectories[k])) {
            Error(_W("Existing directory expected."));
        }
    }
    ArrayOf argDestinationDir = argIn[1];
    std::wstring dstDirectory = argDestinationDir.getContentAsWideString();
    if (!IsDirectory(dstDirectory)) {
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
        retval.push_back(ArrayOf::characterArrayConstructor(outputModuleName));
    } else {
        Error(xmlDirs.getLastError());
    }
    return retval;
}
//=============================================================================
