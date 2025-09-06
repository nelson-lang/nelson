//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "xmldoccheckerBuiltin.hpp"
#include "Error.hpp"
#include "Warning.hpp"
#include "i18n.hpp"
#include "FileSystemWrapper.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "XmlDocXsdChecker.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static std::wstring
getDefaultXsd()
{
    std::wstring path = NelsonConfiguration::getInstance()->getNelsonRootDirectory()
        + L"/modules/help_tools/resources/nelson_help.xsd";
    return path;
}
//=============================================================================
ArrayOfVector
Nelson::HelpToolsGateway::xmldoccheckerBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 3);

    std::wstring dtdFilename = L"";

    if (argIn.size() == 2) {
        dtdFilename = argIn[1].getContentAsWideString();
    } else {
        dtdFilename = getDefaultXsd();
    }
    if (!dtdFilename.empty()) {
        FileSystemWrapper::Path pathIn(dtdFilename);
        bool permissionDenied;
        bool IsFileIn = FileSystemWrapper::Path::is_regular_file(pathIn, permissionDenied);
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
        if (!IsFileIn) {
            Error(_W("Wrong value for argument #2: An existing .dtd file expected."));
        }
    }
    std::wstring xmlFilename = argIn[0].getContentAsWideString();
    FileSystemWrapper::Path pathIn(xmlFilename);
    bool permissionDenied;
    bool IsFileIn = FileSystemWrapper::Path::is_regular_file(pathIn, permissionDenied);
    if (permissionDenied) {
        Error(_W("Permission denied."));
    }
    if (!IsFileIn) {
        Error(_W("Wrong value for argument #1: An existing .xml documentation file expected."));
    }
    wstringVector errorMessage;
    wstringVector warningMessage;
    bool res = xmlDocXsdChecker(xmlFilename, dtdFilename, errorMessage, warningMessage);
    switch (nLhs) {
    case 0: {
        if (!res) {
            if (!errorMessage.empty()) {
                std::wstring message;
                for (auto it = errorMessage.begin(); it != errorMessage.end(); ++it) {
                    message = message + std::wstring(L"\t") + *it;
                    if (std::next(it) != errorMessage.end()) {
                        message += L"\n";
                    }
                }
                Error(message);
            }
            if (!warningMessage.empty()) {
                std::wstring message;
                for (auto it = warningMessage.begin(); it != warningMessage.end(); ++it) {
                    message = message + std::wstring(L"\t") + *it;
                    if (std::next(it) != warningMessage.end()) {
                        message += L"\n";
                    }
                }
                Warning(message);
            }
        }
    } break;
    case 1: {
        retval << ArrayOf::logicalConstructor(res);
    } break;
    case 2: {
        retval << ArrayOf::logicalConstructor(res);
        retval << ArrayOf::toCellArrayOfCharacterColumnVectors(errorMessage);
    } break;
    case 3: {
        retval << ArrayOf::logicalConstructor(res);
        retval << ArrayOf::toCellArrayOfCharacterColumnVectors(errorMessage);
        retval << ArrayOf::toCellArrayOfCharacterColumnVectors(warningMessage);
    } break;
    default:
        // Never reached
        break;
    }
    return retval;
}
//=============================================================================
