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
#include "i18n.hpp"
#include "XmlDocDocument.hpp"
#include "FileSystemWrapper.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HelpToolsGateway::xmldoccheckerBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 2);
    ArrayOf arg1 = argIn[0];
    if (arg1.isRowVectorCharacterArray()) {
        std::wstring fileOrDirName = arg1.getContentAsWideString();
        FileSystemWrapper::Path pathIn(fileOrDirName);
        bool permissionDenied;
        bool IsFileIn = FileSystemWrapper::Path::is_regular_file(pathIn, permissionDenied);
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
        if (IsFileIn) {
            wstringVector errorRes;
            wstringVector warningRes;
            XmlDocDocument* xmlDoc = new XmlDocDocument(fileOrDirName, L"", L"", true);
            xmlDoc->readFile();
            errorRes = xmlDoc->getError();
            warningRes = xmlDoc->getWarning();
            delete xmlDoc;
            if (nLhs == 0) {
                Interface* io = eval->getInterface();
                io->outputMessage(std::wstring(L"\n"));
                if (errorRes.size() > 1) {
                    io->outputMessage(_W("Errors:") + L"\n");
                } else {
                    io->outputMessage(_W("Error:") + L"\n");
                }
                if (!errorRes.empty()) {
                    for (auto& errorRe : errorRes) {
                        io->errorMessage(std::wstring(L"\t") + errorRe);
                    }
                } else {
                    io->outputMessage(std::wstring(L"\t") + _W("No error.") + L"\n");
                }
                io->outputMessage(std::wstring(L"\n"));
                if (warningRes.size() > 1) {
                    io->outputMessage(_W("Warnings:") + L"\n");
                } else {
                    io->outputMessage(_W("Warning:") + L"\n");
                }
                if (!warningRes.empty()) {
                    for (auto& warningRe : warningRes) {
                        io->warningMessage(std::wstring(L"\t") + warningRe);
                    }
                } else {
                    io->warningMessage(std::wstring(L"\t") + _W("No warning.") + L"\n");
                }
            } else {
                retval << ArrayOf::toCellArrayOfCharacterColumnVectors(errorRes);
                if (nLhs > 1) {
                    retval << ArrayOf::toCellArrayOfCharacterColumnVectors(warningRes);
                }
            }
        } else {
            Error(_W("Wrong value for argument #1: An existing .xml documentation file expected."));
        }
    } else {
        Error(_W("Wrong type for argument #1: .xml documentation file expected."));
    }
    return retval;
}
//=============================================================================
