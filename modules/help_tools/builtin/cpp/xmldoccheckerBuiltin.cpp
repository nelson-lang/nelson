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
#include "xmldoccheckerBuiltin.hpp"
#include "Error.hpp"
#include "ToCellString.hpp"
#include "XmlDocDocument.hpp"
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HelpToolsGateway::xmldoccheckerBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 2) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf arg1 = argIn[0];
    if (arg1.isRowVectorCharacterArray()) {
        std::wstring fileOrDirName = arg1.getContentAsWideString();
        boost::filesystem::path pathIn(fileOrDirName);
        bool IsFileIn = false;
        try {
            IsFileIn
                = boost::filesystem::exists(pathIn) && !boost::filesystem::is_directory(pathIn);
        } catch (const boost::filesystem::filesystem_error& e) {
            if (e.code() == boost::system::errc::permission_denied) {
                Error(_W("Permission denied."));
            }
            IsFileIn = false;
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
                    for (size_t k = 0; k < errorRes.size(); k++) {
                        io->errorMessage(std::wstring(L"\t") + errorRes[k]);
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
                    for (size_t k = 0; k < warningRes.size(); k++) {
                        io->warningMessage(std::wstring(L"\t") + warningRes[k]);
                    }
                } else {
                    io->warningMessage(std::wstring(L"\t") + _W("No warning.") + L"\n");
                }
            } else {
                retval.push_back(ToCellStringAsColumn(errorRes));
                if (nLhs > 1) {
                    retval.push_back(ToCellStringAsColumn(warningRes));
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
