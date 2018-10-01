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
#include "markdownBuiltin.hpp"
#include "Error.hpp"
#include "IsCellOfStrings.hpp"
#include "Markdown.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HelpToolsGateway::markdownBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() > 2 || argIn.size() == 0) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 2) {
        std::wstring filenameIn = L"";
        std::wstring filenameOut = L"";
        if (argIn[0].isRowVectorCharacterArray()) {
            filenameIn = argIn[0].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        filenameOut = argIn[1].getContentAsWideString();
        boost::filesystem::path pathIn(filenameIn);
        bool IsDirIn = false;
        try {
            IsDirIn = boost::filesystem::is_directory(pathIn);
        } catch (const boost::filesystem::filesystem_error& e) {
            if (e.code() == boost::system::errc::permission_denied) {
                Error(_W("Permission denied."));
            }
        }
        boost::filesystem::path pathOut(filenameOut);
        bool IsDirOut = false;
        try {
            IsDirOut = boost::filesystem::is_directory(pathOut);
        } catch (const boost::filesystem::filesystem_error& e) {
            if (e.code() == boost::system::errc::permission_denied) {
                Error(_W("Permission denied."));
            }
        }
        if (IsDirIn && IsDirOut) {
            boost::filesystem::directory_iterator end_iter;
            wstringVector filesListIn;
            for (boost::filesystem::directory_iterator dir_iter(pathIn); dir_iter != end_iter;
                 ++dir_iter) {
                boost::filesystem::path current = dir_iter->path();
                if (boost::iequals(current.extension().generic_wstring(), ".md")) {
                    filesListIn.push_back(current.generic_wstring());
                }
            }
            bool bRes = true;
            for (size_t k = 0; k < filesListIn.size(); k++) {
                boost::filesystem::path st(filesListIn[k]);
                boost::filesystem::path out(pathOut);
                out /= st.stem();
                out += L".html";
                bool bLocal = MarkdownFile(filesListIn[k], out.generic_wstring());
                if (!bLocal) {
                    bRes = bLocal;
                }
            }
            retval.push_back(ArrayOf::logicalConstructor(bRes));
        } else {
            bool bRes = MarkdownFile(filenameIn, filenameOut);
            retval.push_back(ArrayOf::logicalConstructor(bRes));
        }
    } else {
        // argIn.size() == 1
        ArrayOf param1 = argIn[0];
        std::wstring stringInput;
        if (IsCellOfString(param1)) {
            wstringVector vstr = param1.getContentAsWideStringColumnVector();
            for (size_t k = 0; k < vstr.size(); k++) {
                stringInput = stringInput + L"\n" + vstr[k];
            }
        } else {
            if (param1.isRowVectorCharacterArray()) {
                stringInput = param1.getContentAsWideString();
            } else {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
            }
        }
        std::wstring stringOutput = L"";
        if (MarkdownString(stringInput, stringOutput)) {
            retval.push_back(ArrayOf::characterArrayConstructor(stringOutput));
        } else {
            Error(_W("Error markdown generation."));
        }
    }
    return retval;
}
//=============================================================================
