//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/algorithm/string.hpp>
#include "FileSystemHelpers.hpp"
#include "markdownBuiltin.hpp"
#include "Error.hpp"
#include "IsCellOfStrings.hpp"
#include "Markdown.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HelpToolsGateway::markdownBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 2) {
        std::wstring filenameIn;
        std::wstring filenameOut;
        if (argIn[0].isRowVectorCharacterArray()) {
            filenameIn = argIn[0].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        filenameOut = argIn[1].getContentAsWideString();
        std::filesystem::path pathIn = createFileSystemPath(filenameIn);
        bool IsDirIn = false;
        try {
            IsDirIn = std::filesystem::is_directory(pathIn);
        } catch (const std::filesystem::filesystem_error& e) {
            if (e.code() == std::errc::permission_denied) {
                Error(_W("Permission denied."));
            }
        }
        std::filesystem::path pathOut = createFileSystemPath(filenameOut);
        bool IsDirOut = false;
        try {
            IsDirOut = std::filesystem::is_directory(pathOut);
        } catch (const std::filesystem::filesystem_error& e) {
            if (e.code() == std::errc::permission_denied) {
                Error(_W("Permission denied."));
            }
        }
        if (IsDirIn && IsDirOut) {
            std::filesystem::directory_iterator end_iter;
            wstringVector filesListIn;
            for (std::filesystem::directory_iterator dir_iter(pathIn); dir_iter != end_iter;
                 ++dir_iter) {
                std::filesystem::path current = dir_iter->path();
                if (boost::iequals(
                        convertFileSytemPathToGenericWString(current.extension()), L".md")) {
                    filesListIn.push_back(convertFileSytemPathToGenericWString(current));
                }
            }
            bool bRes = true;
            for (auto& k : filesListIn) {
                std::filesystem::path st(k);
                std::filesystem::path out(pathOut);
                out /= st.stem();
                out += L".html";
                bool bLocal = MarkdownFile(k, convertFileSytemPathToGenericWString(out));
                if (!bLocal) {
                    bRes = bLocal;
                }
            }
            retval << ArrayOf::logicalConstructor(bRes);
        } else {
            bool bRes = MarkdownFile(filenameIn, filenameOut);
            retval << ArrayOf::logicalConstructor(bRes);
        }
    } else {
        // argIn.size() == 1
        ArrayOf param1 = argIn[0];
        std::wstring stringInput;
        if (IsCellOfString(param1)) {
            wstringVector vstr = param1.getContentAsWideStringColumnVector();
            for (auto& k : vstr) {
                stringInput = stringInput + L"\n" + k;
            }
        } else {
            if (param1.isRowVectorCharacterArray()) {
                stringInput = param1.getContentAsWideString();
            } else {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
            }
        }
        std::wstring stringOutput;
        if (MarkdownString(stringInput, stringOutput)) {
            retval << ArrayOf::characterArrayConstructor(stringOutput);
        } else {
            Error(_W("Error markdown generation."));
        }
    }
    return retval;
}
//=============================================================================
