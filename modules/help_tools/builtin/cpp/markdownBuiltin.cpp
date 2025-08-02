//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "StringHelpers.hpp"
#include "markdownBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Markdown.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
#include <algorithm>
#include "ParallelSort.hpp"
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
        bool permissionDenied;
        FileSystemWrapper::Path pathIn(filenameIn);
        bool IsDirIn = FileSystemWrapper::Path::is_directory(pathIn, permissionDenied);
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }

        FileSystemWrapper::Path pathOut(filenameOut);
        bool IsDirOut = FileSystemWrapper::Path::is_directory(pathOut, permissionDenied);
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
        if (IsDirIn && IsDirOut) {
            nfs::directory_iterator end_iter;
            wstringVector filesListIn;
            for (nfs::directory_iterator dir_iter(pathIn.native()); dir_iter != end_iter;
                 ++dir_iter) {
                FileSystemWrapper::Path current(dir_iter->path().native());
                if (StringHelpers::iequals(current.extension().generic_wstring(), L".md")) {
                    filesListIn.push_back(current.generic_wstring());
                }
                parallelSort(filesListIn);
            }
            bool bRes = true;
            for (auto& k : filesListIn) {
                FileSystemWrapper::Path st(k);
                FileSystemWrapper::Path out(pathOut);
                out /= st.stem();
                out += L".html";
                bool bLocal = MarkdownFile(k, out.generic_wstring());
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
        if (param1.isCellArrayOfCharacterVectors()) {
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
