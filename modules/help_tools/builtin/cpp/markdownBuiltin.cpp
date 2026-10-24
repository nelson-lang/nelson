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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Markdown.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <algorithm>
#include "ParallelSort.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HelpToolsGateway::markdownBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 1);

    MarkdownMode mode = MarkdownMode::SECURE;
    size_t nbInputArguments = argIn.size();

    if (argIn.size() >= 2
        && (argIn.back().isRowVectorCharacterArray() || argIn.back().isScalarStringArray())) {
        std::wstring opt = argIn.back().asWideString();
        if (StringHelpers::iequals(opt, L"secure")) {
            mode = MarkdownMode::SECURE;
            nbInputArguments--;
        } else if (StringHelpers::iequals(opt, L"advanced")) {
            mode = MarkdownMode::ADVANCED;
            nbInputArguments--;
        }
    }

    bool bRes = false;

    if (nbInputArguments == 2
        && (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray())
        && (argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray())) {
        // markdown(md_filename, html_filename [, mode])
        std::wstring filenameIn = argIn[0].asWideString();
        std::wstring filenameOut = argIn[1].asWideString();

        bRes = MarkdownFile(filenameIn, filenameOut, mode);
        retval << ArrayOf::logicalScalar(bRes);
    } else {
        // markdown(md_txt [, mode])
        ArrayOf param1 = argIn[0];
        std::wstring stringInput;
        if (param1.isCellArrayOfCharacterVectors() || param1.isStringArray()) {
            wstringVector vstr = param1.asWideStringColumnVector();
            for (auto& k : vstr) {
                stringInput = stringInput + L"\n" + k;
            }
        } else if (param1.isRowVectorCharacterArray() || param1.isScalarStringArray()) {
            stringInput = param1.asWideString();
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string");
        }

        std::wstring stringOutput;
        bRes = MarkdownString(stringInput, stringOutput, mode);
        if (bRes) {
            retval << ArrayOf::characterArray(stringOutput);
        } else {
            Error(L"Nelson:markdown:generationFailed", L"Markdown generation fails.");
        }
    }
    return retval;
}
//=============================================================================
