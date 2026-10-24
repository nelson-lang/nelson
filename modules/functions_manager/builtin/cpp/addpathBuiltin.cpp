//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include "ErrorID.hpp"
#include <fmt/xchar.h>
#include "FileSystemWrapper.hpp"
#include "addpathBuiltin.hpp"
#include "Error.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "Warning.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static std::wstring
getPathSeparator()
{
#ifdef _MSC_VER
    return L";";
#else
    return L":";
#endif
}
//=============================================================================
static void
appendPathArguments(wstringVector& params, const std::wstring& value)
{
    const std::wstring separator = getPathSeparator();
    size_t begin = 0;
    while (begin <= value.length()) {
        size_t end = value.find(separator, begin);
        std::wstring item = value.substr(begin, end == std::wstring::npos ? end : end - begin);
        if (!item.empty()) {
            params.push_back(item);
        }
        if (end == std::wstring::npos) {
            break;
        }
        begin = end + separator.length();
    }
}
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::addpathBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1);
    bool beginOption = true;
    bool frozenOption = false;
    bool withOption = true;
    size_t lastpos = argIn.size();
    if (argIn.size() > 1) {
        lastpos = argIn.size() - 1;
        ArrayOf lastParam = argIn[lastpos];
        if (lastParam.isRowVectorCharacterArray()) {
            std::wstring option = lastParam.asWideString();
            if ((option == L"-begin") || (option == L"-end") || option == L"-frozen") {
                withOption = true;
                if (option == L"-begin") {
                    beginOption = true;
                }
                if (option == L"-end") {
                    beginOption = false;
                }
                if (option == L"-frozen") {
                    frozenOption = true;
                }
            } else {
                withOption = false;
                lastpos = argIn.size();
            }
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.",
                (static_cast<int>(lastpos) + 1), L"string");
        }
        if (withOption) {
            lastpos = argIn.size() - 2;
            ArrayOf lastParam = argIn[lastpos];
            if (lastParam.isRowVectorCharacterArray()) {
                std::wstring option = lastParam.asWideString();
                if ((option == L"-begin") || (option == L"-end") || option == L"-frozen") {
                    if (option == L"-begin") {
                        beginOption = true;
                    }
                    if (option == L"-end") {
                        beginOption = false;
                    }
                    if (option == L"-frozen") {
                        frozenOption = true;
                    }
                } else {
                    lastpos = argIn.size() - 1;
                }
            } else {
                Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.",
                    (static_cast<int>(lastpos) + 1), L"string");
            }
        }
    }
    wstringVector params;
    for (size_t k = 0; k < lastpos; k++) {
        ArrayOf param = argIn[k];
        if (param.isRowVectorCharacterArray()) {
            appendPathArguments(params, param.asWideString());
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.",
                (static_cast<int>(k) + 1), L"string");
        }
    }
    std::wstring previousPaths = PathFunctionIndexerManager::getInstance()->getPathNameAsString();
    for (const std::wstring& param : params) {
        if (FileSystemWrapper::Path::is_directory(param)) {
            PathFunctionIndexerManager::getInstance()->addPath(param, beginOption, frozenOption);
        } else {
            RaiseWarning(L"Nelson:addpath:notADirectory",
                resolveMessageTemplate(
                    L"Nelson:functions_manager:warningDirectory", L"Warning: Not a directory:")
                    + L" " + param + L"\n");
        }
    }
    if (nLhs == 1) {
        retval << ArrayOf::characterArray(previousPaths);
    }
    return retval;
}
//=============================================================================
