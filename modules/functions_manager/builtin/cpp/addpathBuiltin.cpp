//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fmt/printf.h>
#include <fmt/format.h>
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include "addpathBuiltin.hpp"
#include "Error.hpp"
#include "PathFuncManager.hpp"
#include "Warning.hpp"
#include "NormalizePath.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::addpathBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
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
            std::wstring option = lastParam.getContentAsWideString();
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
            Error(fmt::sprintf(
                ERROR_WRONG_ARGUMENT_X_TYPE_STRING_EXPECTED, static_cast<int>(lastpos) + 1));
        }
        if (withOption) {
            lastpos = argIn.size() - 2;
            ArrayOf lastParam = argIn[lastpos];
            if (lastParam.isRowVectorCharacterArray()) {
                std::wstring option = lastParam.getContentAsWideString();
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
                Error(fmt::sprintf(
                    ERROR_WRONG_ARGUMENT_X_TYPE_STRING_EXPECTED, static_cast<int>(lastpos) + 1));
            }
        }
    }
    wstringVector params;
    for (size_t k = 0; k < lastpos; k++) {
        ArrayOf param = argIn[k];
        if (param.isRowVectorCharacterArray()) {
            params.push_back(param.getContentAsWideString());
        } else {
            Error(
                fmt::sprintf(ERROR_WRONG_ARGUMENT_X_TYPE_STRING_EXPECTED, static_cast<int>(k) + 1));
        }
    }
    std::wstring previousPaths = PathFuncManager::getInstance()->getPathNameAsString();
    for (const std::wstring& param : params) {
        Nelson::FileSystemWrapper::Path data_dir(param);
        bool bRes = false;
        try {
            bRes = Nelson::FileSystemWrapper::Path::is_directory(data_dir);
        } catch (const boost::filesystem::filesystem_error& e) {
            if (e.code() == boost::system::errc::permission_denied) {
                // ONLY FOR DEBUG
            }
            bRes = false;
        }
        if (bRes) {
            PathFuncManager::getInstance()->addPath(param, beginOption, frozenOption);
        } else {
            Warning(_W("Warning: Not a directory:") + L" " + param + L"\n");
        }
    }
    if (nLhs == 1) {
        retval << ArrayOf::characterArrayConstructor(previousPaths);
    }
    return retval;
}
//=============================================================================
