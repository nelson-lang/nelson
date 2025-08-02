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
#include "GetCurrentNFilename.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
GetCurrentNFilenameW(Evaluator* eval)
{
    std::string callerName = eval->getCallerFunctionName();
    std::wstring fileName;
    if (callerName == "EvaluateScript") {
        fileName = eval->getCurrentEvaluateFilename();
    } else {
        if (callerName == "evaluator") {
            fileName.clear();
        } else {
            fileName = utf8_to_wstring(callerName);
        }
    }
    if (!fileName.empty()) {
        if (FileSystemWrapper::Path::is_regular_file(fileName)) {
            std::string errorMessage;
            FileSystemWrapper::Path canonicalPath
                = FileSystemWrapper::Path::canonical(fileName, errorMessage);
            fileName = canonicalPath.generic_wstring();
        }
    }
    return fileName;
}
//=============================================================================
NLSCORE_IMPEXP std::string
GetCurrentNFilenameU(Evaluator* eval)
{
    return wstring_to_utf8(GetCurrentNFilenameW(eval));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
