//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <filesystem>
#include "GetCurrentNFilename.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isFile(const std::wstring& filename)
{
    std::filesystem::path data_dir(filename);
    bool bRes = false;
    try {
        bRes = std::filesystem::exists(data_dir) && !std::filesystem::is_directory(data_dir);
    } catch (const std::filesystem::filesystem_error&) {
        bRes = false;
    }
    return bRes;
}
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
        if (isFile(fileName)) {
            std::filesystem::path canonicalPath;
            try {
                canonicalPath = std::filesystem::canonical(fileName);
                fileName = canonicalPath.generic_wstring();
            } catch (const std::filesystem::filesystem_error&) {
            }
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
