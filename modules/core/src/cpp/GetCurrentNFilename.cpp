//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem.hpp>
#include "GetCurrentNFilename.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isFile(const std::wstring& filename)
{
    boost::filesystem::path data_dir(filename);
    bool bRes = false;
    try {
        bRes = boost::filesystem::exists(data_dir) && !boost::filesystem::is_directory(data_dir);
    } catch (const boost::filesystem::filesystem_error& e) {
        if (e.code() == boost::system::errc::permission_denied) {
            // ONLY FOR DEBUG
        }
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
            boost::filesystem::path canonicalPath;
            try {
                canonicalPath
                    = boost::filesystem::canonical(fileName, boost::filesystem::current_path());
                fileName = canonicalPath.generic_wstring();
            } catch (const boost::filesystem::filesystem_error&) {
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
