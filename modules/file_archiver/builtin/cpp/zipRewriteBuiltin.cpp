//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "zipRewriteBuiltin.hpp"
#include "zipBuiltinHelpers.hpp"
#include "ZipArchiveSessions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// __zip_rewrite__(filename, removed, oldNames, newNames)
ArrayOfVector
Nelson::FileArchiverGateway::zipRewriteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 4, 4);
    nargoutcheck(nLhs, 0, 0);
    const std::wstring filename = argIn[0].asWideString();
    const stringVector removed
        = argIn[1].isEmpty() ? stringVector() : argIn[1].asUtf8StringVector();
    const stringVector oldNames
        = argIn[2].isEmpty() ? stringVector() : argIn[2].asUtf8StringVector();
    const stringVector newNames
        = argIn[3].isEmpty() ? stringVector() : argIn[3].asUtf8StringVector();
    std::vector<std::pair<std::string, std::string>> renamed;
    for (size_t k = 0; k < oldNames.size() && k < newNames.size(); ++k) {
        renamed.emplace_back(oldNames[k], newNames[k]);
    }
    try {
        ZipArchiveSessions::rewrite(filename, removed, renamed);
    } catch (const ZipArchiveError& error) {
        if (error.code() == ZipErrorCode::OpenFailed) {
            Error(L"Nelson:io:zip:cannotOpen", L"Cannot open ZIP archive: %s", filename);
        }
        raiseZipError(error);
    }
    return { };
}
//=============================================================================
