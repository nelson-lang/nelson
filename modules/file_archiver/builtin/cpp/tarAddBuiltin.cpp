//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "tarAddBuiltin.hpp"
#include "tarBuiltinHelpers.hpp"
#include "TarArchiveSessions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "zipBuiltinHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::FileArchiverGateway;
//=============================================================================
// names = __tar_add__(id, path, entryName): adds a file or a folder.
ArrayOfVector
Nelson::FileArchiverGateway::tarAddBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 1);
    const std::wstring path = argIn[1].asWideString();
    std::vector<std::string> added;
    try {
        added = TarArchiveSessions::addPath(archiveId(argIn[0]), path, argIn[2].asUtf8String());
    } catch (const ZipArchiveError& error) {
        if (error.code() == ZipErrorCode::OpenFailed || error.code() == ZipErrorCode::ReadFailed) {
            Error(L"Nelson:io:tar:cannotReadFile", L"Cannot read file: %s", path);
        }
        raiseTarError(error);
    }
    wstringVector names;
    for (const std::string& name : added) {
        names.push_back(utf8_to_wstring(name));
    }
    ArrayOfVector retval;
    retval << stringColumn(names);
    return retval;
}
//=============================================================================
