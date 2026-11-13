//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "zipAddBuiltin.hpp"
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
// names = __zip_add__(id, path, entryName, compression, level)
ArrayOfVector
Nelson::FileArchiverGateway::zipAddBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 5, 5);
    nargoutcheck(nLhs, 0, 1);
    const std::wstring path = argIn[1].asWideString();
    const std::string entryName = argIn[2].asUtf8String();
    const ZipWriteOptions options = writeOptions(argIn[3], argIn[4]);
    std::vector<std::string> added;
    try {
        added = ZipArchiveSessions::addPath(archiveId(argIn[0]), path, entryName, options);
    } catch (const ZipArchiveError& error) {
        if (error.code() == ZipErrorCode::OpenFailed) {
            Error(L"Nelson:io:zip:cannotReadFile", L"Cannot read file: %s", path);
        }
        raiseZipError(error);
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
